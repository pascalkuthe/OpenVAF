/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use super::error::Error::DerivativeNotDefined;
use super::error::UndefinedDerivative;
use super::lints::RoundingDerivativeNotFullyDefined;
use super::{operand_to_derivative, AutoDiff};
use crate::osdi_types::ConstVal::Scalar;
use crate::osdi_types::SimpleConstVal::Real;
use crate::BinOp::{Divide, Minus, Multiply, Plus};
use crate::ComparisonOp::Equal;
use crate::OperandData::Constant;
use crate::{
    fold_rvalue, BinOp, COperand, CallArg, CallTypeDerivative, CfgFunctions, Derivative, Local,
    Operand, OperandData, RValue, RValueFold, StmntKind, SyntaxCtx, Type,
};
use enum_map::{Enum, EnumMap};
use openvaf_data_structures::index_vec::IndexSlice;
use openvaf_diagnostics::lints::Linter;
use openvaf_ir::Math1::{Cos, CosH, Ln, Sin, SinH, Sqrt};
use openvaf_ir::Math2::Pow;
use openvaf_ir::UnaryOperator::ArithmeticNegate;
use openvaf_ir::{Math1, Spanned, UnaryOperator, Unknown};
use openvaf_session::sourcemap::span::DUMMY_SP;
use openvaf_session::sourcemap::Span;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Enum)]
pub enum OuterDerivativeCacheSlot {
    Lhs,
    Rhs,
}
impl OuterDerivativeCacheSlot {
    pub const SINGLE: Self = Self::Lhs;
}

impl<'lt, C: CfgFunctions, MC: CfgFunctions> AutoDiff<'lt, C, MC> {
    pub(crate) fn rvalue_derivative(
        &mut self,
        lhs: Local,
        rhs: &RValue<C>,
        unknown: Unknown,
        origin: SyntaxCtx,
        cache: &mut EnumMap<OuterDerivativeCacheSlot, Option<COperand<C>>>,
    ) -> RValue<C> {
        let ty = self.cfg.locals[lhs].ty;
        let mut fold = RValueAutoDiff {
            original_local: lhs,
            origin,
            ad: self,
            unknown,
            outer_derivative_cache: cache,
        };
        let res = fold_rvalue(&mut fold, rhs, ty);
        res.unwrap_or_else(|| RValue::Use(Operand::new(Constant(0.0.into()), DUMMY_SP)))
    }
}

enum OneAndSquareKind {
    /// 1 + f^2
    Positive,
    /// 1 - f^2
    OneMinusSquared,
    /// f^2 - 1
    SquaredMinusOne,
}

pub struct RValueAutoDiff<'lt, 'adlt, C: CfgFunctions, MC: CfgFunctions> {
    /// Local that the RValue this is being derived will be saved to
    pub original_local: Local,
    origin: SyntaxCtx,
    pub ad: &'lt mut AutoDiff<'adlt, C, MC>,
    pub unknown: Unknown,
    outer_derivative_cache: &'lt mut EnumMap<OuterDerivativeCacheSlot, Option<COperand<C>>>,
}

impl<'lt, 'adlt, C: CfgFunctions, MC: CfgFunctions> RValueFold<C>
    for RValueAutoDiff<'lt, 'adlt, C, MC>
{
    type T = Option<RValue<C>>;

    fn fold_cmplx_arith_negate(&mut self, _op: Span, _arg: &COperand<C>) -> Self::T {
        unimplemented!("Complex derivatives")
    }

    #[inline]
    fn fold_real_arith_negate(&mut self, op: Span, arg: &COperand<C>) -> Self::T {
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);
        Some(RValue::UnaryOperation(
            Spanned::new(UnaryOperator::ArithmeticNegate, op),
            arg_derivative,
        ))
    }

    #[inline]
    fn fold_bit_negate(&mut self, op: Span, _arg: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    #[inline]
    fn fold_int_arith_negate(&mut self, op: Span, arg: &COperand<C>) -> Self::T {
        self.fold_real_arith_negate(op, &arg)
    }

    #[inline]
    fn fold_logic_negate(&mut self, op: Span, _arg: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::LogicOp, op));
        None
    }

    fn fold_cmplx_add(&mut self, _op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        unimplemented!("Complex derivatives")
    }

    fn fold_cmplx_sub(&mut self, _op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        unimplemented!("Complex derivatives")
    }

    fn fold_cmplx_mul(&mut self, _op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        unimplemented!("Complex derivatives")
    }

    fn fold_cmplx_div(&mut self, _op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        unimplemented!("Complex derivatives")
    }

    #[inline]
    fn fold_real_add(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs_derivative = self.derivative(lhs);
        let rhs_derivative = self.derivative(rhs);

        Self::derivative_sum(
            Spanned::new(lhs_derivative, lhs.span),
            Spanned::new(rhs_derivative, rhs.span),
            op,
        )
    }

    #[inline]
    fn fold_real_sub(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs_derivative = self.derivative(lhs);
        let rhs_derivative = self.derivative(rhs);

        Self::derivative_sub(
            Spanned::new(lhs_derivative, lhs.span),
            Spanned::new(rhs_derivative, rhs.span),
            op,
        )
    }

    #[inline]
    fn fold_real_mul(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs_derivative = self.derivative(lhs);
        let rhs_derivative = self.derivative(rhs);

        self.derivative_mul(lhs, rhs, lhs_derivative, rhs_derivative, op)
    }

    #[inline]
    fn fold_real_div(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs_derivative = self.derivative(lhs);
        let rhs_derivative = self.derivative(rhs);

        self.derivative_div(lhs, rhs, lhs_derivative, rhs_derivative, op)
    }

    #[inline]
    fn fold_real_rem(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Modulus, op));
        None
    }

    #[inline]
    fn fold_int_add(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        // plus / minsu don't use the original value no need for casts
        self.fold_real_add(op, &lhs, &rhs)
    }

    #[inline]
    fn fold_int_sub(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        // plus / minsu don't use the original value no need for casts
        self.fold_real_sub(op, &lhs, &rhs)
    }

    #[inline]
    fn fold_int_mul(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs_derivative = self.derivative(lhs);
        let rhs_derivative = self.derivative(rhs);

        let new_lhs;
        let new_rhs;

        let rhs = if lhs_derivative != Derivative::Zero {
            new_rhs = self.gen_temporary(RValue::Cast(rhs.clone()), op);
            &new_rhs
        } else {
            rhs
        };

        let lhs = if rhs_derivative != Derivative::Zero {
            new_lhs = self.gen_temporary(RValue::Cast(lhs.clone()), op);
            &new_lhs
        } else {
            lhs
        };

        self.derivative_mul(lhs, rhs, lhs_derivative, rhs_derivative, op)
    }

    #[inline]
    fn fold_int_div(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        let lhs_derivative = self.derivative(&lhs);
        let rhs_derivative = self.derivative(&rhs);

        let new_lhs;
        let new_rhs;

        let (lhs, rhs) = if lhs_derivative != Derivative::Zero || rhs_derivative != Derivative::Zero
        {
            new_rhs = self.gen_temporary(RValue::Cast(rhs.clone()), op);

            let lhs = if rhs_derivative != Derivative::Zero {
                new_lhs = self.gen_temporary(RValue::Cast(lhs.clone()), op);
                &new_lhs
            } else {
                lhs
            };

            (lhs, &new_rhs)
        } else {
            (lhs, rhs)
        };

        self.derivative_div(lhs, rhs, lhs_derivative, rhs_derivative, op)
    }

    #[inline]
    fn fold_int_rem(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Modulus, op));
        None
    }

    #[inline]
    fn fold_shiftl(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        // f*2^g -> f'*2**g + f*ln(2)*g'*2**g
        //lhs'*2^rhs
        let dlhs = self.chain_rule(
            &rhs,
            |fold| {
                let two = Self::gen_constant(2.0, op);
                let rhs = fold.gen_temporary(RValue::Cast(rhs.clone()), op);
                RValue::Math2(Spanned::new(Pow, op), two, rhs)
            },
            op,
            OuterDerivativeCacheSlot::Lhs,
        );

        // rhs' * (ln(2)*lhs*2**g)
        let drhs = self.chain_rule(
            &rhs,
            |fold| {
                let lhs = fold.gen_temporary(RValue::Cast(lhs.clone()), op);
                let ln2 = Self::gen_constant(std::f64::consts::LN_2, op);
                let product = RValue::BinaryOperation(Spanned::new(Multiply, op), lhs, ln2);
                let product = fold.gen_temporary(product, op);
                let original = fold.original_operand(op);
                let original = fold.gen_temporary(RValue::Cast(original), op);
                RValue::BinaryOperation(Spanned::new(Multiply, op), product, original)
            },
            op,
            OuterDerivativeCacheSlot::Rhs,
        );

        match (dlhs, drhs) {
            (None, None) => None,
            (Some(val), None) | (None, Some(val)) => Some(val),
            (Some(dlhs), Some(drhs)) => {
                let dlhs = self.gen_temporary(dlhs, op);
                let drhs = self.gen_temporary(drhs, op);
                Some(RValue::BinaryOperation(
                    Spanned::new(Multiply, op),
                    dlhs,
                    drhs,
                ))
            }
        }
    }

    #[inline]
    fn fold_shiftr(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        // f*2^-g -> f'*2**-g + -ln(2)*g'*f*2**-g
        //lhs'*2^rhs
        let dlhs = self.chain_rule(
            &rhs,
            |fold| {
                let two = Self::gen_constant(2.0, op);
                let rhs = fold.gen_temporary(
                    RValue::UnaryOperation(Spanned::new(ArithmeticNegate, op), rhs.clone()),
                    op,
                );
                let rhs = fold.gen_temporary(RValue::Cast(rhs), op);
                RValue::Math2(Spanned::new(Pow, op), two, rhs)
            },
            op,
            OuterDerivativeCacheSlot::Lhs,
        );

        // rhs' * (-ln(2)*lhs>>rhs)
        let drhs = self.chain_rule(
            &rhs,
            |fold| {
                let lhs = fold.gen_temporary(RValue::Cast(lhs.clone()), op);
                let ln2 = Self::gen_constant(-std::f64::consts::LN_2, op);
                let product = RValue::BinaryOperation(Spanned::new(Multiply, op), lhs, ln2);
                let product = fold.gen_temporary(product, op);
                let original = fold.original_operand(op);
                let original = fold.gen_temporary(RValue::Cast(original), op);
                RValue::BinaryOperation(Spanned::new(Multiply, op), product, original)
            },
            op,
            OuterDerivativeCacheSlot::Rhs,
        );

        match (dlhs, drhs) {
            (None, None) => None,
            (Some(val), None) | (None, Some(val)) => Some(val),
            (Some(dlhs), Some(drhs)) => {
                let dlhs = self.gen_temporary(dlhs, op);
                let drhs = self.gen_temporary(drhs, op);
                Some(RValue::BinaryOperation(
                    Spanned::new(Multiply, op),
                    dlhs,
                    drhs,
                ))
            }
        }
    }

    #[inline]
    fn fold_lt_real(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_le_real(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_gt_real(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_ge_real(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_lt_int(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_le_int(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_gt_int(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_ge_int(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_eq(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>, _ty: Type) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_ne(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>, _ty: Type) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::Comparison, op));
        None
    }

    #[inline]
    fn fold_xor(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    #[inline]
    fn fold_nxor(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    #[inline]
    fn fold_and(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    #[inline]
    fn fold_or(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    fn fold_bool_xor(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    fn fold_bool_nxor(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    fn fold_bool_and(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    fn fold_bool_or(&mut self, op: Span, _lhs: &COperand<C>, _rhs: &COperand<C>) -> Self::T {
        self.ad
            .errors
            .add(DerivativeNotDefined(UndefinedDerivative::BitWiseOp, op));
        None
    }

    fn fold_exp(&mut self, span: Span, arg: &COperand<C>, _limit: bool) -> Self::T {
        // TODO should limexp have a different derivative?
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);

        let original = self.original_operand(span);

        Some(RValue::BinaryOperation(
            Spanned::new(Multiply, span),
            arg_derivative,
            original,
        ))
    }

    fn fold_ln(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);

        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            arg_derivative,
            arg.clone(),
        ))
    }

    fn fold_log(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        // f' * log10_e/f
        self.chain_rule(
            arg,
            |_| {
                // log10_e / f

                let top = Self::gen_constant(std::f64::consts::LOG10_E, span);

                RValue::BinaryOperation(Spanned::new(Divide, span), top, arg.clone())
            },
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_clog2(&mut self, span: Span, _arg: &COperand<C>) -> Self::T {
        Linter::dispatch_late(
            Box::new(RoundingDerivativeNotFullyDefined(span)),
            self.origin,
        );
        None
    }

    fn fold_sqrt(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        // f' * (1/2 / sqrt(x))
        self.chain_rule(
            arg,
            |fold| {
                let top = Self::gen_constant(0.5, span);
                let den = fold.original_operand(span);

                RValue::BinaryOperation(Spanned::new(Divide, span), top, den)
            },
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_cmplx_abs(&mut self, _span: Span, _arg: &COperand<C>) -> Self::T {
        unimplemented!("Complex derivatives")
    }

    fn fold_real_abs(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        self.derivative_abs(span, arg, Type::REAL)
    }

    fn fold_int_abs(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        self.derivative_abs(span, arg, Type::INT)
    }

    fn fold_ceil(&mut self, span: Span, _arg: &COperand<C>) -> Self::T {
        Linter::dispatch_late(
            Box::new(RoundingDerivativeNotFullyDefined(span)),
            self.origin,
        );
        None
    }

    fn fold_floor(&mut self, span: Span, _arg: &COperand<C>) -> Self::T {
        Linter::dispatch_late(
            Box::new(RoundingDerivativeNotFullyDefined(span)),
            self.origin,
        );
        None
    }

    fn fold_sin(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        self.chain_rule(
            arg,
            |_| RValue::Math1(Spanned::new(Cos, span), arg.clone()),
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_cos(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        self.chain_rule(
            arg,
            |fold| {
                let sin = fold.gen_single_arg_math(Sin, arg.clone(), span);
                RValue::UnaryOperation(Spanned::new(ArithmeticNegate, span), sin)
            },
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_tan(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        // f'*(1+tan^2(f))
        self.chain_rule(
            arg,
            |fold| {
                // 1+tan^2(f)
                let original = fold.original_operand(span);

                fold.gen_one_and_square(OneAndSquareKind::Positive, original, span)
            },
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_sinh(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        self.chain_rule(
            arg,
            |_| RValue::Math1(Spanned::new(CosH, span), arg.clone()),
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_cosh(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        self.chain_rule(
            arg,
            |_| RValue::Math1(Spanned::new(SinH, span), arg.clone()),
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_tanh(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        // f'*(1-tanh^2(f))
        self.chain_rule(
            arg,
            |fold| {
                // 1-tanh^2(f)
                let original = fold.original_operand(span);

                fold.gen_one_and_square(OneAndSquareKind::OneMinusSquared, original, span)
            },
            span,
            OuterDerivativeCacheSlot::SINGLE,
        )
    }

    fn fold_asin(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);

        // 1 - f²
        let sqrt_arg =
            self.gen_one_and_square(OneAndSquareKind::OneMinusSquared, arg.clone(), span);

        let sqrt_arg = self.gen_temporary(sqrt_arg, span);

        // sqrt(1-f²)
        let den = self.gen_single_arg_math(Sqrt, sqrt_arg, span);

        // f'/sqrt(1-f²)
        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            arg_derivative,
            den,
        ))
    }

    fn fold_acos(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        // arccos(f)' = - (arsin(f))'
        let dasin = self.fold_asin(span, arg)?;
        let dasin = self.gen_temporary(dasin, span);

        Some(RValue::UnaryOperation(
            Spanned::new(ArithmeticNegate, span),
            dasin,
        ))
    }

    fn fold_atan(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);

        // 1 + f²
        let den = self.gen_one_and_square(OneAndSquareKind::Positive, arg.clone(), span);
        let den = self.gen_temporary(den, span);

        // f'/(1 + f²)
        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            arg_derivative,
            den,
        ))
    }

    fn fold_asinh(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);
        // 1 + f²
        let sqrt_arg = self.gen_one_and_square(OneAndSquareKind::Positive, arg.clone(), span);
        let sqrt_arg = self.gen_temporary(sqrt_arg, span);

        // sqrt(1 + f²)
        let den = self.gen_single_arg_math(Sqrt, sqrt_arg, span);

        // f'/sqrt(1 + f²)
        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            arg_derivative,
            den,
        ))
    }

    fn fold_acosh(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);
        //  f² - 1
        let sqrt_arg =
            self.gen_one_and_square(OneAndSquareKind::SquaredMinusOne, arg.clone(), span);
        let sqrt_arg = self.gen_temporary(sqrt_arg, span);

        // sqrt(f² - 1)
        let den = self.gen_single_arg_math(Sqrt, sqrt_arg, span);

        // f'/sqrt(f² - 1)
        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            arg_derivative,
            den,
        ))
    }

    fn fold_atanh(&mut self, span: Span, arg: &COperand<C>) -> Self::T {
        let arg_derivative = self.derivative(arg).into_option()?;
        let arg_derivative = Operand::new(arg_derivative, arg.span);

        // 1-f²
        let den = self.gen_one_and_square(OneAndSquareKind::OneMinusSquared, arg.clone(), span);
        let den = self.gen_temporary(den, span);

        // f'/(1-f²)
        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            arg_derivative,
            den,
        ))
    }

    fn fold_pow(&mut self, span: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T {
        // TODO heuristic whether to cache this

        // rhs/lhs * lhs'
        let sum1 = self.chain_rule(
            &lhs,
            |_| RValue::BinaryOperation(Spanned::new(Divide, span), rhs.clone(), lhs.clone()),
            span,
            OuterDerivativeCacheSlot::Lhs,
        );

        //ln (lhs) * rhs'

        let sum2 = self.chain_rule(
            &rhs,
            |_| RValue::Math1(Spanned::new(Ln, span), lhs.clone()),
            span,
            OuterDerivativeCacheSlot::Rhs,
        );

        // f'/f*g + ln(f)*g'
        let sum = match (sum1, sum2) {
            (None, None) => return None,
            (Some(val), None) | (None, Some(val)) => val,
            (Some(lhs), Some(rhs)) => {
                let lhs = self.gen_temporary(lhs, span);
                let rhs = self.gen_temporary(rhs, span);
                RValue::BinaryOperation(Spanned::new(Plus, span), lhs, rhs)
            }
        };
        let sum = self.gen_temporary(sum, span);

        let original = self.original_operand(span);

        // (f**g)' = sum* f**g = (f'/f*g + ln(f)*g')* f**g
        Some(RValue::BinaryOperation(
            Spanned::new(Multiply, span),
            sum,
            original,
        ))
    }

    fn fold_hypot(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        let darg1 = self.derivative(arg1);
        let darg2 = self.derivative(arg2);

        // arguments swapped to get arg2*darg2+ar1*darg1 instead of arg1*darg2+arg2*darg1
        let num = self.derivative_mul(arg1, arg2, darg2, darg1, span)?;
        let num = self.gen_temporary(num, span);

        let original = self.original_operand(span);

        // ( hypport(f,g) )' = ( f * f' + g * g' ) /  hyppot(f,g)
        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            num,
            original,
        ))
    }

    #[inline]
    fn fold_real_min(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        self.minmax_derivative(span, arg1, arg2, Type::REAL)
    }

    #[inline]
    fn fold_int_min(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        self.minmax_derivative(span, arg1, arg2, Type::INT)
    }

    #[inline]
    fn fold_real_max(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        self.minmax_derivative(span, arg1, arg2, Type::REAL)
    }

    #[inline]
    fn fold_int_max(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        self.minmax_derivative(span, arg1, arg2, Type::INT)
    }

    fn fold_atan2(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T {
        // u' = f'
        let darg1 = self.derivative(arg1);

        // v' = -g'
        let darg2 = self.derivative(arg2);
        let darg2 = match darg2 {
            Derivative::Zero => Derivative::Zero,
            Derivative::One => Derivative::Operand(Constant((-1.0).into())),
            Derivative::Operand(op) => {
                let operand = self.gen_temporary(
                    RValue::UnaryOperation(
                        Spanned::new(ArithmeticNegate, span),
                        Spanned::new(op, arg2.span),
                    ),
                    span,
                );
                Derivative::Operand(operand.contents)
            }
        };

        // num = u'*v+v'*u (u=lhs, v=rhs, derivatives are calculated above)
        let num = self.derivative_mul(arg1, arg1, darg1, darg2, span)?;
        let num = self.gen_temporary(num, span);
        // den = g*g + f*f
        let sum1 = self.gen_binop(Multiply, arg1.clone(), arg1.clone(), span);
        let sum2 = self.gen_binop(Multiply, arg2.clone(), arg2.clone(), span);
        let den = self.gen_binop(Plus, sum1, sum2, span);

        // ( arctan2(f,g) )' = num/den = (f'g - g'*f)/(f^2+g^2)
        Some(RValue::BinaryOperation(
            Spanned::new(Divide, span),
            num,
            den,
        ))
    }

    fn fold_cast(&mut self, arg: &COperand<C>, ty: Type) -> Self::T {
        // casting real->int is always zero
        // casting int-> real becomes identity
        // everything else will error later or doesn't reach this point
        if ty == Type::INT {
            Linter::dispatch_late(
                Box::new(RoundingDerivativeNotFullyDefined(arg.span)),
                self.origin,
            );
            None
        } else {
            let arg_derivative = self.derivative(arg).into_option()?;
            let arg_derivative = Operand::new(arg_derivative, arg.span);

            Some(RValue::Use(arg_derivative))
        }
    }

    fn fold_use(&mut self, arg: &COperand<C>) -> Self::T {
        let darg = self.derivative(arg).into_operand();
        let darg = Operand::new(darg, arg.span);
        Some(RValue::Use(darg))
    }

    fn fold_select(
        &mut self,
        cond: &COperand<C>,
        true_op: &COperand<C>,
        false_op: &COperand<C>,
    ) -> Self::T {
        let dtrue_op = self.derivative(true_op);
        let dfalse_op = self.derivative(false_op);
        match (dtrue_op, dfalse_op) {
            (Derivative::Zero, Derivative::Zero) => None,
            (dtrue_op, dfalse_op) => Some(RValue::Select(
                cond.clone(),
                Operand::new(dtrue_op.into_operand(), true_op.span),
                Operand::new(dfalse_op.into_operand(), false_op.span),
            )),
        }
    }

    fn fold_call(
        &mut self,
        call: &C,
        args: &IndexSlice<CallArg, [COperand<C>]>,
        span: Span,
    ) -> Self::T {
        call.derivative(args, self, span)
    }

    fn fold_array(&mut self, _args: &[COperand<C>], _span: Span, _ty: Type) -> Self::T {
        unreachable!("Array derivatives are not defined")
    }
}

impl<'lt, 'adlt, C: CfgFunctions, MC: CfgFunctions> RValueAutoDiff<'lt, 'adlt, C, MC> {
    fn minmax_derivative(
        &mut self,
        span: Span,
        arg1: &COperand<C>,
        arg2: &COperand<C>,
        ty: Type,
    ) -> Option<RValue<C>> {
        let darg1 = self.derivative(arg1);
        let darg2 = self.derivative(arg2);
        match (darg1, darg2) {
            (Derivative::Zero, Derivative::Zero) => None,
            (darg1, darg2) => {
                let cond = self.gen_logic_temporary(
                    RValue::Comparison(
                        Spanned::new(Equal, span),
                        self.original_operand(span),
                        arg1.clone(),
                        ty,
                    ),
                    span,
                );
                Some(RValue::Select(
                    cond,
                    Operand::new(darg1.into_operand(), arg1.span),
                    Operand::new(darg2.into_operand(), arg2.span),
                ))
            }
        }
    }

    fn derivative_abs(&mut self, span: Span, arg: &COperand<C>, ty: Type) -> Option<RValue<C>> {
        let darg = self.derivative(arg).into_option()?;
        let darg = Operand::new(darg, span);

        let minus_darg = self.gen_temporary(
            RValue::UnaryOperation(Spanned::new(ArithmeticNegate, span), darg.clone()),
            span,
        );

        let cond = self.gen_logic_temporary(
            RValue::Comparison(
                Spanned::new(Equal, span),
                self.original_operand(span),
                arg.clone(),
                ty,
            ),
            span,
        );
        Some(RValue::Select(cond, darg, minus_darg))
    }

    pub fn derivative_sum(
        lhs_derivative: Spanned<CallTypeDerivative<C>>,
        rhs_derivative: Spanned<CallTypeDerivative<C>>,
        span: Span,
    ) -> Option<RValue<C>> {
        match (
            lhs_derivative.contents.into_option(),
            rhs_derivative.contents.into_option(),
        ) {
            (None, None) => None,

            (None, Some(rhs)) => {
                let rhs = Operand::new(rhs, rhs_derivative.span);

                Some(RValue::Use(rhs))
            }

            (Some(lhs), None) => {
                let lhs = Operand::new(lhs, lhs_derivative.span);
                Some(RValue::Use(lhs))
            }

            (Some(lhs), Some(rhs)) => {
                let lhs = Operand::new(lhs, lhs_derivative.span);
                let rhs = Operand::new(rhs, rhs_derivative.span);
                Some(RValue::BinaryOperation(
                    Spanned::new(BinOp::Plus, span),
                    lhs,
                    rhs,
                ))
            }
        }
    }

    pub fn derivative_sub(
        lhs_derivative: Spanned<CallTypeDerivative<C>>,
        rhs_derivative: Spanned<CallTypeDerivative<C>>,
        span: Span,
    ) -> Option<RValue<C>> {
        match (
            lhs_derivative.contents.into_option(),
            rhs_derivative.contents.into_option(),
        ) {
            (None, None) => None,

            (None, Some(rhs)) => {
                let rhs = Operand::new(rhs, rhs_derivative.span);
                Some(RValue::UnaryOperation(
                    Spanned::new(UnaryOperator::ArithmeticNegate, span),
                    rhs,
                ))
            }

            (Some(val), None) => Some(RValue::Use(Operand::new(val, span))),

            (Some(lhs), Some(rhs)) => {
                let lhs = Operand::new(lhs, lhs_derivative.span);
                let rhs = Operand::new(rhs, rhs_derivative.span);
                Some(RValue::BinaryOperation(
                    Spanned::new(BinOp::Minus, span),
                    lhs,
                    rhs,
                ))
            }
        }
    }

    #[inline]
    fn derivative_mul(
        &mut self,
        lhs: &COperand<C>,
        rhs: &COperand<C>,
        lhs_derivative: CallTypeDerivative<C>,
        rhs_derivative: CallTypeDerivative<C>,
        span: Span,
    ) -> Option<RValue<C>> {
        let sum1 = match lhs_derivative {
            Derivative::Zero => lhs_derivative,
            Derivative::One => Derivative::Operand(rhs.contents.clone()),
            Derivative::Operand(operand) => {
                let operand = self.gen_binop(
                    BinOp::Multiply,
                    Operand::new(operand, lhs.span),
                    rhs.clone(),
                    span,
                );

                operand_to_derivative::<C>(operand)
            }
        };

        let sum2 = match rhs_derivative {
            Derivative::Zero => rhs_derivative,
            Derivative::One => Derivative::Operand(lhs.contents.clone()),
            Derivative::Operand(operand) => {
                let operand = self.gen_binop(
                    BinOp::Multiply,
                    Operand::new(operand, rhs.span),
                    lhs.clone(),
                    span,
                );
                operand_to_derivative::<C>(operand)
            }
        };

        Self::derivative_sum(
            Spanned::new(sum1, lhs.span),
            Spanned::new(sum2, rhs.span),
            span,
        )
    }

    // f/g -> (f'*g - g' *f) / g^2 = f'/g - g'*f/g^2
    fn derivative_div(
        &mut self,
        lhs: &COperand<C>,
        rhs: &COperand<C>,
        lhs_derivative: CallTypeDerivative<C>,
        rhs_derivative: CallTypeDerivative<C>,
        span: Span,
    ) -> Option<RValue<C>> {
        let sum1 = match lhs_derivative {
            Derivative::Zero => Derivative::Zero,
            Derivative::One => {
                let invers = if let Some(invers) =
                    &self.outer_derivative_cache[OuterDerivativeCacheSlot::Lhs]
                {
                    invers.clone()
                } else {
                    let invers = self.gen_binop(
                        BinOp::Divide,
                        Operand::new(OperandData::Constant(Scalar(Real(1.0))), span),
                        rhs.clone(),
                        span,
                    );
                    self.outer_derivative_cache[OuterDerivativeCacheSlot::Lhs] =
                        Some(invers.clone());
                    invers
                };

                Derivative::Operand(invers.contents)
            }
            Derivative::Operand(ref operand) => {
                let operand = self.gen_binop(
                    BinOp::Divide,
                    Operand::new(operand.clone(), lhs.span),
                    rhs.clone(),
                    span,
                );
                operand_to_derivative::<C>(operand)
            }
        };

        let sum2 = if let Some(res) = self.chain_rule_with_known_derivative(
            rhs_derivative,
            |fold| {
                let bottom = fold.gen_binop(BinOp::Multiply, rhs.clone(), rhs.clone(), span);
                RValue::BinaryOperation(Spanned::new(BinOp::Divide, span), lhs.clone(), bottom)
            },
            span,
            OuterDerivativeCacheSlot::Rhs,
        ) {
            Derivative::Operand(self.gen_temporary(res, span).contents)
        } else {
            Derivative::Zero
        };

        Self::derivative_sub(
            Spanned::new(sum1, lhs.span),
            Spanned::new(sum2, rhs.span),
            span,
        )
    }

    pub fn gen_constant(val: f64, span: Span) -> COperand<C> {
        Operand::new(OperandData::Constant(val.into()), span)
    }

    pub fn gen_temporary(&mut self, rhs: RValue<C>, span: Span) -> COperand<C> {
        let local = self.ad.cfg.new_temporary(Type::REAL);
        let kind = StmntKind::Assignment(local, rhs);
        self.ad.forward_stmnts.push((kind, self.origin));
        Operand::new(OperandData::Copy(local), span)
    }

    pub fn gen_logic_temporary(&mut self, rhs: RValue<C>, span: Span) -> COperand<C> {
        let local = self.ad.cfg.new_temporary(Type::BOOL);
        let kind = StmntKind::Assignment(local, rhs);
        self.ad.forward_stmnts.push((kind, self.origin));
        Operand::new(OperandData::Copy(local), span)
    }

    pub fn gen_binop(
        &mut self,
        op: BinOp,
        lhs: COperand<C>,
        rhs: COperand<C>,
        span: Span,
    ) -> COperand<C> {
        let rhs = RValue::BinaryOperation(Spanned::new(op, span), lhs, rhs);
        self.gen_temporary(rhs, span)
    }

    fn gen_single_arg_math(&mut self, kind: Math1, arg: COperand<C>, span: Span) -> COperand<C> {
        let rhs = RValue::Math1(Spanned::new(kind, span), arg);
        self.gen_temporary(rhs, span)
    }

    pub fn chain_rule(
        &mut self,
        arg: &COperand<C>,
        generate_outer: impl FnOnce(&mut Self) -> RValue<C>,
        span: Span,
        cache_slot: OuterDerivativeCacheSlot,
    ) -> Option<RValue<C>> {
        let derivative = self.derivative(arg);
        self.chain_rule_with_known_derivative(derivative, generate_outer, span, cache_slot)
    }

    fn chain_rule_with_known_derivative(
        &mut self,
        derivative: CallTypeDerivative<C>,
        generate_outer: impl FnOnce(&mut Self) -> RValue<C>,
        span: Span,
        cache_slot: OuterDerivativeCacheSlot,
    ) -> Option<RValue<C>> {
        match derivative {
            Derivative::Zero => None,
            Derivative::One => Some(generate_outer(self)),
            Derivative::Operand(inner) => {
                let outer = if let Some(outer) = &self.outer_derivative_cache[cache_slot] {
                    outer.clone()
                } else {
                    let outer = generate_outer(self);
                    let outer = self.gen_temporary(outer, span);
                    self.outer_derivative_cache[cache_slot] = Some(outer.clone());
                    outer
                };

                let inner = Operand::new(inner, span);
                Some(RValue::BinaryOperation(
                    Spanned::new(BinOp::Multiply, span),
                    inner,
                    outer,
                ))
            }
        }
    }

    /// Generate an expression whos form is determined by [`OnePlusSquareKind`]
    fn gen_one_and_square(
        &mut self,
        kind: OneAndSquareKind,
        arg: COperand<C>,
        op_span: Span,
    ) -> RValue<C> {
        let span = arg.span;
        let squared = self.gen_binop(Multiply, arg.clone(), arg, span);

        match kind {
            OneAndSquareKind::Positive => RValue::BinaryOperation(
                Spanned::new(Plus, op_span),
                Self::gen_constant(1.0, op_span),
                squared,
            ),
            OneAndSquareKind::OneMinusSquared => RValue::BinaryOperation(
                Spanned::new(Minus, op_span),
                Self::gen_constant(1.0, op_span),
                squared,
            ),
            OneAndSquareKind::SquaredMinusOne => RValue::BinaryOperation(
                Spanned::new(Minus, op_span),
                squared,
                Self::gen_constant(1.0, op_span),
            ),
        }
    }

    /// Generates an operand that reads the local that the rvalue currently being folded writes to
    fn original_operand(&self, span: Span) -> COperand<C> {
        Operand::new(OperandData::Copy(self.original_local), span)
    }

    pub fn derivative(&mut self, operand: &COperand<C>) -> CallTypeDerivative<C> {
        // If the MIR and CFG were properly type checked this can't panic
        self.ad
            .cfg
            .demand_operand_derivative_unchecked(self.ad.mir, operand, self.unknown)
    }
}
