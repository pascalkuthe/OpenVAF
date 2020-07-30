//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! Simple constant folding algorithm

use core::mem::replace;

use float_cmp::{ApproxEq, F64Margin};
use log::trace;
use num_traits::Pow;

pub use lints::ConstantOverflow;
pub use propagation::GlobalConstants;
pub use resolver::{ConstResolver, NoConstResolution};

use crate::analysis::constant_fold::DiamondLattice::{NotAConstant, Val};
use crate::ast::UnaryOperator;
use crate::ir::hir::DisciplineAccess;
use crate::ir::ids::{
    BranchId, IntegerExpressionId, NetId, ParameterId, PortId, RealExpressionId,
    StringExpressionId, VariableId,
};
use crate::ir::mir::fold::integer_expressions::{
    IntegerBinaryOperatorFold, IntegerComparisonFold, IntegerExprFold, RealComparisonFold,
};
use crate::ir::mir::fold::real_expressions::{
    RealBinaryOperatorFold, RealBuiltInFunctionCall2pFold, RealExprFold,
};
use crate::ir::mir::fold::string_expressions::StringExprFold;
use crate::ir::mir::{ExpressionId, Mir};
use crate::ir::{DoubleArgMath, NoiseSource, SingleArgMath, Spanned};
use crate::lints::Linter;
use crate::literals::StringLiteral;
use crate::mir::fold::integer_expressions::walk_integer_expression;
use crate::mir::fold::real_expressions::{walk_real_expression, RealBuiltInFunctionCall1pFold};
use crate::mir::fold::string_expressions::walk_string_expression;
use crate::mir::{
    ComparisonOperator, IntegerBinaryOperator, IntegerExpression, RealBinaryOperator,
    RealExpression, StringExpression,
};
use crate::sourcemap::Span;

mod lattice;
mod lints;
pub mod propagation;
mod resolver;

pub use lattice::{DiamondLattice, TypedDiamondLattice};

/// Abstraction over mutability of the mir for constant folding (see [`constant_eval_real_expr`](crate::mir::Mir::constant_eval_real_expr))
trait ConstantFoldType: AsRef<Mir> {
    fn overwrite_real_expr(&mut self, expr: RealExpressionId, val: RealExpression);
    fn overwrite_int_expr(&mut self, expr: IntegerExpressionId, val: IntegerExpression);
    fn overwrite_str_expr(&mut self, expr: StringExpressionId, val: StringExpression);
}

struct MutatingConstantFold<'lt>(pub &'lt mut Mir);

impl<'lt> AsRef<Mir> for MutatingConstantFold<'lt> {
    fn as_ref(&self) -> &Mir {
        &self.0
    }
}

impl<'lt> ConstantFoldType for MutatingConstantFold<'lt> {
    fn overwrite_real_expr(&mut self, expr: RealExpressionId, val: RealExpression) {
        self.0[expr].contents = val;
    }

    fn overwrite_int_expr(&mut self, expr: IntegerExpressionId, val: IntegerExpression) {
        self.0[expr].contents = val;
    }

    fn overwrite_str_expr(&mut self, expr: StringExpressionId, val: StringExpression) {
        self.0[expr].contents = val;
    }
}

struct ReadingConstantFold<'lt>(pub &'lt Mir);

impl<'lt> AsRef<Mir> for ReadingConstantFold<'lt> {
    fn as_ref(&self) -> &Mir {
        &self.0
    }
}

impl<'lt> ConstantFoldType for ReadingConstantFold<'lt> {
    #[inline(always)]
    fn overwrite_real_expr(&mut self, _: RealExpressionId, _: RealExpression) {}

    #[inline(always)]
    fn overwrite_int_expr(&mut self, _: IntegerExpressionId, _: IntegerExpression) {}

    #[inline(always)]
    fn overwrite_str_expr(&mut self, _: StringExpressionId, _: StringExpression) {}
}

/// Implementation of constant folding as a [mir expression fold](crate::ir::mir::fold).
/// All methods return `None` if constant folding was not possible and `Some(value)` otherwise

struct ConstantFold<'lt, T: ConstantFoldType, R: ConstResolver, E> {
    fold_type: &'lt mut T,
    resolver: &'lt mut R,
    expr: E,
}

impl<'lt, T: ConstantFoldType, R: ConstResolver, E> ConstantFold<'lt, T, R, E> {
    fn fold_real_expression(&mut self, expr: RealExpressionId) -> DiamondLattice<f64> {
        ConstantFold {
            fold_type: self.fold_type,
            resolver: self.resolver,
            expr,
        }
        .fold_real_expr(expr)
    }

    fn fold_int_expression(&mut self, expr: IntegerExpressionId) -> DiamondLattice<i64> {
        ConstantFold {
            fold_type: self.fold_type,
            resolver: self.resolver,
            expr,
        }
        .fold_integer_expr(expr)
    }

    fn fold_str_expression(&mut self, expr: StringExpressionId) -> DiamondLattice<StringLiteral> {
        ConstantFold {
            fold_type: self.fold_type,
            resolver: self.resolver,
            expr,
        }
        .fold_string_expr(expr)
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> RealExprFold
    for ConstantFold<'lt, T, R, RealExpressionId>
{
    type T = DiamondLattice<f64>;

    fn mir(&self) -> &Mir {
        self.fold_type.as_ref()
    }

    #[inline]
    fn fold_real_expr(&mut self, expr: RealExpressionId) -> DiamondLattice<f64> {
        let old = replace(&mut self.expr, expr);
        let res = walk_real_expression(self, expr);
        self.expr = old;
        res
    }

    fn fold_literal(&mut self, val: f64) -> DiamondLattice<f64> {
        Val(val)
    }

    fn fold_binary_operator(
        &mut self,
        lhs: RealExpressionId,
        op: Spanned<RealBinaryOperator>,
        rhs: RealExpressionId,
    ) -> DiamondLattice<f64> {
        self.fold_real_binary_op(lhs, op.contents, rhs)
    }

    fn fold_negate(&mut self, _: Span, arg: RealExpressionId) -> DiamondLattice<f64> {
        -self.fold_real_expr(arg)
    }

    fn fold_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: RealExpressionId,
        false_expr: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let cond = self.fold_int_expression(cond);
        cond.and_then(|cond| {
            let (expr, val) = if cond == 0 {
                (false_expr, self.fold_real_expression(false_expr))
            } else {
                (true_expr, self.fold_real_expression(true_expr))
            };

            if !matches!(val, Val(_)) {
                self.fold_type
                    .overwrite_real_expr(self.expr, self.mir()[expr].contents.clone())
            }

            val
        })
    }

    fn fold_variable_reference(&mut self, var: VariableId) -> DiamondLattice<f64> {
        self.resolver.real_variable_value(var)
    }

    fn fold_parameter_reference(&mut self, param: ParameterId) -> DiamondLattice<f64> {
        self.resolver.real_parameter_value(param)
    }

    fn fold_branch_access(
        &mut self,
        _discipline_accesss: DisciplineAccess,
        _branch: BranchId,
        _time_derivative_order: u8,
    ) -> DiamondLattice<f64> {
        NotAConstant
    }

    fn fold_port_flow_access(
        &mut self,
        _port: PortId,
        _time_derivative_order: u8,
    ) -> DiamondLattice<f64> {
        NotAConstant
    }

    fn fold_noise(
        &mut self,
        _noise_src: NoiseSource<RealExpressionId, ()>,
        _name: Option<StringLiteral>,
    ) -> DiamondLattice<f64> {
        NotAConstant
    }

    fn fold_builtin_function_call_1p(
        &mut self,
        call: SingleArgMath,
        arg: RealExpressionId,
    ) -> DiamondLattice<f64> {
        self.fold_real_builtin_function_call_1p(call, arg)
    }

    fn fold_builtin_function_call_2p(
        &mut self,
        call: DoubleArgMath,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) -> DiamondLattice<f64> {
        self.fold_real_builtin_function_call_2p(call, arg1, arg2)
    }

    fn fold_temperature(&mut self) -> DiamondLattice<f64> {
        NotAConstant
    }

    fn fold_sim_param(
        &mut self,
        _name: StringExpressionId,
        _default: Option<RealExpressionId>,
    ) -> DiamondLattice<f64> {
        NotAConstant
    }

    fn fold_integer_conversion(&mut self, expr: IntegerExpressionId) -> DiamondLattice<f64> {
        ConstantFold {
            expr,
            fold_type: self.fold_type,
            resolver: self.resolver,
        }
        .fold_integer_expr(expr)
        .map(|val| val as f64)
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> RealBuiltInFunctionCall2pFold
    for ConstantFold<'lt, T, R, RealExpressionId>
{
    type T = DiamondLattice<f64>;

    fn fold_pow(
        &mut self,
        arg1_expr: RealExpressionId,
        arg2_expr: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let arg1 = self.fold_real_expr(arg1_expr);
        let arg2 = self.fold_real_expr(arg2_expr);
        arg1.pow(arg2)
    }

    fn fold_hypot(
        &mut self,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let arg1 = self.fold_real_expr(arg1);
        let arg2 = self.fold_real_expr(arg2);
        arg1.apply_binary_op(|x, y| x.hypot(y), arg2)
    }

    fn fold_arctan2(
        &mut self,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let arg1 = self.fold_real_expr(arg1);
        let arg2 = self.fold_real_expr(arg2);
        arg1.apply_binary_op(|x, y| x.atan2(y), arg2)
    }

    fn fold_max(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> DiamondLattice<f64> {
        let arg1 = self.fold_real_expr(arg1);
        let arg2 = self.fold_real_expr(arg2);
        arg1.apply_binary_op(|x, y| x.max(y), arg2)
    }

    fn fold_min(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> DiamondLattice<f64> {
        let arg1 = self.fold_real_expr(arg1);
        let arg2 = self.fold_real_expr(arg2);
        arg1.apply_binary_op(|x, y| x.min(y), arg2)
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> RealBuiltInFunctionCall1pFold
    for ConstantFold<'lt, T, R, RealExpressionId>
{
    type T = DiamondLattice<f64>;

    fn fold_sqrt(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.sqrt())
    }

    fn fold_exp(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.exp())
    }

    fn fold_ln(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.ln())
    }

    fn fold_log(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.log10())
    }

    fn fold_abs(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.abs())
    }

    fn fold_floor(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.floor())
    }

    fn fold_ceil(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.ceil())
    }

    fn fold_sin(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.sin())
    }

    fn fold_cos(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.cos())
    }

    fn fold_tan(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.tan())
    }

    fn fold_arcsin(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.asin())
    }

    fn fold_arccos(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.acos())
    }

    fn fold_arctan(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.atan())
    }

    fn fold_sinh(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.sinh())
    }

    fn fold_cosh(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.cosh())
    }

    fn fold_tanh(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.tanh())
    }

    fn fold_arcsinh(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.asinh())
    }

    fn fold_arccosh(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.acosh())
    }

    fn fold_arctanh(&mut self, arg: RealExpressionId) -> DiamondLattice<f64> {
        self.fold_real_expr(arg).map(|arg| arg.atanh())
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> RealBinaryOperatorFold
    for ConstantFold<'lt, T, R, RealExpressionId>
{
    type T = DiamondLattice<f64>;

    fn fold_sum(
        &mut self,
        lhs_expr: RealExpressionId,
        rhs_expr: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let lhs = self.fold_real_expr(lhs_expr);
        let rhs = self.fold_real_expr(rhs_expr);
        lhs + rhs
    }

    fn fold_diff(
        &mut self,
        lhs_expr: RealExpressionId,
        rhs_expr: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let lhs = self.fold_real_expr(lhs_expr);
        let rhs = self.fold_real_expr(rhs_expr);

        lhs - rhs
    }

    fn fold_mul(
        &mut self,
        lhs_expr: RealExpressionId,
        rhs_expr: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let lhs = self.fold_real_expr(lhs_expr);
        let rhs = self.fold_real_expr(rhs_expr);

        lhs * rhs
    }

    fn fold_quotient(
        &mut self,
        lhs_expr: RealExpressionId,
        rhs_expr: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let lhs = self.fold_real_expr(lhs_expr);
        let rhs = self.fold_real_expr(rhs_expr);

        lhs / rhs
    }

    fn fold_pow(
        &mut self,
        lhs_expr: RealExpressionId,
        rhs_expr: RealExpressionId,
    ) -> DiamondLattice<f64> {
        let lhs = self.fold_real_expr(lhs_expr);
        let rhs = self.fold_real_expr(rhs_expr);

        lhs.pow(rhs)
    }

    fn fold_mod(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> DiamondLattice<f64> {
        let lhs = self.fold_real_expr(lhs);
        let rhs = self.fold_real_expr(rhs);
        lhs.apply_binary_op(|x, y| x % y, rhs)
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> IntegerExprFold
    for ConstantFold<'lt, T, R, IntegerExpressionId>
{
    type T = DiamondLattice<i64>;

    fn mir(&self) -> &Mir {
        self.fold_type.as_ref()
    }

    fn fold_integer_expr(&mut self, expr: IntegerExpressionId) -> DiamondLattice<i64> {
        let old = replace(&mut self.expr, expr);
        let res = walk_integer_expression(self, expr);
        if let Val(res) = res {
            self.fold_type
                .overwrite_int_expr(self.expr, IntegerExpression::Literal(res))
        }
        self.expr = old;
        res
    }

    fn fold_literal(&mut self, val: i64) -> DiamondLattice<i64> {
        Val(val)
    }

    fn fold_binary_operator(
        &mut self,
        lhs: IntegerExpressionId,
        op: Spanned<IntegerBinaryOperator>,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        self.fold_integer_binary_op(lhs, op.contents, rhs)
    }

    fn fold_integer_comparison(
        &mut self,
        lhs: IntegerExpressionId,
        op: Spanned<ComparisonOperator>,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        IntegerComparisonFold::fold_integer_comparison(self, lhs, op.contents, rhs)
    }

    fn fold_real_comparison(
        &mut self,
        lhs: RealExpressionId,
        op: Spanned<ComparisonOperator>,
        rhs: RealExpressionId,
    ) -> DiamondLattice<i64> {
        RealComparisonFold::fold_real_comparison(self, lhs, op.contents, rhs)
    }

    fn fold_unary_op(
        &mut self,
        op: Spanned<UnaryOperator>,
        arg: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let arg = self.fold_integer_expr(arg);
        match op.contents {
            UnaryOperator::BitNegate | UnaryOperator::LogicNegate => !arg,
            UnaryOperator::ArithmeticNegate => -arg,
            UnaryOperator::ExplicitPositive => arg,
        }
    }

    fn fold_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: IntegerExpressionId,
        false_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let cond = self.fold_int_expression(cond);
        cond.and_then(|cond| {
            let (expr, val) = if cond == 0 {
                (false_expr, self.fold_int_expression(false_expr))
            } else {
                (true_expr, self.fold_int_expression(true_expr))
            };

            if !matches!(val, Val(_)) {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[expr].contents.clone())
            }

            val
        })
    }

    fn fold_min(
        &mut self,
        arg1: IntegerExpressionId,
        arg2: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let arg1 = self.fold_integer_expr(arg1);
        let arg2 = self.fold_integer_expr(arg2);
        arg1.apply_binary_op(i64::min, arg2)
    }

    fn fold_max(
        &mut self,
        arg1: IntegerExpressionId,
        arg2: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let arg1 = self.fold_integer_expr(arg1);
        let arg2 = self.fold_integer_expr(arg2);
        arg1.apply_binary_op(i64::max, arg2)
    }

    fn fold_abs(&mut self, arg: IntegerExpressionId) -> DiamondLattice<i64> {
        self.fold_integer_expr(arg).map(|arg| arg.abs())
    }

    fn fold_variable_reference(&mut self, var: VariableId) -> DiamondLattice<i64> {
        self.resolver.int_variable_value(var)
    }

    fn fold_parameter_reference(&mut self, param: ParameterId) -> DiamondLattice<i64> {
        self.resolver.int_parameter_value(param)
    }

    fn fold_real_cast(&mut self, expr: RealExpressionId) -> DiamondLattice<i64> {
        self.fold_real_expression(expr)
            .map(|val| val.round() as i64)
    }

    fn fold_port_connected(&mut self, _: PortId) -> DiamondLattice<i64> {
        NotAConstant
    }

    fn fold_param_given(&mut self, _: ParameterId) -> DiamondLattice<i64> {
        NotAConstant
    }

    fn fold_port_reference(&mut self, _: PortId) -> DiamondLattice<i64> {
        NotAConstant
    }

    fn fold_net_reference(&mut self, _: NetId) -> DiamondLattice<i64> {
        NotAConstant
    }

    fn fold_string_eq(
        &mut self,
        lhs: StringExpressionId,
        rhs: StringExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_str_expression(lhs);
        let rhs = self.fold_str_expression(rhs);

        lhs.apply_binary_op(|x, y| (x == y) as i64, rhs)
    }

    fn fold_string_neq(
        &mut self,
        lhs: StringExpressionId,
        rhs: StringExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_str_expression(lhs);
        let rhs = self.fold_str_expression(rhs);

        lhs.apply_binary_op(|x, y| (x != y) as i64, rhs)
    }
}

const U32_MAX_I64: i64 = u32::MAX as i64;
const U32_OVERFLOW_START: i64 = U32_MAX_I64 + 1;

impl<'lt, T: ConstantFoldType, R: ConstResolver> IntegerBinaryOperatorFold
    for ConstantFold<'lt, T, R, IntegerExpressionId>
{
    type T = DiamondLattice<i64>;

    fn fold_sum(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);

        lhs + rhs
    }

    fn fold_diff(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);

        lhs - rhs
    }

    fn fold_mul(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);

        lhs * rhs
    }

    fn fold_quotient(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);

        if rhs == Val(0) {
            Linter::dispatch_late(
                Box::new(ConstantOverflow(self.mir()[self.expr].span)),
                self.expr.into(),
            )
        }

        lhs / rhs
    }

    fn fold_pow(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = match self.fold_integer_expr(rhs_expr) {
            // Negative powers are 0 < |1/x| < 1 and as such always truncated to 0 according to VAMS standard
            Val(i64::MIN..=-1) => return Val(0),

            // valid range
            Val(val @ 0..=U32_MAX_I64) => Val(val as u32),

            // raising to a power higher than u32::MAX leads to an overflow
            Val(U32_OVERFLOW_START..=i64::MAX) => {
                Linter::dispatch_late(
                    Box::new(ConstantOverflow(self.mir()[self.expr].span)),
                    self.expr.into(),
                );
                NotAConstant
            }

            DiamondLattice::Unknown => DiamondLattice::Unknown,
            DiamondLattice::NotAConstant => DiamondLattice::NotAConstant,
        };

        lhs.pow(rhs)
    }

    fn fold_mod(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);

        lhs % rhs
    }

    fn fold_shiftl(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);

        lhs << rhs
    }

    fn fold_shiftr(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);

        lhs >> rhs
    }

    fn fold_xor(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);

        lhs ^ rhs
    }

    fn fold_nxor(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);

        !(lhs ^ rhs)
    }

    fn fold_and(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);

        match (lhs, rhs) {
            (Val(i64::MAX), _) => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[lhs_expr].contents.clone());
            }
            (_, Val(i64::MAX)) => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[rhs_expr].contents.clone());
            }
            _ => (),
        }

        lhs & rhs
    }

    fn fold_or(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);
        trace!("folding {:?} | {:?}", lhs, rhs);

        match (lhs, rhs) {
            (Val(0), _) => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[lhs_expr].contents.clone());
            }
            (_, Val(0)) => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[rhs_expr].contents.clone());
            }
            _ => (),
        }

        lhs | rhs
    }

    fn fold_logic_and(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);
        trace!("folding {:?} && {:?}", lhs, rhs);

        match (lhs, rhs) {
            (Val(x), _) | (_, Val(x)) if x == 0 => Val(0),
            (Val(x), y) if x != 0 => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[rhs_expr].contents.clone());
                y
            }
            (y, Val(x)) if x != 0 => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[lhs_expr].contents.clone());
                y
            }
            (lhs, rhs) => lhs.apply_binary_op(|x, y| ((x != 0) && (y != 0)) as i64, rhs),
        }
    }

    fn fold_logic_or(
        &mut self,
        lhs_expr: IntegerExpressionId,
        rhs_expr: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs_expr);
        let rhs = self.fold_integer_expr(rhs_expr);
        trace!("folding {:?} || {:?}", lhs, rhs);

        match (lhs, rhs) {
            (Val(x), _) | (_, Val(x)) if x != 0 => Val(1),
            (Val(x), y) if x == 0 => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[rhs_expr].contents.clone());
                y
            }
            (y, Val(x)) if x == 0 => {
                self.fold_type
                    .overwrite_int_expr(self.expr, self.mir()[lhs_expr].contents.clone());
                y
            }
            (lhs, rhs) => lhs.apply_binary_op(|x, y| ((x != 0) || (y != 0)) as i64, rhs),
        }
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> IntegerComparisonFold
    for ConstantFold<'lt, T, R, IntegerExpressionId>
{
    type T = DiamondLattice<i64>;

    fn fold_lt(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);
        trace!("folding {:?} < {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x < y) as i64, rhs)
    }

    fn fold_le(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);
        trace!("folding {:?} <= {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x <= y) as i64, rhs)
    }

    fn fold_gt(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);
        trace!("folding {:?} > {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x > y) as i64, rhs)
    }

    fn fold_ge(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);
        trace!("folding {:?} >= {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x >= y) as i64, rhs)
    }

    fn fold_eq(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);
        trace!("folding {:?} == {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x == y) as i64, rhs)
    }

    fn fold_ne(
        &mut self,
        lhs: IntegerExpressionId,
        rhs: IntegerExpressionId,
    ) -> DiamondLattice<i64> {
        let lhs = self.fold_integer_expr(lhs);
        let rhs = self.fold_integer_expr(rhs);
        trace!("folding {:?} != {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x != y) as i64, rhs)
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> RealComparisonFold
    for ConstantFold<'lt, T, R, IntegerExpressionId>
{
    type T = DiamondLattice<i64>;

    fn fold_lt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> DiamondLattice<i64> {
        let lhs = self.fold_real_expression(lhs);
        let rhs = self.fold_real_expression(rhs);
        trace!("folding {:?} < {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x < y) as i64, rhs)
    }

    fn fold_le(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> DiamondLattice<i64> {
        let lhs = self.fold_real_expression(lhs);
        let rhs = self.fold_real_expression(rhs);
        trace!("folding {:?} <= {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x <= y) as i64, rhs)
    }

    fn fold_gt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> DiamondLattice<i64> {
        let lhs = self.fold_real_expression(lhs);
        let rhs = self.fold_real_expression(rhs);
        trace!("folding {:?} > {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x > y) as i64, rhs)
    }

    fn fold_ge(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> DiamondLattice<i64> {
        let lhs = self.fold_real_expression(lhs);
        let rhs = self.fold_real_expression(rhs);
        trace!("folding {:?} >= {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| (x >= y) as i64, rhs)
    }

    fn fold_eq(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> DiamondLattice<i64> {
        let lhs = self.fold_real_expression(lhs);
        let rhs = self.fold_real_expression(rhs);
        trace!("folding {:?} == {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| x.approx_eq(y, F64Margin::default()) as i64, rhs)
    }

    fn fold_ne(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> DiamondLattice<i64> {
        let lhs = self.fold_real_expression(lhs);
        let rhs = self.fold_real_expression(rhs);
        trace!("folding {:?} != {:?}", lhs, rhs);
        lhs.apply_binary_op(|x, y| x.approx_ne(y, F64Margin::default()) as i64, rhs)
    }
}

impl<'lt, T: ConstantFoldType, R: ConstResolver> StringExprFold
    for ConstantFold<'lt, T, R, StringExpressionId>
{
    type T = DiamondLattice<StringLiteral>;

    #[inline]
    fn fold_string_expr(&mut self, expr: StringExpressionId) -> DiamondLattice<StringLiteral> {
        let old = replace(&mut self.expr, expr);
        let res = walk_string_expression(self, expr);
        if let Val(res) = res {
            self.fold_type
                .overwrite_str_expr(expr, StringExpression::Literal(res))
        }
        self.expr = old;
        res
    }

    fn mir(&self) -> &Mir {
        self.fold_type.as_ref()
    }

    fn fold_literal(&mut self, val: StringLiteral) -> DiamondLattice<StringLiteral> {
        Val(val)
    }

    fn fold_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: StringExpressionId,
        false_expr: StringExpressionId,
    ) -> DiamondLattice<StringLiteral> {
        let cond = self.fold_int_expression(cond);
        cond.and_then(|cond| {
            let (expr, val) = if cond == 0 {
                (false_expr, self.fold_string_expr(false_expr))
            } else {
                (true_expr, self.fold_string_expr(true_expr))
            };

            if !matches!(val, Val(_)) {
                self.fold_type
                    .overwrite_str_expr(self.expr, self.mir()[expr].contents.clone())
            }

            val
        })
    }

    fn fold_variable_reference(&mut self, var: VariableId) -> DiamondLattice<StringLiteral> {
        self.resolver.str_variable_value(var)
    }

    fn fold_parameter_reference(&mut self, param: ParameterId) -> DiamondLattice<StringLiteral> {
        self.resolver.str_parameter_value(param)
    }

    fn fold_sim_parameter(&mut self, _name: StringExpressionId) -> DiamondLattice<StringLiteral> {
        NotAConstant
    }
}

impl Mir {
    /// Tries to fold as many constants in `expr` as possible.
    ///
    /// This function can only fold simple constants
    /// (for example `abs(x) > 0?0:1` can not be cosntant folded currently)
    ///
    ///
    /// # Arguments
    ///
    /// * `expr` - the Expression to be constant folded
    /// * `resolver` - Trait that handles constant folding references to variables and parameters
    ///
    ///
    /// # Returns
    ///
    /// * `None` if `expr` was only partially (or not at all) constant folded
    /// * `Some(val)` if `expr` was completly constant folded
    ///
    ///
    /// # Examples
    ///
    /// `x + (32*2)`
    /// * `resolver` can not resolve `foo`: `foo + (32*2)` -> `foo + 64` (return value `None`)
    /// * `resolver` resolved `foo` to `-16`: `foo + (32*2)` -> `foo + 64` (return value `Some(42)`)
    pub fn constant_fold_expr(
        &mut self,
        expr: ExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> TypedDiamondLattice {
        match expr {
            ExpressionId::String(expr) => self.constant_fold_str_expr(expr, resolver).into(),
            ExpressionId::Real(expr) => self.constant_fold_real_expr(expr, resolver).into(),
            ExpressionId::Integer(expr) => self.constant_fold_int_expr(expr, resolver).into(),
        }
    }

    /// See [`constant_fold_expr`](crate::mir::Mir::constant_fold_real_expr)
    pub fn constant_fold_real_expr(
        &mut self,
        expr: RealExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> DiamondLattice<f64> {
        ConstantFold {
            fold_type: &mut MutatingConstantFold(self),
            resolver,
            expr,
        }
        .fold_real_expr(expr)
    }

    /// See [`constant_fold_expr`](crate::mir::Mir::constant_fold_real_expr)
    pub fn constant_fold_int_expr(
        &mut self,
        expr: IntegerExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> DiamondLattice<i64> {
        ConstantFold {
            fold_type: &mut MutatingConstantFold(self),
            resolver,
            expr,
        }
        .fold_integer_expr(expr)
    }

    /// See [`constant_fold_expr`](crate::mir::Mir::constant_fold_real_expr)
    pub fn constant_fold_str_expr(
        &mut self,
        expr: StringExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> DiamondLattice<StringLiteral> {
        ConstantFold {
            fold_type: &mut MutatingConstantFold(self),
            resolver,
            expr,
        }
        .fold_string_expr(expr)
    }

    /// Same as [`constant_fold_expr`](crate::mir::Mir::constant_fold_str_expr) but no expressions in `self` actually change only the return value is calculated
    pub fn constant_eval_expr(
        &self,
        expr: ExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> TypedDiamondLattice {
        match expr {
            ExpressionId::String(expr) => self.constant_eval_str_expr(expr, resolver).into(),
            ExpressionId::Real(expr) => self.constant_eval_real_expr(expr, resolver).into(),
            ExpressionId::Integer(expr) => self.constant_eval_int_expr(expr, resolver).into(),
        }
    }

    /// See [`constant_eval_expr`](crate::mir::Mir::constant_eval_real_expr)
    pub fn constant_eval_real_expr(
        &self,
        expr: RealExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> DiamondLattice<f64> {
        ConstantFold {
            fold_type: &mut ReadingConstantFold(self),
            resolver,
            expr,
        }
        .fold_real_expr(expr)
    }

    /// See [`constant_eval_expr`](crate::mir::Mir::constant_eval_real_expr)
    pub fn constant_eval_int_expr(
        &self,
        expr: IntegerExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> DiamondLattice<i64> {
        ConstantFold {
            fold_type: &mut ReadingConstantFold(self),
            resolver,
            expr,
        }
        .fold_integer_expr(expr)
    }

    /// See [`constant_eval_expr`](crate::mir::Mir::constant_eval_real_expr)
    pub fn constant_eval_str_expr(
        &self,
        expr: StringExpressionId,
        resolver: &mut impl ConstResolver,
    ) -> DiamondLattice<StringLiteral> {
        ConstantFold {
            fold_type: &mut ReadingConstantFold(self),
            resolver,
            expr,
        }
        .fold_string_expr(expr)
    }
}
