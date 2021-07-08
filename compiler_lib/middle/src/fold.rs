/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::{BinOp, COperand, CallArg, CfgFunctions, ComparisonOp, Math1, Math2, RValue, Type};
use openvaf_data_structures::index_vec::IndexSlice;
use openvaf_ir::UnaryOperator;
use openvaf_session::sourcemap::Span;

#[inline]
pub fn fold_rvalue<C: CfgFunctions, F: RValueFold<C>>(
    fold: &mut F,
    rvalue: &RValue<C>,
    ty: Type,
) -> F::T {
    match *rvalue {
        RValue::UnaryOperation(op, ref arg) => match op.contents {
            UnaryOperator::ArithmeticNegate if ty == Type::REAL => {
                fold.fold_real_arith_negate(op.span, arg)
            }
            UnaryOperator::ArithmeticNegate if ty == Type::INT => {
                fold.fold_int_arith_negate(op.span, arg)
            }
            UnaryOperator::ArithmeticNegate if ty == Type::CMPLX => {
                fold.fold_cmplx_arith_negate(op.span, arg)
            }
            UnaryOperator::ArithmeticNegate => unreachable!("Misstyped MIR"),

            // always integer (assuming correctly typed MIR)
            UnaryOperator::BitNegate => fold.fold_bit_negate(op.span, arg),
            UnaryOperator::LogicNegate => fold.fold_logic_negate(op.span, arg),
        },

        RValue::BinaryOperation(op, ref lhs, ref rhs) => match op.contents {
            BinOp::Plus if ty == Type::CMPLX => fold.fold_cmplx_add(op.span, lhs, rhs),
            BinOp::Plus if ty == Type::REAL => fold.fold_real_add(op.span, lhs, rhs),
            BinOp::Plus if ty == Type::INT => fold.fold_int_add(op.span, lhs, rhs),
            BinOp::Plus => unreachable!("Misstyped MIR"),

            BinOp::Minus if ty == Type::CMPLX => fold.fold_cmplx_sub(op.span, lhs, rhs),
            BinOp::Minus if ty == Type::REAL => fold.fold_real_sub(op.span, lhs, rhs),
            BinOp::Minus if ty == Type::INT => fold.fold_int_sub(op.span, lhs, rhs),
            BinOp::Minus => unreachable!("Misstyped MIR"),

            BinOp::Multiply if ty == Type::CMPLX => fold.fold_cmplx_mul(op.span, lhs, rhs),
            BinOp::Multiply if ty == Type::REAL => fold.fold_real_mul(op.span, lhs, rhs),
            BinOp::Multiply if ty == Type::INT => fold.fold_int_mul(op.span, lhs, rhs),
            BinOp::Multiply => unreachable!("Misstyped MIR"),

            BinOp::Divide if ty == Type::CMPLX => fold.fold_cmplx_div(op.span, lhs, rhs),
            BinOp::Divide if ty == Type::REAL => fold.fold_real_div(op.span, lhs, rhs),
            BinOp::Divide if ty == Type::INT => fold.fold_int_div(op.span, lhs, rhs),
            BinOp::Divide => unreachable!("Misstyped MIR"),

            BinOp::Modulus if ty == Type::REAL => fold.fold_real_rem(op.span, lhs, rhs),
            BinOp::Modulus if ty == Type::INT => fold.fold_int_rem(op.span, lhs, rhs),
            BinOp::Modulus => unreachable!("Misstyped MIR"),

            // always integer (assuming correctly typed MIR)
            BinOp::ShiftLeft => fold.fold_shiftl(op.span, lhs, rhs),
            BinOp::ShiftRight => fold.fold_shiftr(op.span, lhs, rhs),
            BinOp::Xor if ty == Type::INT => fold.fold_xor(op.span, lhs, rhs),
            BinOp::NXor if ty == Type::INT => fold.fold_nxor(op.span, lhs, rhs),
            BinOp::And if ty == Type::INT => fold.fold_and(op.span, lhs, rhs),
            BinOp::Or if ty == Type::INT => fold.fold_or(op.span, lhs, rhs),
            BinOp::Xor if ty == Type::BOOL => fold.fold_bool_xor(op.span, lhs, rhs),
            BinOp::NXor if ty == Type::BOOL => fold.fold_bool_nxor(op.span, lhs, rhs),
            BinOp::And if ty == Type::BOOL => fold.fold_bool_and(op.span, lhs, rhs),
            BinOp::Or if ty == Type::BOOL => fold.fold_bool_or(op.span, lhs, rhs),
            BinOp::Xor => unreachable!("Misstyped MIR"),
            BinOp::NXor => unreachable!("Misstyped MIR"),
            BinOp::And => unreachable!("Misstyped MIR"),
            BinOp::Or => unreachable!("Misstyped MIR"),
        },

        RValue::Math1(kind, ref arg) => match kind.contents {
            Math1::Abs if ty == Type::CMPLX => fold.fold_cmplx_abs(kind.span, arg),
            Math1::Abs if ty == Type::REAL => fold.fold_real_abs(kind.span, arg),
            Math1::Abs if ty == Type::INT => fold.fold_int_abs(kind.span, arg),
            Math1::Abs => unreachable!("Misstyped MIR"),

            // TODO CMPLX numbers
            // always real (assuming correctly typed MIR)
            Math1::Sqrt => fold.fold_sqrt(kind.span, arg),
            Math1::Exp(lim) => fold.fold_exp(kind.span, arg, lim),
            Math1::Ln => fold.fold_ln(kind.span, arg),
            Math1::Log => fold.fold_log(kind.span, arg),
            Math1::Clog2 => fold.fold_clog2(kind.span, arg),
            Math1::Floor => fold.fold_floor(kind.span, arg),
            Math1::Ceil => fold.fold_ceil(kind.span, arg),
            Math1::Sin => fold.fold_sin(kind.span, arg),
            Math1::Cos => fold.fold_cos(kind.span, arg),
            Math1::Tan => fold.fold_tan(kind.span, arg),
            Math1::ArcSin => fold.fold_asin(kind.span, arg),
            Math1::ArcCos => fold.fold_acos(kind.span, arg),
            Math1::ArcTan => fold.fold_atan(kind.span, arg),
            Math1::SinH => fold.fold_sinh(kind.span, arg),
            Math1::CosH => fold.fold_cosh(kind.span, arg),
            Math1::TanH => fold.fold_tanh(kind.span, arg),
            Math1::ArcSinH => fold.fold_asinh(kind.span, arg),
            Math1::ArcCosH => fold.fold_acosh(kind.span, arg),
            Math1::ArcTanH => fold.fold_atanh(kind.span, arg),
        },

        RValue::Comparison(op, ref lhs, ref rhs, ty) => match op.contents {
            ComparisonOp::LessThen if ty == Type::INT => fold.fold_lt_int(op.span, lhs, rhs),
            ComparisonOp::LessThen if ty == Type::REAL => fold.fold_lt_real(op.span, lhs, rhs),
            ComparisonOp::LessThen => unreachable!("Misstyped MIR"),

            ComparisonOp::LessEqual if ty == Type::INT => fold.fold_le_int(op.span, lhs, rhs),
            ComparisonOp::LessEqual if ty == Type::REAL => fold.fold_le_real(op.span, lhs, rhs),
            ComparisonOp::LessEqual => unreachable!("Misstyped MIR"),

            ComparisonOp::GreaterThen if ty == Type::INT => fold.fold_gt_int(op.span, lhs, rhs),
            ComparisonOp::GreaterThen if ty == Type::REAL => fold.fold_gt_real(op.span, lhs, rhs),
            ComparisonOp::GreaterThen => unreachable!("Misstyped MIR"),

            ComparisonOp::GreaterEqual if ty == Type::INT => fold.fold_ge_int(op.span, lhs, rhs),
            ComparisonOp::GreaterEqual if ty == Type::REAL => fold.fold_ge_real(op.span, lhs, rhs),
            ComparisonOp::GreaterEqual => unreachable!("Misstyped MIR"),

            ComparisonOp::Equal => fold.fold_eq(op.span, lhs, rhs, ty),
            ComparisonOp::NotEqual => fold.fold_ne(op.span, lhs, rhs, ty),
        },

        RValue::Math2(kind, ref arg1, ref arg2) => match kind.contents {
            Math2::Min if ty == Type::REAL => fold.fold_real_min(kind.span, arg1, arg2),
            Math2::Min if ty == Type::INT => fold.fold_int_min(kind.span, arg1, arg2),
            Math2::Min => unreachable!("Misstyped MIR"),

            Math2::Max if ty == Type::REAL => fold.fold_real_max(kind.span, arg1, arg2),
            Math2::Max if ty == Type::INT => fold.fold_int_max(kind.span, arg1, arg2),
            Math2::Max => unreachable!("Misstyped MIR"),

            // always real (assuming correctly typed MIR)
            Math2::Pow => fold.fold_pow(kind.span, arg1, arg2),
            Math2::Hypot => fold.fold_hypot(kind.span, arg1, arg2),
            Math2::ArcTan2 => fold.fold_atan2(kind.span, arg1, arg2),
        },

        RValue::Cast(ref arg) => fold.fold_cast(arg, ty),

        RValue::Use(ref arg) => fold.fold_use(arg),
        RValue::Select(ref cond, ref arg1, ref arg2) => fold.fold_select(cond, arg1, arg2),
        RValue::Call(ref call, ref args, span) => fold.fold_call(call, args, span),
        RValue::Array(ref args, span) => fold.fold_array(args, span, ty),
    }
}

pub trait RValueFold<C: CfgFunctions> {
    /// Result of the Fold
    type T;

    // UnaryOperation

    fn fold_cmplx_arith_negate(&mut self, op: Span, arg: &COperand<C>) -> Self::T;
    fn fold_real_arith_negate(&mut self, op: Span, arg: &COperand<C>) -> Self::T;
    fn fold_bit_negate(&mut self, op: Span, arg: &COperand<C>) -> Self::T;
    fn fold_int_arith_negate(&mut self, op: Span, arg: &COperand<C>) -> Self::T;
    fn fold_logic_negate(&mut self, op: Span, arg: &COperand<C>) -> Self::T;

    // BinaryOperation

    fn fold_cmplx_add(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_cmplx_sub(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_cmplx_mul(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_cmplx_div(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    fn fold_real_add(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_real_sub(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_real_mul(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_real_div(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_real_rem(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    fn fold_int_add(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_int_sub(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_int_mul(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_int_div(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_int_rem(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    fn fold_shiftl(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_shiftr(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    fn fold_lt_real(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_le_real(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_gt_real(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_ge_real(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    fn fold_lt_int(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_le_int(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_gt_int(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_ge_int(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    fn fold_eq(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>, ty: Type) -> Self::T;
    fn fold_ne(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>, ty: Type) -> Self::T;

    fn fold_xor(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_nxor(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_and(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_or(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    fn fold_bool_xor(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_bool_nxor(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_bool_and(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;
    fn fold_bool_or(&mut self, op: Span, lhs: &COperand<C>, rhs: &COperand<C>) -> Self::T;

    // Single Arg Math

    fn fold_exp(&mut self, span: Span, arg: &COperand<C>, limit: bool) -> Self::T;
    fn fold_ln(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_log(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_clog2(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_sqrt(&mut self, span: Span, arg: &COperand<C>) -> Self::T;

    fn fold_cmplx_abs(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_real_abs(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_int_abs(&mut self, span: Span, arg: &COperand<C>) -> Self::T;

    fn fold_ceil(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_floor(&mut self, span: Span, arg: &COperand<C>) -> Self::T;

    fn fold_sin(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_cos(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_tan(&mut self, span: Span, arg: &COperand<C>) -> Self::T;

    fn fold_sinh(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_cosh(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_tanh(&mut self, span: Span, arg: &COperand<C>) -> Self::T;

    fn fold_asin(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_acos(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_atan(&mut self, span: Span, arg: &COperand<C>) -> Self::T;

    fn fold_asinh(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_acosh(&mut self, span: Span, arg: &COperand<C>) -> Self::T;
    fn fold_atanh(&mut self, span: Span, arg: &COperand<C>) -> Self::T;

    // Double arg math

    fn fold_pow(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T;
    fn fold_hypot(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T;

    fn fold_real_min(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T;
    fn fold_int_min(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T;

    fn fold_real_max(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T;
    fn fold_int_max(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T;

    fn fold_atan2(&mut self, span: Span, arg1: &COperand<C>, arg2: &COperand<C>) -> Self::T;

    /// Folds a cast from int -> real
    fn fold_cast(&mut self, arg: &COperand<C>, dst: Type) -> Self::T;

    fn fold_use(&mut self, arg: &COperand<C>) -> Self::T;
    fn fold_select(
        &mut self,
        cond: &COperand<C>,
        arg1: &COperand<C>,
        arg2: &COperand<C>,
    ) -> Self::T;

    fn fold_call(
        &mut self,
        call: &C,
        args: &IndexSlice<CallArg, [COperand<C>]>,
        span: Span,
    ) -> Self::T;

    fn fold_array(&mut self, args: &[COperand<C>], span: Span, ty: Type) -> Self::T;
}
