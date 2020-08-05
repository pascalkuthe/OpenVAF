/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ComparisonOperator;
use crate::{IntegerBinaryOperator, IntegerExpression, Mir};
use openvaf_ir::ids::{
    IntegerExpressionId, NetId, ParameterId, PortId, RealExpressionId, StringExpressionId,
    VariableId,
};
use openvaf_ir::Spanned;
use openvaf_ir::UnaryOperator;

pub fn walk_integer_expression<V: IntegerExprFold>(
    fold: &mut V,
    expr: IntegerExpressionId,
) -> V::T {
    match fold.mir()[expr].contents {
        IntegerExpression::Literal(val) => fold.fold_literal(val),
        IntegerExpression::BinaryOperator(lhs, op, rhs) => fold.fold_binary_operator(lhs, op, rhs),
        IntegerExpression::UnaryOperator(op, expr) => fold.fold_unary_op(op, expr),
        IntegerExpression::Condition(cond, true_expr, false_expr) => {
            fold.fold_condition(cond, true_expr, false_expr)
        }
        IntegerExpression::VariableReference(var) => fold.fold_variable_reference(var),
        IntegerExpression::ParameterReference(param) => fold.fold_parameter_reference(param),
        IntegerExpression::RealCast(expr) => fold.fold_real_cast(expr),
        IntegerExpression::IntegerComparison(lhs, op, rhs) => {
            fold.fold_integer_comparison(lhs, op, rhs)
        }
        IntegerExpression::RealComparison(lhs, op, rhs) => fold.fold_real_comparison(lhs, op, rhs),
        IntegerExpression::StringEq(lhs, rhs) => fold.fold_string_eq(lhs, rhs),
        IntegerExpression::StringNEq(lhs, rhs) => fold.fold_string_neq(lhs, rhs),
        IntegerExpression::NetReference(net) => fold.fold_net_reference(net),
        IntegerExpression::PortReference(port) => fold.fold_port_reference(port),
        IntegerExpression::Min(arg1, arg2) => fold.fold_min(arg1, arg2),
        IntegerExpression::Max(arg1, arg2) => fold.fold_max(arg1, arg2),
        IntegerExpression::Abs(arg) => fold.fold_abs(arg),
        IntegerExpression::ParamGiven(param) => fold.fold_param_given(param),
        IntegerExpression::PortConnected(port) => fold.fold_port_connected(port),
    }
}

pub trait IntegerExprFold: Sized {
    type T;
    fn mir(&self) -> &Mir;

    #[inline]
    fn fold_integer_expr(&mut self, expr: IntegerExpressionId) -> Self::T {
        walk_integer_expression(self, expr)
    }

    fn fold_literal(&mut self, val: i64) -> Self::T;

    fn fold_binary_operator(
        &mut self,
        lhs: IntegerExpressionId,
        op: Spanned<IntegerBinaryOperator>,
        rhs: IntegerExpressionId,
    ) -> Self::T;

    fn fold_integer_comparison(
        &mut self,
        lhs: IntegerExpressionId,
        op: Spanned<ComparisonOperator>,
        rhs: IntegerExpressionId,
    ) -> Self::T;

    fn fold_real_comparison(
        &mut self,
        lhs: RealExpressionId,
        op: Spanned<ComparisonOperator>,
        rhs: RealExpressionId,
    ) -> Self::T;

    fn fold_unary_op(&mut self, op: Spanned<UnaryOperator>, arg: IntegerExpressionId) -> Self::T;

    fn fold_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: IntegerExpressionId,
        false_expr: IntegerExpressionId,
    ) -> Self::T;

    fn fold_min(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) -> Self::T;

    fn fold_max(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) -> Self::T;

    fn fold_abs(&mut self, arg: IntegerExpressionId) -> Self::T;

    fn fold_variable_reference(&mut self, var: VariableId) -> Self::T;

    fn fold_parameter_reference(&mut self, param: ParameterId) -> Self::T;

    fn fold_real_cast(&mut self, expr: RealExpressionId) -> Self::T;

    fn fold_port_connected(&mut self, port: PortId) -> Self::T;

    fn fold_param_given(&mut self, param: ParameterId) -> Self::T;

    fn fold_port_reference(&mut self, port: PortId) -> Self::T;

    fn fold_net_reference(&mut self, net: NetId) -> Self::T;

    fn fold_string_eq(&mut self, lhs: StringExpressionId, rhs: StringExpressionId) -> Self::T;

    fn fold_string_neq(&mut self, lhs: StringExpressionId, rhs: StringExpressionId) -> Self::T;
}

#[inline]
pub fn walk_integer_binary_operator<V: IntegerBinaryOperatorFold>(
    fold: &mut V,
    lhs: IntegerExpressionId,
    op: IntegerBinaryOperator,
    rhs: IntegerExpressionId,
) -> V::T {
    match op {
        IntegerBinaryOperator::Sum => fold.fold_sum(lhs, rhs),
        IntegerBinaryOperator::Subtract => fold.fold_diff(lhs, rhs),
        IntegerBinaryOperator::Multiply => fold.fold_mul(lhs, rhs),
        IntegerBinaryOperator::Divide => fold.fold_quotient(lhs, rhs),
        IntegerBinaryOperator::Exponent => fold.fold_pow(lhs, rhs),
        IntegerBinaryOperator::Modulus => fold.fold_mod(lhs, rhs),
        IntegerBinaryOperator::ShiftLeft => fold.fold_shiftl(lhs, rhs),
        IntegerBinaryOperator::ShiftRight => fold.fold_shiftr(lhs, rhs),
        IntegerBinaryOperator::Xor => fold.fold_xor(lhs, rhs),
        IntegerBinaryOperator::NXor => fold.fold_nxor(lhs, rhs),
        IntegerBinaryOperator::And => fold.fold_and(lhs, rhs),
        IntegerBinaryOperator::Or => fold.fold_or(lhs, rhs),
        IntegerBinaryOperator::LogicOr => fold.fold_logic_or(lhs, rhs),
        IntegerBinaryOperator::LogicAnd => fold.fold_logic_and(lhs, rhs),
    }
}

pub trait IntegerBinaryOperatorFold: Sized {
    type T;

    fn fold_integer_binary_op(
        &mut self,
        lhs: IntegerExpressionId,
        op: IntegerBinaryOperator,
        rhs: IntegerExpressionId,
    ) -> Self::T {
        walk_integer_binary_operator(self, lhs, op, rhs)
    }

    fn fold_sum(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_diff(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_mul(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_quotient(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_pow(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_mod(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_shiftl(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_shiftr(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_xor(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_nxor(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_and(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_or(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_logic_and(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_logic_or(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
}

#[inline]
pub fn walk_integer_comparison<V: IntegerComparisonFold>(
    fold: &mut V,
    lhs: IntegerExpressionId,
    op: ComparisonOperator,
    rhs: IntegerExpressionId,
) -> V::T {
    match op {
        ComparisonOperator::LessThen => fold.fold_lt(lhs, rhs),
        ComparisonOperator::LessEqual => fold.fold_le(lhs, rhs),
        ComparisonOperator::GreaterThen => fold.fold_gt(lhs, rhs),
        ComparisonOperator::GreaterEqual => fold.fold_ge(lhs, rhs),
        ComparisonOperator::LogicEqual => fold.fold_eq(lhs, rhs),
        ComparisonOperator::LogicalNotEqual => fold.fold_ne(lhs, rhs),
    }
}

pub trait IntegerComparisonFold: Sized {
    type T;

    fn fold_integer_comparison(
        &mut self,
        lhs: IntegerExpressionId,
        op: ComparisonOperator,
        rhs: IntegerExpressionId,
    ) -> Self::T {
        walk_integer_comparison(self, lhs, op, rhs)
    }

    fn fold_lt(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_le(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_gt(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_ge(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_eq(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn fold_ne(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
}

#[inline]
pub fn walk_real_comparison<V: RealComparisonFold>(
    fold: &mut V,
    lhs: RealExpressionId,
    op: ComparisonOperator,
    rhs: RealExpressionId,
) -> V::T {
    match op {
        ComparisonOperator::LessThen => fold.fold_lt(lhs, rhs),
        ComparisonOperator::LessEqual => fold.fold_le(lhs, rhs),
        ComparisonOperator::GreaterThen => fold.fold_gt(lhs, rhs),
        ComparisonOperator::GreaterEqual => fold.fold_ge(lhs, rhs),
        ComparisonOperator::LogicEqual => fold.fold_eq(lhs, rhs),
        ComparisonOperator::LogicalNotEqual => fold.fold_ne(lhs, rhs),
    }
}

pub trait RealComparisonFold: Sized {
    type T;

    fn fold_real_comparison(
        &mut self,
        lhs: RealExpressionId,
        op: ComparisonOperator,
        rhs: RealExpressionId,
    ) -> Self::T {
        walk_real_comparison(self, lhs, op, rhs)
    }

    fn fold_lt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn fold_le(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn fold_gt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn fold_ge(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn fold_eq(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn fold_ne(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
}
