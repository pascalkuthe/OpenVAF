/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::visit::ExpressionVisit;
use crate::{ComparisonOperator, IntegerBinaryOperator};
use openvaf_ir::ids::{IntegerExpressionId, RealExpressionId};

#[inline]
pub fn walk_integer_binary_operator<V: IntegerBinaryOperatorVisit>(
    visit: &mut V,
    lhs: IntegerExpressionId,
    op: IntegerBinaryOperator,
    rhs: IntegerExpressionId,
) {
    match op {
        IntegerBinaryOperator::Sum => visit.visit_sum(lhs, rhs),
        IntegerBinaryOperator::Subtract => visit.visit_diff(lhs, rhs),
        IntegerBinaryOperator::Multiply => visit.visit_mul(lhs, rhs),
        IntegerBinaryOperator::Divide => visit.visit_quotient(lhs, rhs),
        IntegerBinaryOperator::Exponent => visit.visit_pow(lhs, rhs),
        IntegerBinaryOperator::Modulus => visit.visit_mod(lhs, rhs),
        IntegerBinaryOperator::ShiftLeft => visit.visit_shiftl(lhs, rhs),
        IntegerBinaryOperator::ShiftRight => visit.visit_shiftr(lhs, rhs),
        IntegerBinaryOperator::Xor => visit.visit_xor(lhs, rhs),
        IntegerBinaryOperator::NXor => visit.visit_nxor(lhs, rhs),
        IntegerBinaryOperator::And => visit.visit_and(lhs, rhs),
        IntegerBinaryOperator::Or => visit.visit_or(lhs, rhs),
        IntegerBinaryOperator::LogicOr => visit.visit_logic_or(lhs, rhs),
        IntegerBinaryOperator::LogicAnd => visit.visit_logic_and(lhs, rhs),
    }
}

pub trait IntegerBinaryOperatorVisit: ExpressionVisit {
    fn visit_integer_binary_op(
        &mut self,
        lhs: IntegerExpressionId,
        op: IntegerBinaryOperator,
        rhs: IntegerExpressionId,
    ) {
        walk_integer_binary_operator(self, lhs, op, rhs)
    }

    fn visit_sum(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_diff(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_mul(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_quotient(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_pow(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_mod(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_shiftl(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_shiftr(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_xor(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_nxor(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_and(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_or(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_logic_and(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_logic_or(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
}

#[inline]
pub fn walk_integer_comparison<V: IntegerComparisonVisit>(
    visit: &mut V,
    lhs: IntegerExpressionId,
    op: ComparisonOperator,
    rhs: IntegerExpressionId,
) {
    match op {
        ComparisonOperator::LessThen => visit.visit_lt(lhs, rhs),
        ComparisonOperator::LessEqual => visit.visit_le(lhs, rhs),
        ComparisonOperator::GreaterThen => visit.visit_gt(lhs, rhs),
        ComparisonOperator::GreaterEqual => visit.visit_ge(lhs, rhs),
        ComparisonOperator::LogicEqual => visit.visit_eq(lhs, rhs),
        ComparisonOperator::LogicalNotEqual => visit.visit_ne(lhs, rhs),
    }
}

pub trait IntegerComparisonVisit: ExpressionVisit {
    type T;

    fn visit_integer_comparison(
        &mut self,
        lhs: IntegerExpressionId,
        op: ComparisonOperator,
        rhs: IntegerExpressionId,
    ) {
        walk_integer_comparison(self, lhs, op, rhs)
    }

    fn visit_lt(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_le(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_gt(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_ge(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_eq(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
    fn visit_ne(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) {
        self.visit_integer_expr(lhs);
        self.visit_integer_expr(rhs);
    }
}

#[inline]
pub fn walk_real_comparison<V: RealComparisonVisit>(
    visit: &mut V,
    lhs: RealExpressionId,
    op: ComparisonOperator,
    rhs: RealExpressionId,
) {
    match op {
        ComparisonOperator::LessThen => visit.visit_lt(lhs, rhs),
        ComparisonOperator::LessEqual => visit.visit_le(lhs, rhs),
        ComparisonOperator::GreaterThen => visit.visit_gt(lhs, rhs),
        ComparisonOperator::GreaterEqual => visit.visit_ge(lhs, rhs),
        ComparisonOperator::LogicEqual => visit.visit_eq(lhs, rhs),
        ComparisonOperator::LogicalNotEqual => visit.visit_ne(lhs, rhs),
    }
}

pub trait RealComparisonVisit: ExpressionVisit {
    type T;

    fn visit_real_comparison(
        &mut self,
        lhs: RealExpressionId,
        op: ComparisonOperator,
        rhs: RealExpressionId,
    ) {
        walk_real_comparison(self, lhs, op, rhs)
    }

    fn visit_lt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }
    fn visit_le(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }
    fn visit_gt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }
    fn visit_ge(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }
    fn visit_eq(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }
    fn visit_ne(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }
}
