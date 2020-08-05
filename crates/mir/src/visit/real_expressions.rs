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
use crate::RealBinaryOperator;
use openvaf_ir::ids::RealExpressionId;
use openvaf_ir::{DoubleArgMath, SingleArgMath};

#[inline]
pub fn walk_real_binary_operator<V: RealBinaryOperatorVisit>(
    visit: &mut V,
    lhs: RealExpressionId,
    op: RealBinaryOperator,
    rhs: RealExpressionId,
) {
    match op {
        RealBinaryOperator::Sum => visit.visit_sum(lhs, rhs),
        RealBinaryOperator::Subtract => visit.visit_diff(lhs, rhs),
        RealBinaryOperator::Multiply => visit.visit_mul(lhs, rhs),
        RealBinaryOperator::Divide => visit.visit_quotient(lhs, rhs),
        RealBinaryOperator::Exponent => visit.visit_pow(lhs, rhs),
        RealBinaryOperator::Modulus => visit.visit_mod(lhs, rhs),
    }
}

pub trait RealBinaryOperatorVisit: Sized + ExpressionVisit {
    fn visit_real_binary_op(
        &mut self,
        lhs: RealExpressionId,
        op: RealBinaryOperator,
        rhs: RealExpressionId,
    ) {
        walk_real_binary_operator(self, lhs, op, rhs)
    }

    fn visit_sum(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }

    fn visit_diff(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }

    fn visit_mul(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }

    fn visit_quotient(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }

    fn visit_pow(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }

    fn visit_mod(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) {
        self.visit_real_expr(lhs);
        self.visit_real_expr(rhs);
    }
}

#[inline]
pub fn walk_real_builtin_function_call_1p<V: RealBuiltInFunctionCall1pVisit>(
    visit: &mut V,
    call: SingleArgMath,
    arg: RealExpressionId,
) {
    match call {
        SingleArgMath::Sqrt => visit.visit_sqrt(arg),
        SingleArgMath::Exp(_) => visit.visit_exp(arg),
        SingleArgMath::Ln => visit.visit_ln(arg),
        SingleArgMath::Log => visit.visit_log(arg),
        SingleArgMath::Abs => visit.visit_abs(arg),
        SingleArgMath::Floor => visit.visit_floor(arg),
        SingleArgMath::Ceil => visit.visit_ceil(arg),
        SingleArgMath::Sin => visit.visit_sin(arg),
        SingleArgMath::Cos => visit.visit_cos(arg),
        SingleArgMath::Tan => visit.visit_tan(arg),
        SingleArgMath::ArcSin => visit.visit_arcsin(arg),
        SingleArgMath::ArcCos => visit.visit_arccos(arg),
        SingleArgMath::ArcTan => visit.visit_arctan(arg),
        SingleArgMath::SinH => visit.visit_sinh(arg),
        SingleArgMath::CosH => visit.visit_cosh(arg),
        SingleArgMath::TanH => visit.visit_tanh(arg),
        SingleArgMath::ArcSinH => visit.visit_arcsinh(arg),
        SingleArgMath::ArcCosH => visit.visit_arccosh(arg),
        SingleArgMath::ArcTanH => visit.visit_arctanh(arg),
    }
}

pub trait RealBuiltInFunctionCall1pVisit: Sized + ExpressionVisit {
    #[inline]
    fn visit_real_builtin_function_call_1p(&mut self, call: SingleArgMath, arg: RealExpressionId) {
        walk_real_builtin_function_call_1p(self, call, arg)
    }

    fn visit_sqrt(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_exp(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_ln(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_log(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_abs(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_floor(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_ceil(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_sin(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_cos(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_tan(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_arcsin(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_arccos(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_arctan(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_sinh(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_cosh(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_tanh(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_arcsinh(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_arccosh(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }

    fn visit_arctanh(&mut self, arg: RealExpressionId) {
        self.visit_real_expr(arg);
    }
}

#[inline]
pub fn walk_real_builtin_function_call_2p<V: RealBuiltInFunctionCall2pVisit>(
    visit: &mut V,
    call: DoubleArgMath,
    arg1: RealExpressionId,
    arg2: RealExpressionId,
) {
    match call {
        DoubleArgMath::Pow => visit.visit_pow(arg1, arg2),
        DoubleArgMath::Hypot => visit.visit_hypot(arg1, arg2),
        DoubleArgMath::ArcTan2 => visit.visit_arctan2(arg1, arg2),
        DoubleArgMath::Max => visit.visit_max(arg1, arg2),
        DoubleArgMath::Min => visit.visit_min(arg1, arg2),
    }
}

pub trait RealBuiltInFunctionCall2pVisit: Sized + ExpressionVisit {
    fn visit_real_builtin_function_call_2p(
        &mut self,
        call: DoubleArgMath,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) {
        walk_real_builtin_function_call_2p(self, call, arg1, arg2)
    }

    fn visit_pow(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) {
        self.visit_real_expr(arg1);
        self.visit_real_expr(arg2);
    }
    fn visit_hypot(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) {
        self.visit_real_expr(arg1);
        self.visit_real_expr(arg2);
    }
    fn visit_arctan2(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) {
        self.visit_real_expr(arg1);
        self.visit_real_expr(arg2);
    }
    fn visit_max(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) {
        self.visit_real_expr(arg1);
        self.visit_real_expr(arg2);
    }
    fn visit_min(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) {
        self.visit_real_expr(arg1);
        self.visit_real_expr(arg2);
    }
}
