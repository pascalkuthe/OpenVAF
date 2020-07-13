use crate::ir::mir::visit::ExpressionVisit;
use crate::ir::mir::RealBinaryOperator;
use crate::ir::{BuiltInFunctionCall1p, BuiltInFunctionCall2p, RealExpressionId};

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
    call: BuiltInFunctionCall1p,
    arg: RealExpressionId,
) {
    match call {
        BuiltInFunctionCall1p::Sqrt => visit.visit_sqrt(arg),
        BuiltInFunctionCall1p::Exp(_) => visit.visit_exp(arg),
        BuiltInFunctionCall1p::Ln => visit.visit_ln(arg),
        BuiltInFunctionCall1p::Log => visit.visit_log(arg),
        BuiltInFunctionCall1p::Abs => visit.visit_abs(arg),
        BuiltInFunctionCall1p::Floor => visit.visit_floor(arg),
        BuiltInFunctionCall1p::Ceil => visit.visit_ceil(arg),
        BuiltInFunctionCall1p::Sin => visit.visit_sin(arg),
        BuiltInFunctionCall1p::Cos => visit.visit_cos(arg),
        BuiltInFunctionCall1p::Tan => visit.visit_tan(arg),
        BuiltInFunctionCall1p::ArcSin => visit.visit_arcsin(arg),
        BuiltInFunctionCall1p::ArcCos => visit.visit_arccos(arg),
        BuiltInFunctionCall1p::ArcTan => visit.visit_arctan(arg),
        BuiltInFunctionCall1p::SinH => visit.visit_sinh(arg),
        BuiltInFunctionCall1p::CosH => visit.visit_cosh(arg),
        BuiltInFunctionCall1p::TanH => visit.visit_tanh(arg),
        BuiltInFunctionCall1p::ArcSinH => visit.visit_arcsinh(arg),
        BuiltInFunctionCall1p::ArcCosH => visit.visit_arccosh(arg),
        BuiltInFunctionCall1p::ArcTanH => visit.visit_arctanh(arg),
    }
}

pub trait RealBuiltInFunctionCall1pVisit: Sized + ExpressionVisit {
    #[inline]
    fn visit_real_builtin_function_call_1p(
        &mut self,
        call: BuiltInFunctionCall1p,
        arg: RealExpressionId,
    ) {
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
    call: BuiltInFunctionCall2p,
    arg1: RealExpressionId,
    arg2: RealExpressionId,
) {
    match call {
        BuiltInFunctionCall2p::Pow => visit.visit_pow(arg1, arg2),
        BuiltInFunctionCall2p::Hypot => visit.visit_hypot(arg1, arg2),
        BuiltInFunctionCall2p::ArcTan2 => visit.visit_arctan2(arg1, arg2),
        BuiltInFunctionCall2p::Max => visit.visit_max(arg1, arg2),
        BuiltInFunctionCall2p::Min => visit.visit_min(arg1, arg2),
    }
}

pub trait RealBuiltInFunctionCall2pVisit: Sized + ExpressionVisit {
    fn visit_real_builtin_function_call_2p(
        &mut self,
        call: BuiltInFunctionCall2p,
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
