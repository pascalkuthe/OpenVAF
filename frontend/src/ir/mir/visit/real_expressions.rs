use crate::ir::hir::DisciplineAccess;
use crate::ir::mir::RealBinaryOperator;
use crate::ir::{
    BranchId, BuiltInFunctionCall1p, BuiltInFunctionCall2p, IntegerExpressionId, Node, NoiseSource,
    ParameterId, RealExpressionId, StringExpressionId, VariableId,
};
use crate::mir::{Mir, RealExpression};
use crate::{Span, StringLiteral};

pub fn walk_real_expression<V: RealExprVisitor>(visit: &mut V, expr: RealExpressionId) -> V::T {
    let mir = visit.mir();
    match mir[expr].contents {
        RealExpression::Literal(val) => visit.visit_literal(val),
        RealExpression::BinaryOperator(lhs, op, rhs) => visit.visit_binary_operator(lhs, op, rhs),
        RealExpression::Negate(op, expr) => visit.visit_negate(op, expr),
        RealExpression::Condition(cond, true_expr, false_expr) => {
            visit.visit_condition(cond, true_expr, false_expr)
        }
        RealExpression::VariableReference(var) => visit.visit_variable_reference(var),
        RealExpression::ParameterReference(param) => visit.visit_parameter_reference(param),
        RealExpression::BranchAccess(discipline_access, branch, derivative_order) => {
            visit.visit_branch_access(discipline_access, branch, derivative_order)
        }
        RealExpression::Noise(noise_src, name) => visit.visit_noise(noise_src, name),
        RealExpression::BuiltInFunctionCall1p(call, arg) => {
            visit.visit_builtin_function_call_1p(call, arg)
        }
        RealExpression::BuiltInFunctionCall2p(call, arg1, arg2) => {
            visit.visit_builtin_function_call_2p(call, arg1, arg2)
        }
        RealExpression::IntegerConversion(integer_expr) => {
            visit.visit_integer_conversion(integer_expr)
        }
        RealExpression::Temperature => visit.visit_temperature(),
        RealExpression::SimParam(name, default) => visit.visit_sim_param(name, default),
    }
}

pub trait RealExprVisitor: Sized {
    type T;
    fn mir(&self) -> &Mir;

    #[inline]
    fn visit_real_expr(&mut self, expr: RealExpressionId) -> Self::T {
        walk_real_expression(self, expr)
    }

    fn visit_literal(&mut self, val: f64) -> Self::T;
    fn visit_binary_operator(
        &mut self,
        lhs: RealExpressionId,
        op: Node<RealBinaryOperator>,
        rhs: RealExpressionId,
    ) -> Self::T;
    fn visit_negate(&mut self, op: Span, arg: RealExpressionId) -> Self::T;
    fn visit_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: RealExpressionId,
        false_expr: RealExpressionId,
    ) -> Self::T;
    fn visit_variable_reference(&mut self, var: VariableId) -> Self::T;
    fn visit_parameter_reference(&mut self, param: ParameterId) -> Self::T;
    fn visit_branch_access(
        &mut self,
        discipline_accesss: DisciplineAccess,
        branch: BranchId,
        time_derivative_order: u8,
    ) -> Self::T;
    fn visit_noise(
        &mut self,
        noise_src: NoiseSource<RealExpressionId, ()>,
        name: Option<StringLiteral>,
    ) -> Self::T;
    fn visit_builtin_function_call_1p(
        &mut self,
        call: BuiltInFunctionCall1p,
        arg: RealExpressionId,
    ) -> Self::T;
    fn visit_builtin_function_call_2p(
        &mut self,
        call: BuiltInFunctionCall2p,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) -> Self::T;
    fn visit_temperature(&mut self) -> Self::T;
    fn visit_sim_param(
        &mut self,
        name: StringExpressionId,
        default: Option<RealExpressionId>,
    ) -> Self::T;
    fn visit_integer_conversion(&mut self, expr: IntegerExpressionId) -> Self::T;
}

#[inline]
pub fn walk_real_binary_operator<V: RealBinaryOperatorVisitor>(
    visit: &mut V,
    lhs: RealExpressionId,
    op: RealBinaryOperator,
    rhs: RealExpressionId,
) -> V::T {
    match op {
        RealBinaryOperator::Sum => visit.visit_sum(lhs, rhs),
        RealBinaryOperator::Subtract => visit.visit_diff(lhs, rhs),
        RealBinaryOperator::Multiply => visit.visit_mul(lhs, rhs),
        RealBinaryOperator::Divide => visit.visit_quotient(lhs, rhs),
        RealBinaryOperator::Exponent => visit.visit_pow(lhs, rhs),
        RealBinaryOperator::Modulus => visit.visit_mod(lhs, rhs),
    }
}

pub trait RealBinaryOperatorVisitor: Sized {
    type T;

    fn visit_real_binary_op(
        &mut self,
        lhs: RealExpressionId,
        op: RealBinaryOperator,
        rhs: RealExpressionId,
    ) -> Self::T {
        walk_real_binary_operator(self, lhs, op, rhs)
    }

    fn visit_sum(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_diff(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_mul(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_quotient(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_pow(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_mod(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
}

#[inline]
pub fn walk_real_builtin_function_call_1p<V: RealBuiltInFunctionCall1pVisitor>(
    visit: &mut V,
    call: BuiltInFunctionCall1p,
    arg: RealExpressionId,
) -> V::T {
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

pub trait RealBuiltInFunctionCall1pVisitor: Sized {
    type T;

    #[inline]
    fn visit_real_builtin_function_call_1p(
        &mut self,
        call: BuiltInFunctionCall1p,
        arg: RealExpressionId,
    ) -> Self::T {
        walk_real_builtin_function_call_1p(self, call, arg)
    }

    fn visit_sqrt(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_exp(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_ln(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_log(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_abs(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_floor(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_ceil(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_sin(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_cos(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_tan(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_arcsin(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_arccos(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_arctan(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_sinh(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_cosh(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_tanh(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_arcsinh(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_arccosh(&mut self, arg: RealExpressionId) -> Self::T;
    fn visit_arctanh(&mut self, arg: RealExpressionId) -> Self::T;
}

#[inline]
pub fn walk_real_builtin_function_call_2p<V: RealBuiltInFunctionCall2pVisitor>(
    visit: &mut V,
    call: BuiltInFunctionCall2p,
    arg1: RealExpressionId,
    arg2: RealExpressionId,
) -> V::T {
    match call {
        BuiltInFunctionCall2p::Pow => visit.visit_pow(arg1, arg2),
        BuiltInFunctionCall2p::Hypot => visit.visit_hypot(arg1, arg2),
        BuiltInFunctionCall2p::ArcTan2 => visit.visit_arctan2(arg1, arg2),
        BuiltInFunctionCall2p::Max => visit.visit_max(arg1, arg2),
        BuiltInFunctionCall2p::Min => visit.visit_min(arg1, arg2),
    }
}

pub trait RealBuiltInFunctionCall2pVisitor: Sized {
    type T;

    fn visit_real_builtin_function_call_2p(
        &mut self,
        call: BuiltInFunctionCall2p,
        arg1: RealExpressionId,
        arg2: RealExpressionId,
    ) -> Self::T {
        walk_real_builtin_function_call_2p(self, call, arg1, arg2)
    }

    fn visit_pow(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Self::T;
    fn visit_hypot(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Self::T;
    fn visit_arctan2(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Self::T;
    fn visit_max(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Self::T;
    fn visit_min(&mut self, arg1: RealExpressionId, arg2: RealExpressionId) -> Self::T;
}
