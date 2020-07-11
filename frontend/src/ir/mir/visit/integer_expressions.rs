use crate::ir::ast::UnaryOperator;
use crate::ir::mir::ComparisonOperator;
use crate::ir::{
    IntegerExpressionId, NetId, Node, ParameterId, PortId, RealExpressionId, StringExpressionId,
    VariableId,
};
use crate::mir::{IntegerBinaryOperator, IntegerExpression, Mir};

pub fn walk_integer_expression<V: IntegerExprVisitor>(
    visit: &mut V,
    expr: IntegerExpressionId,
) -> V::T {
    match visit.mir()[expr].contents {
        IntegerExpression::Literal(val) => visit.visit_literal(val),
        IntegerExpression::BinaryOperator(lhs, op, rhs) => {
            visit.visit_binary_operator(lhs, op, rhs)
        }
        IntegerExpression::UnaryOperator(op, expr) => visit.visit_unary_op(op, expr),
        IntegerExpression::Condition(cond, true_expr, false_expr) => {
            visit.visit_condition(cond, true_expr, false_expr)
        }
        IntegerExpression::VariableReference(var) => visit.visit_variable_reference(var),
        IntegerExpression::ParameterReference(param) => visit.visit_parameter_reference(param),
        IntegerExpression::RealCast(expr) => visit.visit_real_cast(expr),
        IntegerExpression::IntegerComparison(lhs, op, rhs) => {
            visit.visit_integer_comparison(lhs, op, rhs)
        }
        IntegerExpression::RealComparison(lhs, op, rhs) => {
            visit.visit_real_comparison(lhs, op, rhs)
        }
        IntegerExpression::StringEq(lhs, rhs) => visit.visit_string_eq(lhs, rhs),
        IntegerExpression::StringNEq(lhs, rhs) => visit.visit_string_neq(lhs, rhs),
        IntegerExpression::NetReference(net) => visit.visit_net_reference(net),
        IntegerExpression::PortReference(port) => visit.visit_port_reference(port),
        IntegerExpression::Min(arg1, arg2) => visit.visit_min(arg1, arg2),
        IntegerExpression::Max(arg1, arg2) => visit.visit_max(arg1, arg2),
        IntegerExpression::Abs(arg) => visit.visit_abs(arg),
        IntegerExpression::ParamGiven(param) => visit.visit_param_given(param),
        IntegerExpression::PortConnected(port) => visit.visit_port_connected(port),
    }
}

pub trait IntegerExprVisitor: Sized {
    type T;
    fn mir(&self) -> &Mir;

    #[inline]
    fn visit_integer_expr(&mut self, expr: IntegerExpressionId) -> Self::T {
        walk_integer_expression(self, expr)
    }

    fn visit_literal(&mut self, val: i64) -> Self::T;

    fn visit_binary_operator(
        &mut self,
        lhs: IntegerExpressionId,
        op: Node<IntegerBinaryOperator>,
        rhs: IntegerExpressionId,
    ) -> Self::T;

    fn visit_integer_comparison(
        &mut self,
        lhs: IntegerExpressionId,
        op: Node<ComparisonOperator>,
        rhs: IntegerExpressionId,
    ) -> Self::T;

    fn visit_real_comparison(
        &mut self,
        lhs: RealExpressionId,
        op: Node<ComparisonOperator>,
        rhs: RealExpressionId,
    ) -> Self::T;

    fn visit_unary_op(&mut self, op: Node<UnaryOperator>, arg: IntegerExpressionId) -> Self::T;

    fn visit_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: IntegerExpressionId,
        false_expr: IntegerExpressionId,
    ) -> Self::T;

    fn visit_min(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) -> Self::T;

    fn visit_max(&mut self, arg1: IntegerExpressionId, arg2: IntegerExpressionId) -> Self::T;

    fn visit_abs(&mut self, arg: IntegerExpressionId) -> Self::T;

    fn visit_variable_reference(&mut self, var: VariableId) -> Self::T;

    fn visit_parameter_reference(&mut self, param: ParameterId) -> Self::T;

    fn visit_real_cast(&mut self, expr: RealExpressionId) -> Self::T;

    fn visit_port_connected(&mut self, port: PortId) -> Self::T;

    fn visit_param_given(&mut self, param: ParameterId) -> Self::T;

    fn visit_port_reference(&mut self, port: PortId) -> Self::T;

    fn visit_net_reference(&mut self, net: NetId) -> Self::T;

    fn visit_string_eq(&mut self, lhs: StringExpressionId, rhs: StringExpressionId) -> Self::T;

    fn visit_string_neq(&mut self, lhs: StringExpressionId, rhs: StringExpressionId) -> Self::T;
}

#[inline]
pub fn walk_integer_binary_operator<V: IntegerBinaryOperatorVisitor>(
    visit: &mut V,
    lhs: IntegerExpressionId,
    op: IntegerBinaryOperator,
    rhs: IntegerExpressionId,
) -> V::T {
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

pub trait IntegerBinaryOperatorVisitor: Sized {
    type T;

    fn visit_integer_binary_op(
        &mut self,
        lhs: IntegerExpressionId,
        op: IntegerBinaryOperator,
        rhs: IntegerExpressionId,
    ) -> Self::T {
        walk_integer_binary_operator(self, lhs, op, rhs)
    }

    fn visit_sum(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_diff(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_mul(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_quotient(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_pow(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_mod(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_shiftl(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_shiftr(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_xor(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_nxor(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_and(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_or(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_logic_and(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_logic_or(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
}

#[inline]
pub fn walk_integer_comparison<V: IntegerComparisonVisit>(
    visit: &mut V,
    lhs: IntegerExpressionId,
    op: ComparisonOperator,
    rhs: IntegerExpressionId,
) -> V::T {
    match op {
        ComparisonOperator::LessThen => visit.visit_lt(lhs, rhs),
        ComparisonOperator::LessEqual => visit.visit_le(lhs, rhs),
        ComparisonOperator::GreaterThen => visit.visit_gt(lhs, rhs),
        ComparisonOperator::GreaterEqual => visit.visit_ge(lhs, rhs),
        ComparisonOperator::LogicEqual => visit.visit_eq(lhs, rhs),
        ComparisonOperator::LogicalNotEqual => visit.visit_ne(lhs, rhs),
    }
}

pub trait IntegerComparisonVisit: Sized {
    type T;

    fn visit_integer_comparison(
        &mut self,
        lhs: IntegerExpressionId,
        op: ComparisonOperator,
        rhs: IntegerExpressionId,
    ) -> Self::T {
        walk_integer_comparison(self, lhs, op, rhs)
    }

    fn visit_lt(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_le(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_gt(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_ge(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_eq(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
    fn visit_ne(&mut self, lhs: IntegerExpressionId, rhs: IntegerExpressionId) -> Self::T;
}

#[inline]
pub fn walk_real_comparison<V: RealComparisonVisit>(
    visit: &mut V,
    lhs: RealExpressionId,
    op: ComparisonOperator,
    rhs: RealExpressionId,
) -> V::T {
    match op {
        ComparisonOperator::LessThen => visit.visit_lt(lhs, rhs),
        ComparisonOperator::LessEqual => visit.visit_le(lhs, rhs),
        ComparisonOperator::GreaterThen => visit.visit_gt(lhs, rhs),
        ComparisonOperator::GreaterEqual => visit.visit_ge(lhs, rhs),
        ComparisonOperator::LogicEqual => visit.visit_eq(lhs, rhs),
        ComparisonOperator::LogicalNotEqual => visit.visit_ne(lhs, rhs),
    }
}

pub trait RealComparisonVisit: Sized {
    type T;

    fn visit_real_comparison(
        &mut self,
        lhs: RealExpressionId,
        op: ComparisonOperator,
        rhs: RealExpressionId,
    ) -> Self::T {
        walk_real_comparison(self, lhs, op, rhs)
    }

    fn visit_lt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_le(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_gt(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_ge(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_eq(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
    fn visit_ne(&mut self, lhs: RealExpressionId, rhs: RealExpressionId) -> Self::T;
}
