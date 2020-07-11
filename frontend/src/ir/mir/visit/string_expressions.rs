use crate::ir::mir::StringExpression;
use crate::ir::{IntegerExpressionId, ParameterId, StringExpressionId, VariableId};
use crate::mir::Mir;
use crate::StringLiteral;

pub fn walk_string_expression<V: StringExprVisitor>(
    visit: &mut V,
    expr: StringExpressionId,
) -> V::T {
    let mir = visit.mir();
    match mir[expr].contents {
        StringExpression::Literal(val) => visit.visit_literal(val),
        StringExpression::VariableReference(var) => visit.visit_variable_reference(var),
        StringExpression::ParameterReference(param) => visit.visit_parameter_reference(param),
        StringExpression::Condition(cond, true_expr, false_expr) => {
            visit.visit_condition(cond, true_expr, false_expr)
        }
        StringExpression::SimParam(name) => visit.visit_string_expr(name),
    }
}

pub trait StringExprVisitor: Sized {
    type T;
    fn mir(&self) -> &Mir;

    #[inline]
    fn visit_string_expr(&mut self, expr: StringExpressionId) -> Self::T {
        walk_string_expression(self, expr)
    }

    fn visit_literal(&mut self, val: StringLiteral) -> Self::T;

    fn visit_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: StringExpressionId,
        false_expr: StringExpressionId,
    ) -> Self::T;

    fn visit_variable_reference(&mut self, var: VariableId) -> Self::T;

    fn visit_parameter_reference(&mut self, param: ParameterId) -> Self::T;

    fn visit_sim_parameter(&mut self, name: StringExpressionId) -> Self::T;
}
