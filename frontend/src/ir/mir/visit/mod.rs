use crate::ir::mir::visit::integer_expressions::IntegerExprVisitor;
use crate::ir::mir::visit::real_expressions::RealExprVisitor;
use crate::ir::mir::visit::string_expressions::StringExprVisitor;
use crate::ir::mir::ExpressionId;

pub mod integer_expressions;
pub mod real_expressions;
pub mod string_expressions;

pub fn walk_expr<V: ExpressionVisitor>(
    visit: &mut V,
    expr: ExpressionId,
) -> <V as ExpressionVisitor>::T {
    match expr {
        ExpressionId::Real(expr) => visit.visit_real_expr(expr),
        ExpressionId::Integer(expr) => visit.visit_integer_expr(expr),
        ExpressionId::String(expr) => visit.visit_string_expr(expr),
    }
}

pub trait ExpressionVisitor:
    RealExprVisitor<T = <Self as ExpressionVisitor>::T>
    + IntegerExprVisitor<T = <Self as ExpressionVisitor>::T>
    + StringExprVisitor<T = <Self as ExpressionVisitor>::T>
{
    type T;
    fn visit_expr(&mut self, expr: ExpressionId) -> <Self as ExpressionVisitor>::T {
        walk_expr(self, expr)
    }
}
