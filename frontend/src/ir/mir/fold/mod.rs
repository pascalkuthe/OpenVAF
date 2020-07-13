use crate::ir::mir::fold::integer_expressions::IntegerExprFold;
use crate::ir::mir::fold::real_expressions::RealExprFold;
use crate::ir::mir::fold::string_expressions::StringExprFold;
use crate::ir::mir::ExpressionId;

pub mod integer_expressions;
pub mod real_expressions;
pub mod string_expressions;

pub fn walk_expr<V: ExpressionFold>(fold: &mut V, expr: ExpressionId) -> <V as ExpressionFold>::T {
    match expr {
        ExpressionId::Real(expr) => fold.fold_real_expr(expr),
        ExpressionId::Integer(expr) => fold.fold_integer_expr(expr),
        ExpressionId::String(expr) => fold.fold_string_expr(expr),
    }
}

pub trait ExpressionFold:
    RealExprFold<T = <Self as ExpressionFold>::T>
    + IntegerExprFold<T = <Self as ExpressionFold>::T>
    + StringExprFold<T = <Self as ExpressionFold>::T>
{
    type T;
    fn fold_expr(&mut self, expr: ExpressionId) -> <Self as ExpressionFold>::T {
        walk_expr(self, expr)
    }
}
