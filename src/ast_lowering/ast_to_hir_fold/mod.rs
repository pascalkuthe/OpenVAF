pub(super) use branches::Branches;
pub(super) use expression::ExpressionFolder;
pub(super) use global::Global;
pub(super) use statements::Statements;

use crate::ast_lowering::error::Error;
use crate::ast_lowering::name_resolution::Resolver;
use crate::{Ast, Hir};

mod branches;
mod expression;
mod global;
mod statements;

/// An internal struct that contains data and functionality all Folds in this module share
/// It is used for abstracting over functionality/data for the [`resolve!`](VARF::ast_lowering::name_resolution::resolve)/[`resolve_hierarchical!`](VARF::ast_lowering::name_resolution::resolve_hierarchical) macros and [`BranchResolver`](VARF::ast_lowering::branch_resolution::BranchResolver)
pub struct Fold<'tag, 'lt> {
    pub resolver: Resolver<'tag, 'lt>,
    pub errors: Vec<Error<'tag>>,
    pub hir: Box<Hir<'tag>>,
    pub ast: &'lt Ast<'tag>,
}
impl<'tag, 'lt> Fold<'tag, 'lt> {
    pub fn error(&mut self, error: Error<'tag>) {
        self.errors.push(error)
    }
}

///The point of this entire Module. It lowers an AST to an HIR by resolving references, ambiguities and enforcing nature/discipline comparability
pub fn fold<'tag>(
    mut ast: Box<Ast<'tag>>,
) -> Result<Box<Hir<'tag>>, (Vec<Error<'tag>>, Box<Ast<'tag>>)> {
    try_fold(&mut ast).map_err(|err| (err, ast))
}

/// A Helper method to avoid code duplication until try blocks are stable
fn try_fold<'tag>(ast: &mut Ast<'tag>) -> Result<Box<Hir<'tag>>, Vec<Error<'tag>>> {
    Ok(Global::new(ast).fold()?.fold()?.fold()?)
}
