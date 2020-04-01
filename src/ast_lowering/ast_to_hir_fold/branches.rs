use std::ops::Range;

use crate::ast;
use crate::ast::{
    AttributeNode, NumericalParameterRangeBound, NumericalParameterRangeExclude, Parameter,
    ParameterType,
};
use crate::ast_lowering::ast_to_hir_fold::Statements;
use crate::ast_lowering::ast_to_hir_fold::{ExpressionFolder, Fold};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::{Error, Type};
use crate::ast_lowering::name_resolution::Resolver;
use crate::ast_lowering::VerilogContext;
use crate::compact_arena::{NanoArena, TinyArena};
use crate::hir::BranchDeclaration;
use crate::ir::ast::Variable;
use crate::ir::{BranchId, ExpressionId, ModuleId, ParameterId, UnsafeWrite, VariableId, Write};
use crate::parser::error::Unsupported;
use crate::util::SafeRangeCreation;
use crate::{Ast, Hir};

/// The second fold folds Items that are defined on a module lvl (variables, branches) and have not been folded previously
/// After this fold is complete Branches can be safely accessed from the hir
pub struct Branches<'tag, 'lt> {
    pub(super) branch_resolver: BranchResolver<'tag, 'lt>,
    pub(super) base: Fold<'tag, 'lt>,
}
impl<'tag, 'lt> Branches<'tag, 'lt> {
    pub fn fold(mut self) -> std::result::Result<Statements<'tag, 'lt>, Vec<Error<'tag>>> {
        for module in SafeRangeCreation::<ModuleId<'tag>>::full_range(self.base.ast) {
            let module: &AttributeNode<ast::Module> = &self.base.ast[module];
            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

            unsafe {
                //This is save since we get the ptrs using borrows and drop is never called since they are copy
                NanoArena::init_from(&mut self.base.hir.branches, &self.base.ast.branches)
            }
            for branch in module.contents.branches {
                self.fold_branch_declaration(branch);
            }
            self.base.resolver.exit_scope();
        }

        if self.base.errors.is_empty() {
            Ok(Statements {
                branch_resolver: self.branch_resolver,
                state: VerilogContext::empty(),
                base: self.base,
            })
        } else {
            Err(self.base.errors)
        }
    }

    /// Just a utility method that makes folding expressions a little more ergonomic
    fn fold_expression(&mut self, expr: ExpressionId<'tag>) -> Result<ExpressionId<'tag>, ()> {
        let mut fold = ExpressionFolder {
            base: &mut self.base,
            state: VerilogContext::constant,
            branch_resolver: &mut self.branch_resolver,
        };
        fold.fold_expression(expr)
    }

    /// Folds a branch declaration (such as branch (a,b) x:) using the branch_resolver
    fn fold_branch_declaration(&mut self, branch_declaration_id: BranchId<'tag>) {
        let branch_declaration = &self.base.ast[branch_declaration_id];
        if let Ok((resolved_branch, _)) =
            BranchResolver::resolve_branch(&mut self.base, &branch_declaration.contents.branch)
        {
            self.base.hir.write(
                branch_declaration_id,
                AttributeNode {
                    attributes: branch_declaration.attributes,
                    source: branch_declaration.source,
                    contents: BranchDeclaration {
                        name: branch_declaration.contents.name,
                        branch: resolved_branch,
                    },
                },
            )
        }
    }
}
