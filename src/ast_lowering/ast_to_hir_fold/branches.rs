/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast_lowering::ast_to_hir_fold::Fold;
use crate::ast_lowering::ast_to_hir_fold::Statements;
use crate::ast_lowering::ast_to_hir_fold::VerilogContext;
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::Error;
use crate::compact_arena::NanoArena;
use crate::hir::BranchDeclaration;
use crate::ir::{BranchId, ModuleId, Write};
use crate::ir::{SafeRangeCreation, VariableId};

/// The second fold folds all branches. This requires folding of disciplines be complete and is required for expressions and statement folding
/// After this fold is complete Branches can be safely accessed from the hir
pub struct Branches<
    'tag,
    'lt,
    V: FnMut(VariableId<'tag>, &mut Fold, &VerilogContext, &BranchResolver),
> {
    pub(super) branch_resolver: BranchResolver<'tag>,
    pub(super) base: Fold<'tag, 'lt>,
    pub(super) on_variable_declaration: V,
}
impl<'tag, 'lt, V: FnMut(VariableId<'tag>, &mut Fold, &VerilogContext, &BranchResolver)>
    Branches<'tag, 'lt, V>
{
    pub fn fold(mut self) -> std::result::Result<Statements<'tag, 'lt, V>, Vec<Error<'tag>>> {
        for module in SafeRangeCreation::<ModuleId<'tag>>::full_range(self.base.ast) {
            let module = &self.base.ast[module];
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
                on_variable_declaration: self.on_variable_declaration,
            })
        } else {
            Err(self.base.errors)
        }
    }

    /// Folds a branch declaration (such as branch (a,b) x:) using the branch_resolver
    fn fold_branch_declaration(&mut self, branch_declaration_id: BranchId<'tag>) {
        let branch_declaration = &self.base.ast[branch_declaration_id];
        if let Some((resolved_branch, _)) = self
            .branch_resolver
            .resolve_branch(&mut self.base, &branch_declaration.contents.branch)
        {
            self.base.hir.write(
                branch_declaration_id,
                branch_declaration.map(BranchDeclaration {
                    name: branch_declaration.contents.name,
                    branch: resolved_branch,
                }),
            )
        }
    }
}
