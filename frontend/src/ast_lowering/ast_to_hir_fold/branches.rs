/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast_lowering::ast_to_hir_fold::Statements;
use crate::ast_lowering::ast_to_hir_fold::VerilogContext;
use crate::ast_lowering::ast_to_hir_fold::{DeclarationHandler, Fold};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::Error;

use crate::diagnostic::MultiDiagnostic;
use crate::hir::BranchDeclaration;
use crate::symbol_table::SymbolDeclaration;

/// The second fold folds all branches. This requires folding of disciplines be complete and is required for expressions and statement folding
/// After this fold is complete Branches can be safely accessed from the hir
pub struct Branches<'lt, H: DeclarationHandler> {
    pub(super) branch_resolver: BranchResolver,
    pub(super) base: Fold<'lt>,
    pub(super) declaration_handler: &'lt mut H,
}
impl<'lt, H: DeclarationHandler> Branches<'lt, H> {
    pub fn fold(mut self) -> std::result::Result<Statements<'lt, H>, MultiDiagnostic<Error>> {
        for module in self.base.ast.modules.iter() {
            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

            for id in module.contents.branch_list.clone() {
                self.declaration_handler
                    .handle_declaration(&mut self.base, SymbolDeclaration::Branch(id));
                let declaration = &self.base.ast[id];
                if let Some((branch, _)) = self
                    .branch_resolver
                    .resolve_branch(&mut self.base, &declaration.contents.branch)
                {
                    self.base
                        .hir
                        .branches
                        .push(declaration.map(BranchDeclaration {
                            name: declaration.contents.name,
                            branch,
                        }));
                }
            }

            self.base.resolver.exit_scope();
        }

        if self.base.errors.is_empty() {
            Ok(Statements {
                branch_resolver: self.branch_resolver,
                state: VerilogContext::empty(),
                base: self.base,
                declaration_handler: self.declaration_handler,
            })
        } else {
            Err(self.base.errors)
        }
    }
}
