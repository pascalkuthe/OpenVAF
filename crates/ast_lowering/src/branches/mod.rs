/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::error::Error;
use crate::Fold;
use crate::Statements;
use crate::VerilogContext;
use resolver::BranchResolver;

use openvaf_diagnostics::MultiDiagnostic;
use openvaf_hir::Branch;
use openvaf_ir::ids::PortId;

pub mod resolver;

/// The second fold folds all branches. This requires folding of disciplines be complete and is required for expressions and statement folding
/// After this fold is complete Branches can be safely accessed from the hir
pub struct Branches<'lt> {
    pub(super) base: Fold<'lt>,
}
impl<'lt> Branches<'lt> {
    pub fn fold(mut self) -> std::result::Result<Statements<'lt>, MultiDiagnostic<Error>> {
        let mut branch_resolver = BranchResolver::new(self.base.ast);
        for module in self.base.ast.modules.iter() {
            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

            for id in module.contents.branches.clone() {
                let declaration = &self.base.ast[id];
                if let Some((hi, lo)) = branch_resolver.resolve_branch(
                    &mut self.base,
                    &declaration.contents.hi_net,
                    declaration.contents.lo_net.as_ref(),
                ) {
                    self.base.fold_attributes(declaration.attributes);
                    debug_assert_eq!(self.base.hir.branches.len_idx(), id);
                    self.base.hir.branches.push(declaration.map(Branch {
                        ident: declaration.contents.ident,
                        hi,
                        lo,
                    }));
                }
            }

            for id in module.contents.port_branches.clone() {
                debug_assert_eq!(branch_resolver.port_branches.len_idx(), id);

                let ident = &self.base.ast[id].contents.port;
                self.base.fold_attributes(self.base.ast[id].attributes);
                #[allow(clippy::or_fun_call)]
                let id = resolve_hierarchical!(self.base; ident as Port(id) => id)
                    .unwrap_or(PortId::from_raw_unchecked(0));
                branch_resolver.port_branches.push(id);
            }

            self.base.resolver.exit_scope();
        }

        if self.base.errors.is_empty() {
            Ok(Statements {
                branch_resolver,
                state: VerilogContext::empty(),
                base: self.base,
            })
        } else {
            Err(self.base.errors)
        }
    }
}
