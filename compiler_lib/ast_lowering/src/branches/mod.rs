/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::error::{Error, MockSymbolDeclaration};
use crate::Fold;
use crate::Statements;
use resolver::BranchResolver;

use crate::allowed_operations::VerilogAState;
use crate::error::Error::ReservedSymbol;
use openvaf_diagnostics::MultiDiagnostic;
use openvaf_hir::{AllowedOperations, Branch, BranchKind, SyntaxContextData};
use openvaf_ir::ids::{NodeId, PortId};
use openvaf_session::symbols::Symbol;
use tracing::trace_span;

pub mod resolver;

/// The second fold folds all branches. This requires folding of disciplines be complete and is required for expressions and statement folding
/// After this fold is complete Branches can be safely accessed from the hir
pub struct Branches<'lt, F: Fn(Symbol) -> AllowedOperations> {
    pub(super) base: Fold<'lt, F>,
}
impl<'lt, F: Fn(Symbol) -> AllowedOperations> Branches<'lt, F> {
    pub fn fold(mut self) -> std::result::Result<Statements<'lt, F>, MultiDiagnostic<Error>> {
        let span = trace_span!("branches_fold");
        let _enter = span.enter();
        let mut branch_resolver = BranchResolver::new(self.base.ast);
        for module in self.base.ast.modules.iter() {
            self.base
                .resolver
                .enter_scope(&module.contents.symbol_table);

            for id in module.contents.branches.clone() {
                let declaration = &self.base.ast[id];
                if declaration.contents.ident.is_reserved() {
                    self.base.error(ReservedSymbol {
                        decl_kind: MockSymbolDeclaration::Branch,
                        ident: declaration.contents.ident,
                    })
                }
                let sctx = self.base.hir.syntax_ctx.push(SyntaxContextData {
                    span: declaration.span,
                    attributes: declaration.attributes,
                    parent: None,
                });

                let (hi, lo) = branch_resolver
                    .resolve_branch(
                        &mut self.base,
                        &declaration.contents.hi_net,
                        declaration.contents.lo_net.as_ref(),
                    )
                    .unwrap_or((NodeId::from_raw_unchecked(0), NodeId::from_raw_unchecked(0)));

                self.base.hir.branches.push(Branch {
                    ident: declaration.contents.ident,
                    hi,
                    lo,
                    sctx,
                    kind: BranchKind::Explicit,
                    current_contributions: vec![],
                    voltage_contributions: vec![],
                    current_acccess: vec![],
                    voltage_access: vec![],
                });
            }

            for id in module.contents.port_branches.clone() {
                let ident = self.base.ast[id].contents.ident;
                self.base
                    .check_ident(ident, MockSymbolDeclaration::PortBranch);

                let ident = &self.base.ast[id].contents.port;
                self.base.fold_attributes(self.base.ast[id].attributes);
                #[allow(clippy::or_fun_call)]
                let id = resolve_hierarchical!(self.base; ident as Port(id) => id)
                    .unwrap_or(PortId::from_raw_unchecked(0));
                branch_resolver.port_branche_probes.push(id);
            }

            self.base.resolver.exit_scope();
        }

        if self.base.errors.is_empty() {
            Ok(Statements {
                branch_resolver,
                state: VerilogAState::new_analog_block(),
                base: self.base,
            })
        } else {
            Err(self.base.errors)
        }
    }
}
