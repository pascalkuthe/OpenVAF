/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

#![allow(clippy::similar_names)]

use crate::ast_lowering::ast_to_hir_fold::expression::ConstantExpressionFolder;
use crate::ast_lowering::ast_to_hir_fold::{Branches, DeclarationHandler, ExpressionFolder, Fold};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::Error;
use crate::ast_lowering::name_resolution::Resolver;

use crate::diagnostic::MultiDiagnostic;
use crate::hir::{Discipline, Nature, Net, Port};
use crate::ir::{ast, AttributeNode};
use crate::ir::{DisciplineId, ExpressionId, NatureId};
use crate::symbol::Ident;
use crate::symbol_table::SymbolDeclaration;
use crate::{Ast, Hir};

/// This is the first fold. All Items that are defined globally or do not reference other items (nets & ports) are folded here
pub struct Global<'lt, H: DeclarationHandler> {
    pub(super) base: Fold<'lt>,
    pub(super) declaration_handler: &'lt mut H,
}

impl<'lt, H: DeclarationHandler> Global<'lt, H> {
    pub fn new(ast: &'lt mut Ast, declaration_handler: &'lt mut H) -> Self {
        let mut res = Self {
            base: Fold {
                hir: Hir::init(ast),
                ast: &*ast,
                errors: MultiDiagnostic(Vec::with_capacity(32)),
                resolver: Resolver::new(&*ast),
            },
            declaration_handler,
        };
        res.base.resolver.enter_scope(&ast.top_symbols);
        res
    }

    pub fn fold(mut self) -> std::result::Result<Branches<'lt, H>, MultiDiagnostic<Error>> {
        for id in self.base.hir.attributes.indices() {
            self.base.hir[id].value = self.base.hir[id]
                .value
                .and_then(|expr| self.fold_constant_expr(expr));
        }

        self.base.hir.natures = self
            .base
            .ast
            .natures
            .iter_enumerated()
            .filter_map(|(id, nature)| {
                // callback
                self.declaration_handler
                    .handle_declaration(&mut self.base, SymbolDeclaration::Nature(id));

                self.fold_nature(id, nature)
            })
            .collect();

        self.base.hir.disciplines = self
            .base
            .ast
            .disciplines
            .iter_enumerated()
            .map(|(id, discipline)| {
                // callback
                self.declaration_handler
                    .handle_declaration(&mut self.base, SymbolDeclaration::Discipline(id));

                self.fold_discipline(discipline)
            })
            .collect();

        self.base.hir.nets = self
            .base
            .ast
            .nets
            .iter_enumerated()
            .filter_map(|(id, net)| {
                // callback
                self.declaration_handler
                    .handle_declaration(&mut self.base, SymbolDeclaration::Net(id));

                self.fold_net(net)
            })
            .collect();

        self.base.hir.ports = self
            .base
            .ast
            .ports
            .iter_enumerated()
            .filter_map(|(id, port)| {
                // callback
                self.declaration_handler
                    .handle_declaration(&mut self.base, SymbolDeclaration::Port(id));

                self.fold_port(port)
            })
            .collect();

        if self.base.errors.is_empty() {
            Ok(Branches {
                branch_resolver: BranchResolver::new(self.base.ast),
                base: self.base,
                declaration_handler: self.declaration_handler,
            })
        } else {
            Err(self.base.errors)
        }
    }

    /// Folds a discipline by resolving its flow and potential natures
    /// This is currently incomplete and doesn't handle other nature properties besides its name and potential/flow natures
    fn fold_discipline(
        &mut self,
        discipline: &AttributeNode<ast::Discipline>,
    ) -> AttributeNode<Discipline> {
        discipline.copy_with(|old| {
            let flow_nature = old.flow_nature.and_then(|ident| {
                resolve!(self.base; ident as
                    Nature(id) => {
                        return Some(id)
                    }
                );
                None
            });

            let potential_nature = old.potential_nature.and_then(|ident| {
                resolve!(self.base; ident as
                    Nature(id) => {
                        return Some(id)
                    }
                );
                None
            });
            Discipline {
                ident: old.name,
                flow_nature,
                potential_nature,
                continuous: old.continuous,
            }
        })
    }

    fn fold_nature(
        &mut self,
        id: NatureId,
        nature_node: &AttributeNode<ast::Nature>,
    ) -> Option<AttributeNode<Nature>> {
        let nature = &nature_node.contents;

        let idt_nature = nature
            .idt_nature
            .and_then(|ident| {
                resolve!(self.base; ident as
                    Nature(id) => {
                        return Some(id)
                    }
                );
                None
            })
            .unwrap_or(id);

        let ddt_nature = nature
            .ddt_nature
            .and_then(|ident| {
                resolve!(self.base; ident as
                    Nature(id) => {
                        return Some(id)
                    }
                );
                None
            })
            .unwrap_or(id);

        let abstol = self.fold_constant_expr(nature.abstol);
        let units = self.fold_constant_expr(nature.units);
        let abstol = abstol?;
        let units = units?;

        Some(self.base.ast[id].copy_with(|old| Nature {
            ident: old.name,
            abstol,
            units,
            access: old.access,
            idt_nature,
            ddt_nature,
        }))
    }

    /// In the AST Ports are separate items. But ports are really just a special type of net.
    /// As such they get folded into a net and the id of that net plus their input/output property become the port
    fn fold_port(&mut self, port: &AttributeNode<ast::Port>) -> Option<Port> {
        let discipline = self.resolve_discipline(&port.contents.discipline)?;
        let net = self.base.hir.nets.push(port.copy_with(|port| Net {
            name: port.ident,
            discipline,
            signed: port.signed,
            net_type: port.net_type,
        }));

        Some(Port {
            input: port.contents.input,
            output: port.contents.output,
            net,
        })
    }

    /// Only the discpline is resolved here the rest is just a copy
    fn fold_net(&mut self, net: &AttributeNode<ast::Net>) -> Option<AttributeNode<Net>> {
        let discipline = self.resolve_discipline(&net.contents.discipline)?;
        Some(net.copy_with(|old| Net {
            name: old.name,
            discipline,
            signed: old.signed,
            net_type: old.net_type,
        }))
    }

    pub fn resolve_discipline(&mut self, ident: &Ident) -> Option<DisciplineId> {
        resolve!(self.base; ident as Discipline(id) => {return Some(id)});
        None
    }

    pub fn fold_constant_expr(&mut self, expr: ExpressionId) -> Option<ExpressionId> {
        ConstantExpressionFolder().fold(expr, &mut self.base)
    }
}
