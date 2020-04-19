/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast_lowering::ast_to_hir_fold::expression::ConstantExpressionFolder;
use crate::ast_lowering::ast_to_hir_fold::{Branches, ExpressionFolder, Fold};
use crate::ast_lowering::branch_resolution::BranchResolver;
use crate::ast_lowering::error::{Error, Type};
use crate::ast_lowering::name_resolution::Resolver;
use crate::compact_arena::{NanoArena, TinyArena};
use crate::hir::{Discipline, Nature, Net, Port};
use crate::ir::{
    Attribute, AttributeId, DisciplineId, ExpressionId, NatureId, NetId, PortId, Write,
};
use crate::ir::{Push, SafeRangeCreation};
use crate::parser::error::Unsupported;
use crate::symbol::{keywords, Ident};
use crate::{Ast, Hir};

/// This is the first fold. All Items that are defined globally or do not reference other items (nets & ports) are folded here
pub struct Global<'tag, 'lt> {
    pub(super) base: Fold<'tag, 'lt>,
}

impl<'tag, 'lt> Global<'tag, 'lt> {
    pub fn new(ast: &'lt mut Ast<'tag>) -> Self {
        let mut res = Self {
            base: Fold {
                hir: Hir::init(ast),
                ast: &*ast,
                errors: Vec::with_capacity(32),
                resolver: Resolver::new(&*ast),
            },
        };
        res.base.resolver.enter_scope(&ast.top_symbols);
        res
    }

    pub fn fold(mut self) -> std::result::Result<Branches<'tag, 'lt>, Vec<Error<'tag>>> {
        unsafe {
            //This is save since we get the ptrs using borrows and drop is never called since they are copy
            TinyArena::init_from(&mut self.base.hir.attributes, &self.base.ast.attributes);
        }

        for attribute in SafeRangeCreation::<AttributeId<'tag>>::full_range(self.base.ast) {
            let value = self.base.ast[attribute]
                .value
                .map(|expr| self.fold_constant_expr(expr))
                .flatten();
            self.base.hir.write(
                attribute,
                Attribute {
                    name: self.base.ast[attribute].name,
                    value,
                },
            );
        }

        unsafe {
            //This is save since we get the ptrs using borrows and drop is never called since they are copy
            NanoArena::init_from(&mut self.base.hir.natures, &self.base.ast.natures);
        }

        for nature in self.base.ast.full_range() {
            self.fold_nature(nature)
        }

        unsafe {
            //This is save since we get the ptrs using borrows and drop is never called since they are copy
            NanoArena::init_from(&mut self.base.hir.disciplines, &self.base.ast.disciplines);
        }

        for discipline in self.base.ast.full_range() {
            self.fold_discipline(discipline);
        }

        unsafe {
            //This is save since we get the ptrs using borrows and drop is never called since they are copy
            TinyArena::init_from(&mut self.base.hir.nets, &self.base.ast.nets);
        }

        for net in self.base.ast.full_range() {
            self.fold_net(net);
        }
        unsafe {
            //This is save since we get the ptrs using borrows and drop is never called since they are copy
            NanoArena::init_from(&mut self.base.hir.ports, &self.base.ast.ports);
        }

        for port in self.base.ast.full_range() {
            self.fold_port(port);
        }

        if self.base.errors.is_empty() {
            Ok(Branches {
                branch_resolver: BranchResolver::new(self.base.ast),
                base: self.base,
            })
        } else {
            Err(self.base.errors)
        }
    }

    /// Folds a discipline by resolving its flow and potential natures
    /// This is currently incomplete and doesn't handle other nature properties besides its name and potential/flow natures
    fn fold_discipline(&mut self, discipline: DisciplineId<'tag>) {
        let unresolved_discipline = &self.base.ast[discipline].contents;

        let flow_nature = (&unresolved_discipline.flow_nature).and_then(|ident| {
            resolve!(self.base; ident as
                Nature(id) => {
                    return Some(id)
                }
            );
            None
        });

        let potential_nature = unresolved_discipline.potential_nature.and_then(|ident| {
            resolve!(self.base; ident as
                Nature(id) => {
                    return Some(id)
                }
            );
            None
        });

        self.base.hir.write(
            discipline,
            self.base.ast[discipline].copy_with(|old| Discipline {
                name: old.name,
                flow_nature,
                potential_nature,
                continuous: old.continuous,
            }),
        );
    }

    /// Folds a discipline by resolving its flow and potential natures
    /// This is currently incomplete and doesn't handle other nature properties besides its name and potential/flow natures
    fn fold_nature(&mut self, nature: NatureId<'tag>) {
        let unresolved_nature = &self.base.ast[nature].contents;

        let idt_nature = unresolved_nature
            .idt_nature
            .and_then(|ident| {
                resolve!(self.base; ident as
                    Nature(id) => {
                        return Some(id)
                    }
                );
                None
            })
            .unwrap_or(nature);

        let ddt_nature = unresolved_nature
            .ddt_nature
            .and_then(|ident| {
                resolve!(self.base; ident as
                    Nature(id) => {
                        return Some(id)
                    }
                );
                None
            })
            .unwrap_or(nature);

        let abstol = self.fold_constant_expr(unresolved_nature.abstol);
        let units = self.fold_constant_expr(unresolved_nature.units);

        if let (Some(abstol), Some(units)) = (abstol, units) {
            self.base.hir.write(
                nature,
                self.base.ast[nature].copy_with(|old| Nature {
                    name: old.name,
                    abstol,
                    units,
                    access: old.access,
                    idt_nature,
                    ddt_nature,
                }),
            );
        }
    }

    /// In the AST Ports are separate items. But ports are really just a special type of net.
    /// As such they get folded into a net and the id of that net plus their input/output property become the port
    fn fold_port(&mut self, port: PortId<'tag>) {
        let unresolved_port = &self.base.ast[port];
        if let Some(discipline) = self.resolve_discipline(&unresolved_port.contents.discipline) {
            let net = self.base.hir.push(unresolved_port.copy_with(|port| Net {
                name: port.name,
                discipline,
                signed: port.signed,
                net_type: port.net_type,
            }));

            self.base.hir.write(
                port,
                Port {
                    input: unresolved_port.contents.input,
                    output: unresolved_port.contents.output,
                    net,
                },
            );
        }
    }

    /// Only the discpline is resolved here the rest is just a copy
    fn fold_net(&mut self, net: NetId<'tag>) {
        let unresolved_net = &self.base.ast[net];
        if let Some(discipline) = self.resolve_discipline(&unresolved_net.contents.discipline) {
            self.base.hir.write(
                net,
                unresolved_net.copy_with(|old| Net {
                    name: old.name,
                    discipline,
                    signed: old.signed,
                    net_type: old.net_type,
                }),
            )
        }
    }

    pub fn resolve_discipline(&mut self, ident: &Ident) -> Option<DisciplineId<'tag>> {
        match ident.name {
            keywords::EMPTY_SYMBOL => {
                self.base.error(Error {
                    error_type: Type::Unsupported(Unsupported::DefaultDiscipline),
                    source: ident.span,
                });
                todo!("Implicit Disciplines are currently not supported")
            }
            _ => {
                resolve!(self.base; ident as Discipline(id) => {return Some(id)});
                None
            }
        }
    }

    pub fn fold_constant_expr(&mut self, expr: ExpressionId<'tag>) -> Option<ExpressionId<'tag>> {
        ConstantExpressionFolder {}.fold(expr, &mut self.base)
    }
}
