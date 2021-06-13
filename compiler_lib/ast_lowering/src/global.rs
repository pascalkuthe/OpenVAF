/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::ast::{Ast, DisciplineItem};
use crate::error::Error::{
    AttributeAlreadyDefined, CircularAttributeInheritance, DiscreteDisciplineHasNatures,
    IllegalNatureAttributeOverwrite, NetWithoutDiscipline, RequiredBaseNatureAttributesNotDefined,
};
use crate::error::{AttributeKind, Error, IllegalNatureAttributeOverwriteKind};
use crate::expression::{AllowedReferences, ConstantExpressionFolder};
use crate::{Branches, ExpressionFolder, Fold};
use openvaf_ast as ast;
use openvaf_ast::NatureAttribute;
use openvaf_data_structures::HashMap;
use openvaf_diagnostics::ListFormatter;
use openvaf_diagnostics::MultiDiagnostic;
use openvaf_hir::{Discipline, Nature, Net, SyntaxContextData};
use openvaf_ir::ids::{ExpressionId, NatureId, SyntaxCtx};
use openvaf_ir::{Node, Spanned};
use openvaf_session::symbols::{keywords, Ident, Symbol};
use tracing::{debug, trace_span};

/// This is the first fold. All Items that are defined globally or do not reference other items (nets & ports) are folded here
pub struct Global<'a, F: Fn(Symbol) -> AllowedReferences> {
    pub(super) base: Fold<'a, F>,
}

impl<'a, F: Fn(Symbol) -> AllowedReferences> Global<'a, F> {
    pub fn new(ast: &'a mut Ast, allowed_attribute_references: F) -> Self {
        Self {
            base: Fold::new(ast, allowed_attribute_references),
        }
    }

    pub fn fold(mut self) -> std::result::Result<Branches<'a, F>, MultiDiagnostic<Error>> {
        let span = trace_span!("Globals");
        let _enter = span.enter();
        self.base.hir.natures = self
            .base
            .ast
            .natures
            .iter_enumerated()
            .filter_map(|(id, nature)| {
                let span = trace_span!("nature", name = display(nature.contents.ident));
                let _enter = span.enter();
                self.fold_nature(id, nature)
            })
            .collect();

        self.base.hir.disciplines = self
            .base
            .ast
            .disciplines
            .iter()
            .map(|discipline| {
                let span = trace_span!("discipline", name = display(discipline.contents.ident));
                let _enter = span.enter();
                self.fold_discipline(discipline)
            })
            .collect();

        self.base.hir.nets = self
            .base
            .ast
            .nets
            .iter()
            .filter_map(|net| {
                let span = trace_span!("net", name = display(net.contents.ident));
                let _enter = span.enter();
                self.fold_net(net)
            })
            .collect();

        if self.base.errors.is_empty() {
            Ok(Branches { base: self.base })
        } else {
            Err(self.base.errors)
        }
    }

    fn fold_discipline(&mut self, discipline: &Node<ast::Discipline>) -> Discipline {
        self.base
            .enter_sctxt(discipline.span, discipline.attributes);

        let mut pot_nature: Option<Spanned<Ident>> = None;
        let mut flow_nature: Option<Spanned<Ident>> = None;
        let mut domain: Option<Spanned<bool>> = None;

        for item in &discipline.contents.items {
            match item.contents {
                DisciplineItem::Potential(nature) => {
                    if let Some(old) = pot_nature {
                        self.base.error(AttributeAlreadyDefined {
                            attribute_kind: AttributeKind::DisciplinePotential(
                                discipline.contents.ident,
                            ),
                            old: old.span,
                            new: item.span,
                        })
                    } else {
                        pot_nature = Some(Spanned {
                            contents: nature,
                            span: item.span,
                        })
                    }
                }
                DisciplineItem::Flow(nature) => {
                    if let Some(old) = flow_nature {
                        self.base.error(AttributeAlreadyDefined {
                            attribute_kind: AttributeKind::DisciplineFlow(
                                discipline.contents.ident,
                            ),
                            old: old.span,
                            new: item.span,
                        })
                    } else {
                        flow_nature = Some(Spanned {
                            contents: nature,
                            span: item.span,
                        })
                    }
                }
                DisciplineItem::Domain(continous) => {
                    if let Some(old) = domain {
                        self.base.error(AttributeAlreadyDefined {
                            attribute_kind: AttributeKind::DisciplineDomain(
                                discipline.contents.ident,
                            ),
                            old: old.span,
                            new: item.span,
                        })
                    } else {
                        domain = Some(Spanned {
                            contents: continous,
                            span: item.span,
                        })
                    }
                }
                DisciplineItem::Error => {
                    debug!("Encountered error node");
                    continue;
                }
            }
        }

        let continuous = match domain {
            Some(Spanned { contents: true, .. }) => true,
            Some(Spanned {
                contents: false,
                span,
            }) => match (pot_nature, flow_nature) {
                (Some(pot), flow) => {
                    self.base.error(DiscreteDisciplineHasNatures {
                        span: discipline.span,
                        discrete_declaration: span,
                        first_nature: pot.span,
                        second_nature: flow.map(|Spanned { span, .. }| span),
                    });

                    false
                }
                (None, Some(flow)) => {
                    self.base.error(DiscreteDisciplineHasNatures {
                        span: discipline.span,
                        discrete_declaration: span,
                        first_nature: flow.span,
                        second_nature: None,
                    });

                    false
                }
                (None, None) => false,
            },

            None if pot_nature.is_some() || flow_nature.is_some() => true,
            c => c.map_or(false, |domain| domain.contents),
        };

        let flow_nature = flow_nature.and_then(
            |Spanned {
                 contents: ident, ..
             }| resolve!(self.base; ident as Nature(id) => id),
        );

        let potential_nature = pot_nature.and_then(
            |Spanned {
                 contents: ident, ..
             }| resolve!(self.base; ident as Nature(id) => id),
        );

        Discipline {
            ident: discipline.contents.ident,
            flow_nature,
            potential_nature,
            continuous,
            sctx: self.base.exit_sctxt(),
        }
    }

    fn fold_nature(&mut self, id: NatureId, nature_node: &Node<ast::Nature>) -> Option<Nature> {
        self.base.fold_attributes(nature_node.attributes);

        let sctx = self.base.hir.syntax_ctx.push(SyntaxContextData {
            span: nature_node.span,
            attributes: nature_node.attributes,
            parent: Some(SyntaxCtx::ROOT),
        });

        let nature = &nature_node.contents;
        let mut attributes: HashMap<NatureAttribute, Spanned<ExpressionId>> =
            HashMap::with_capacity(nature.attributes.len() + 2);

        for attr in &nature.attributes {
            if let Some(attr) = attr {
                match attr.contents.0 {
                    NatureAttribute::Abstol => {
                        if let Some(parent) = nature.parent {
                            self.base.error(IllegalNatureAttributeOverwrite {
                                kind: IllegalNatureAttributeOverwriteKind::Abstol,
                                nature: nature.ident,
                                parent,
                                overwrite: attr.span,
                            });
                            continue;
                        }
                    }
                    NatureAttribute::Access => {
                        if let Some(parent) = nature.parent {
                            self.base.error(IllegalNatureAttributeOverwrite {
                                kind: IllegalNatureAttributeOverwriteKind::Access,
                                nature: nature.ident,
                                parent,
                                overwrite: attr.span,
                            });
                            continue;
                        }
                    }
                    _ => (),
                };

                if let Some(old) = attributes.insert(
                    attr.contents.0,
                    Spanned {
                        span: attr.span,
                        contents: attr.contents.1,
                    },
                ) {
                    self.base.error(AttributeAlreadyDefined {
                        attribute_kind: AttributeKind::NatureAttribute(
                            attr.contents.0,
                            nature.ident,
                        ),
                        old: old.span,
                        new: attr.span,
                    })
                }
            }
        }
        // TODO cache already processed natures to avoid repeated errors for circular inhertiance and potentiall improve performance
        // Probably not worth it because NOBODY uses this (doubt anyone outside of the committee even knowns this exists) and its pretty decent already

        // Inheritance is really rare and its not useful to make very nested inheritance
        // Therefore vector is probably faster than a bitset and can be used for a nice traceback
        let mut processed_natures = vec![nature.ident];
        let mut parent = nature.parent;

        while let Some(id) =
            parent.and_then(|parent| resolve!(self.base; parent as Nature(id) => id))
        {
            // TODO error on abstol and access overwrite
            let nature = &self.base.ast[id].contents;
            if processed_natures.contains(&nature.ident) {
                self.base.error(CircularAttributeInheritance {
                    name: nature.ident,
                    parent: parent.unwrap(),
                    trace_back: processed_natures,
                });
                break;
            } else {
                processed_natures.push(nature.ident)
            }

            for attr in &nature.attributes {
                if let Some(attr) = attr {
                    attributes.entry(attr.contents.0).or_insert(Spanned {
                        span: attr.span,
                        contents: attr.contents.1,
                    });
                }
            }
            parent = nature.parent;
        }

        let idt_nature = attributes
            .get(&NatureAttribute::AntiDerivativeNature)
            .and_then(|expr| {
                self.base.reinterpret_expression_as_identifier(
                    "idt_nature",
                    &self.base.ast[expr.contents],
                )
            })
            .and_then(|ident| resolve!(self.base; ident as Nature(id) => id))
            .unwrap_or(id);

        let ddt_nature = attributes
            .get(&NatureAttribute::DerivativeNature)
            .and_then(|expr| {
                self.base.reinterpret_expression_as_identifier(
                    "ddt_nature",
                    &self.base.ast[expr.contents],
                )
            })
            .and_then(|ident| resolve!(self.base; ident as Nature(id) => id))
            .unwrap_or(id);

        let mut missing_attributes = Vec::new();

        let abstol = if let Some(expr) = attributes.get(&NatureAttribute::Abstol) {
            self.fold_constant_expr(expr.contents)
        } else {
            // Errors only for base natures
            if nature.parent.is_none() {
                missing_attributes.push(keywords::abstol);
            }
            None
        };

        let units = if let Some(expr) = attributes.get(&NatureAttribute::Units) {
            self.fold_constant_expr(expr.contents)
        } else {
            // Errors only for base natures
            if nature.parent.is_none() {
                missing_attributes.push(keywords::units);
            }
            None
        };

        let access = if let Some(expr) = attributes.get(&NatureAttribute::Access) {
            self.base
                .reinterpret_expression_as_identifier("access", &self.base.ast[expr.contents])
        } else {
            // Errors only for base natures
            if nature.parent.is_none() {
                missing_attributes.push(keywords::access);
            }
            None
        };

        if !missing_attributes.is_empty() {
            self.base.error(RequiredBaseNatureAttributesNotDefined(
                ListFormatter::new(missing_attributes),
                nature.ident,
                nature_node.span,
            ))
        }

        let access = access?;

        self.base.resolver.insert_nature_access(access, id);

        Some(Nature {
            ident: nature.ident,
            abstol: abstol?,
            units: units?,
            access,
            idt_nature,
            ddt_nature,
            sctx,
        })
    }

    /// Only the discpline is resolved here the rest is just a copy
    fn fold_net(&mut self, net: &Node<ast::Net>) -> Option<Net> {
        let sctx = self.base.hir.syntax_ctx.push(SyntaxContextData {
            span: net.span,
            attributes: net.attributes,
            parent: None,
        });

        let discipline = if let Some(ident) = &net.contents.discipline {
            resolve!(self.base; ident as Discipline(id) => id)?
        } else {
            self.base.error(NetWithoutDiscipline(net.contents.ident));
            return None;
        };

        Some(Net {
            ident: net.contents.ident,
            discipline,
            net_type: net.contents.net_type,
            sctx,
        })
    }

    pub fn fold_constant_expr(&mut self, expr: ExpressionId) -> Option<ExpressionId> {
        ConstantExpressionFolder(AllowedReferences::None).fold(expr, &mut self.base)
    }
}
