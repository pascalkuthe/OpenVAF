/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::error::Error::{
    DisciplineMismatch, EmptyBranchAccess, NatureNotPotentialOrFlow, TooManyBranchAccessArgs,
};
use crate::error::{AllowedNatures, NetInfo};
use crate::expression::AllowedReferences;
use crate::name_resolution::NatureAccessId;
use crate::Fold;
use openvaf_ast as ast;
use openvaf_ast::Ast;
use openvaf_ast::HierarchicalId;
use openvaf_ast::NetType::GROUND;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_data_structures::HashMap;
use openvaf_hir::{Branch, Net};
use openvaf_hir::{DisciplineAccess, SyntaxContextData};
use openvaf_ir::ids::{
    BranchId, DisciplineId, ExpressionId, NetId, PortBranchId, PortId, SyntaxCtx,
};
use openvaf_ir::{Attributes, Spanned};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::{keywords, Ident, Symbol};

pub enum BranchProbeKind {
    Port(PortId),
    Branch(DisciplineAccess, BranchId),
}

#[derive(Copy, Clone, Debug)]
pub enum NatureAccess {
    Flow(Span),
    Pot(Span),
    Named(NatureAccessId),
}

impl NatureAccess {
    pub fn resolve_from_ident<F>(ident: Ident, fold: &mut Fold<F>) -> Option<Self>
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        Some(match ident.name {
            keywords::potential => NatureAccess::Pot(ident.span),
            keywords::flow => NatureAccess::Flow(ident.span),
            _ => {
                let id = resolve!(fold; ident as NatureAccess(id) => id)?;
                Self::Named(id)
            }
        })
    }
    fn into_ident<F>(self, fold: &Fold<F>) -> Ident
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        match self {
            Self::Flow(span) => Ident::new(keywords::flow, span),
            Self::Pot(span) => Ident::new(keywords::potential, span),
            Self::Named(id) => fold.resolver.get_nature_access_ident(id),
        }
    }
}

/// Handles branch resolution which is more complicated because unnamed branches exist and discipline comparability has to be enforced
pub struct BranchResolver {
    unnamed_branches: HashMap<(NetId, NetId), BranchId>,
    implicit_grounds: HashMap<DisciplineId, NetId>,
    pub(super) port_branches: IndexVec<PortBranchId, PortId>,
}

impl BranchResolver {
    #[must_use]
    pub(super) fn new(ast: &Ast) -> Self {
        Self {
            unnamed_branches: HashMap::with_capacity(32),
            implicit_grounds: HashMap::with_capacity(ast.disciplines.len()),
            port_branches: IndexVec::with_capacity(ast.port_branches.len()),
        }
    }

    /// Resolves a DisciplineAccess (for example `V(b)` or `V(x,y)`)    
    pub fn resolve_discipline_access<F>(
        fold: &mut Fold<F>,
        access: NatureAccess,
        discipline: DisciplineId,
    ) -> Option<DisciplineAccess>
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        match access {
            NatureAccess::Named(id)
                if fold
                    .resolver
                    .get_nature_access(id)
                    .iter()
                    .any(|&x| Some(x) == fold.hir[discipline].flow_nature) =>
            {
                Some(DisciplineAccess::Flow)
            }
            NatureAccess::Named(id)
                if fold
                    .resolver
                    .get_nature_access(id)
                    .iter()
                    .any(|&x| Some(x) == fold.hir[discipline].potential_nature) =>
            {
                Some(DisciplineAccess::Potential)
            }
            NatureAccess::Pot(_) if fold.hir[discipline].potential_nature.is_some() => {
                Some(DisciplineAccess::Potential)
            }
            NatureAccess::Flow(_) if fold.hir[discipline].flow_nature.is_some() => {
                Some(DisciplineAccess::Flow)
            }
            _ => {
                fold.error(NatureNotPotentialOrFlow {
                    nature: access.into_ident(&fold),
                    allowed_natures: AllowedNatures::from_discipline(discipline, &fold.hir),
                });
                None
            }
        }
    }

    /// Resolves a DisciplineAccess (for example `V(b)` or `V(x,y)`)    
    pub fn check_port_discipline_access<F>(
        fold: &mut Fold<F>,
        access: NatureAccess,
        discipline: DisciplineId,
    ) where
        F: Fn(Symbol) -> AllowedReferences,
    {
        match access {
            NatureAccess::Named(id)
                if fold
                    .resolver
                    .get_nature_access(id)
                    .iter()
                    .any(|&x| Some(x) == fold.hir[discipline].flow_nature) => {}

            NatureAccess::Flow(_) if fold.hir[discipline].flow_nature.is_some() => (),
            _ => {
                fold.error(NatureNotPotentialOrFlow {
                    nature: access.into_ident(&fold),
                    allowed_natures: AllowedNatures::from_port_discipline(discipline, &fold.hir),
                });
            }
        }
    }

    /// Returns the implicit ground net for `discipline`. Creates one if none is known yet
    pub fn implicit_ground_net<F>(&mut self, discipline: DisciplineId, fold: &mut Fold<F>) -> NetId
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        *self.implicit_grounds.entry(discipline).or_insert_with(|| {
            let sctx = fold.hir.syntax_ctx.push(SyntaxContextData {
                span: fold.ast[discipline].span,
                attributes: Attributes::EMPTY,
                parent: Some(SyntaxCtx::ROOT),
            });

            fold.hir.nets.push(Net {
                ident: Ident::from_str("implicit_ground"),
                discipline,
                net_type: GROUND,
                sctx,
            })
        })
    }

    /// Creates an unnamed branch from `net` to the implict ground node
    pub fn unnamed_branch_to_ground<F>(
        &mut self,
        span: Span,
        net: NetId,
        fold: &mut Fold<F>,
    ) -> BranchId
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        let ground = self.implicit_ground_net(fold.hir[net].discipline, fold);
        self.unnamed_branch(span, net, ground, fold)
    }

    /// Returns the `BranchId` for the unnamed `branch`. If the branch does not yet exist it is created
    ///
    /// # Note
    ///
    /// This should only be called with `hi`/`lo` that have been [checked](crate::branches::resolver::BranchResolver::unnamed_branch)
    pub fn unnamed_branch<F>(
        &mut self,
        span: Span,
        hi: NetId,
        lo: NetId,
        fold: &mut Fold<F>,
    ) -> BranchId
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        *self.unnamed_branches.entry((hi, lo)).or_insert_with(|| {
            let name = format!("( {} , {} )", fold.hir[hi].ident, fold.hir[lo].ident);
            let ident = Ident::from_str_and_span(&name, span);

            let declaration = Branch {
                ident,
                hi,
                lo,
                sctx: fold.hir.syntax_ctx.push(SyntaxContextData {
                    span,
                    attributes: Attributes::EMPTY,
                    parent: None,
                }),
            };

            fold.hir.branches.push(declaration)
        })
    }

    /// Checks whether a branch is valid.
    /// A branch between two nets (flow trough a port can't be invalid) is invalid if the disciplines of the nets are incompataible.
    ///
    /// # Note
    ///
    /// OpenVAF does currently not implement "proper" discipline comparability check as defined in the standard
    /// Instead if simply checks that the disciplines are equal because this feature is not used in compact models in practice
    pub fn check_branch<F>(net1: NetId, net2: NetId, span: Span, fold: &mut Fold<F>)
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        let net1 = &fold.hir[net1];
        let net2 = &fold.hir[net2];
        if net1.discipline != net2.discipline {
            let discipline1 = fold.hir[net1.discipline].ident.name;

            let discipline2 = fold.hir[net2.discipline].ident.name;

            let err = DisciplineMismatch(
                NetInfo {
                    discipline: discipline1,
                    name: net1.ident.name,
                    declaration: fold.hir[net1.sctx].span,
                },
                NetInfo {
                    discipline: discipline2,
                    name: net2.ident.name,
                    declaration: fold.hir[net2.sctx].span,
                },
                span,
            );

            fold.errors.add(err);
        }
    }

    pub fn resolve_branch_probe_call<F>(
        &mut self,
        nature: NatureAccess,
        arguments: &[ExpressionId],
        fold: &mut Fold<F>,
    ) -> Option<BranchProbeKind>
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        self.handle_branch_probe_args(
            nature.into_ident(&fold),
            arguments,
            |resolver, fold, net, span| {
                let branch = resolver.unnamed_branch_to_ground(span, net, fold);
                let discipline = fold.hir[net].discipline;
                let access = Self::resolve_discipline_access(fold, nature, discipline)?;
                Some(BranchProbeKind::Branch(access, branch))
            },
            |resolver, fold, hi, lo, span| {
                Self::check_branch(hi, lo, span, fold);
                let branch = resolver.unnamed_branch(span, hi, lo, fold);
                let discipline = fold.hir[hi].discipline;
                let access = Self::resolve_discipline_access(fold, nature, discipline)?;
                Some(BranchProbeKind::Branch(access, branch))
            },
            |_, fold, branch, _| {
                let discipline = fold.hir[fold.hir[branch].hi].discipline;
                let access = Self::resolve_discipline_access(fold, nature, discipline)?;
                Some(BranchProbeKind::Branch(access, branch))
            },
            |_, fold, port, _span| {
                let discipline = fold.hir[fold.hir[port].net].discipline;
                Self::check_port_discipline_access(fold, nature, discipline);
                Some(BranchProbeKind::Port(port))
            },
            fold,
        )
        .flatten()
    }

    /// Checks that valid arguments were passed to a branch probe and calls the appropriate function on success
    ///
    /// # Returns
    /// The result of call `handle_node`/`handle_unnamed_branch`/`handle_branch` on success
    /// `None` otherwise
    #[allow(clippy::too_many_arguments)]
    pub fn handle_branch_probe_args<'a, F, T>(
        &mut self,
        nature_ident: Ident,
        arguments: &[ExpressionId],
        handle_node: impl FnOnce(&mut Self, &mut Fold<'a, F>, NetId, Span) -> T,
        handle_unnamed_branch: impl FnOnce(&mut Self, &mut Fold<'a, F>, NetId, NetId, Span) -> T,
        handle_branch: impl FnOnce(&mut Self, &mut Fold<'a, F>, BranchId, Span) -> T,
        handle_port: impl FnOnce(&mut Self, &mut Fold<'a, F>, PortId, Span) -> T,
        fold: &mut Fold<'a, F>,
    ) -> Option<T>
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        match arguments {
            [branch] => {
                let expr = &fold.ast[*branch];
                let ident = Self::reinterpret_expression_as_identifier(expr, fold)?;

                resolve_hierarchical!( fold;
                    ident as
                        Net(id)    => handle_node(self, fold, id, expr.span),
                        Port(id)   => handle_node(self, fold, fold.hir[id].net, expr.span),
                        Branch(id) => handle_branch(self, fold, id, expr.span),
                        PortBranch(id) => {
                            handle_port(self, fold, self.port_branches[id],expr.span)
                        }
                )
            }

            [net1, net2] => {
                let net1 = &fold.ast[*net1];
                let net2 = &fold.ast[*net2];

                let span1 = net1.span;
                let span2 = net2.span;

                let net1 = Self::reinterpret_expression_as_identifier(net1, fold)?;
                let net1 = resolve_hierarchical!(fold;
                    net1 as
                        Net(id) => id,
                        Port(id)   => fold.hir[id].net

                );

                let net2 = Self::reinterpret_expression_as_identifier(net2, fold)?;
                let net2 = resolve_hierarchical!(fold;
                    net2 as
                        Net(id) => id,
                        Port(id)   => fold.hir[id].net
                );

                if let (Some(net1), Some(net2)) = (net1, net2) {
                    Some(handle_unnamed_branch(
                        self,
                        fold,
                        net1,
                        net2,
                        span1.extend(span2),
                    ))
                } else {
                    None
                }
            }

            [] => {
                fold.error(EmptyBranchAccess {
                    nature: nature_ident.name,
                    span: nature_ident.span,
                });
                None
            }

            [_, _, unexpected, ..] => {
                fold.error(TooManyBranchAccessArgs {
                    nature: nature_ident.name,
                    span: fold.ast[*unexpected]
                        .span
                        .extend(fold.ast[*arguments.last().unwrap()].span),
                });
                None
            }
        }
    }

    fn reinterpret_expression_as_identifier<'a, F>(
        expression: &'a Spanned<ast::Expression>,
        base: &mut Fold<'a, F>,
    ) -> Option<&'a HierarchicalId>
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        base.reinterpret_expression_as_hierarchical_identifier(
            "Branch probe function calls",
            expression,
        )
    }

    /// Resolves a branch declaration such as `(NET1)`or `(NET1,NET2)` or `(<PORT>)` for further processing
    ///
    /// # Arguments
    ///
    /// * fold - The calling fold which is used for name resolution and error handling
    ///
    /// * branch - An Ast node describing a branch

    pub fn resolve_branch<'a, F>(
        &mut self,
        fold: &mut Fold<'a, F>,
        hi_ident: &'a HierarchicalId,
        lo_ident: Option<&'a HierarchicalId>,
    ) -> Option<(NetId, NetId)>
    where
        F: Fn(Symbol) -> AllowedReferences,
    {
        let hi = resolve_hierarchical!(fold; hi_ident as
            Net(id) => id,
            Port(id) => fold.hir[id].net
        );

        let (lo, span) = if let Some(lo_ident) = lo_ident {
            let span = hi_ident.span().extend(lo_ident.span());
            let lo = resolve_hierarchical!(fold; lo_ident as
                Net(id) => id,
                Port(id) => fold.hir[id].net
            );
            (lo, span)
        } else {
            (None, hi_ident.span())
        };

        let hi = hi?;

        let lo = if let Some(lo) = lo {
            Self::check_branch(hi, lo, span, fold);
            lo
        } else {
            self.implicit_ground_net(fold.hir[hi].discipline, fold)
        };

        Some((hi, lo))
    }
}
