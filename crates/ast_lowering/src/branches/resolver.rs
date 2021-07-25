/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::error::Error::{
    DisciplineMismatch, EmptyBranchAccess, NatureNotPotentialOrFlow, TooManyBranchAccessArgs,
};
use crate::error::{AllowedNatures, NetInfo};
use crate::name_resolution::NatureAccessId;
use crate::Fold;
use ast as ast;
use ast::Ast;
use ast::HierarchicalId;
use data_structures::{
    index_vec::{index_vec, IndexSlice, IndexVec},
    HashMap,
};
use hir::Branch;
use hir::{AllowedOperations, Node};
use hir::{BranchKind, SyntaxContextData};
use ir::ids::{
    BranchId, CallArg, DisciplineId, ExpressionId, NodeId, PortBranchId, PortId,
};
use ir::{Attributes, DisciplineAccess, Spanned};
use session::sourcemap::Span;
use session::symbols::{kw, Ident, Symbol};

#[derive(Copy, Clone, Debug)]
pub enum NatureAccess {
    Flow(Span),
    Pot(Span),
    Named(NatureAccessId),
}

impl NatureAccess {
    pub fn resolve_from_ident<F>(ident: Ident, fold: &mut Fold<F>) -> Option<Self>
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        Some(match ident.name {
            kw::potential => NatureAccess::Pot(ident.span),
            kw::flow => NatureAccess::Flow(ident.span),
            _ => {
                let id = resolve!(fold; ident as NatureAccess(id) => id)?;
                Self::Named(id)
            }
        })
    }
    fn into_ident<F>(self, fold: &Fold<F>) -> Ident
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        match self {
            Self::Flow(span) => Ident::new(kw::flow, span),
            Self::Pot(span) => Ident::new(kw::potential, span),
            Self::Named(id) => fold.resolver.get_nature_access_ident(id),
        }
    }
}

/// Handles branch resolution which is more complicated because unnamed branches exist and discipline comparability has to be enforced
pub struct BranchResolver {
    unnamed_branches: HashMap<(NodeId, NodeId), BranchId>,
    pub(super) port_branche_probes: IndexVec<PortBranchId, PortId>,
    port_branches: IndexVec<PortId, Option<BranchId>>,
}

impl BranchResolver {
    #[must_use]
    pub(super) fn new(ast: &Ast) -> Self {
        Self {
            unnamed_branches: HashMap::with_capacity(32),
            port_branche_probes: IndexVec::with_capacity(ast.port_branches.len()),
            port_branches: index_vec![None;ast.ports.len()],
        }
    }

    /// Resolves a DisciplineAccess (for example `V(b)` or `V(x,y)`)    
    pub fn resolve_discipline_access<F>(
        fold: &mut Fold<F>,
        access: NatureAccess,
        discipline: DisciplineId,
    ) -> Option<DisciplineAccess>
    where
        F: Fn(Symbol) -> AllowedOperations,
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
    pub fn resolve_port_access<F: Fn(Symbol) -> AllowedOperations>(
        &mut self,
        fold: &mut Fold<F>,
        access: NatureAccess,
        discipline: DisciplineId,
        port: PortId,
    ) -> BranchId {
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

        match self.port_branches[port] {
            None => {
                let port_info = fold.hir[fold.hir[port].node];
                let probe_node = fold.hir.nodes.push(Node {
                    ident: Ident::from_str_and_span(
                        &format!("{}'", port_info.ident),
                        port_info.ident.span,
                    ),
                    discipline: port_info.discipline,
                    sctx: port_info.sctx,
                });
                fold.hir.branches.push(Branch {
                    ident: Ident::from_str_and_span(
                        &format!("port_flow(<{}>)", port_info.ident),
                        port_info.ident.span,
                    ),
                    hi: probe_node,
                    lo: fold.hir[port].node,
                    sctx: port_info.sctx,
                    kind: BranchKind::PortBranch,
                    current_contributions: vec![],
                    voltage_contributions: vec![],
                    current_acccess: vec![],
                    voltage_access: vec![],
                })
            }
            Some(branch) => branch,
        }
    }

    /// Creates an unnamed branch from `net` to the implict ground node
    pub fn unnamed_branch_to_ground<F>(
        &mut self,
        span: Span,
        net: NodeId,
        fold: &mut Fold<F>,
    ) -> BranchId
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        self.unnamed_branch(span, net, NodeId::from_raw_unchecked(0), fold, true)
    }

    /// Returns the `BranchId` for the unnamed `branch`. If the branch does not yet exist it is created
    ///
    /// # Note
    ///
    /// This should only be called with `hi`/`lo` that have been [checked](crate::branches::resolver::BranchResolver::check_branch)
    pub fn unnamed_branch<F>(
        &mut self,
        span: Span,
        hi: NodeId,
        lo: NodeId,
        fold: &mut Fold<F>,
        to_gnd: bool,
    ) -> BranchId
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        *self.unnamed_branches.entry((hi, lo)).or_insert_with(|| {
            let name = format!("( {} , {} )", fold.hir[hi].ident, fold.hir[lo].ident);
            let ident = Ident::from_str_and_span(&name, span);
            let kind = if to_gnd { BranchKind::UnnamedToGnd } else { BranchKind::Unnamed };

            let declaration = Branch {
                ident,
                hi,
                lo,
                sctx: fold.hir.syntax_ctx.push(SyntaxContextData {
                    span,
                    attributes: Attributes::EMPTY,
                    parent: Some(fold.sctx),
                }),

                kind,
                current_contributions: vec![],
                voltage_contributions: vec![],
                current_acccess: vec![],
                voltage_access: vec![],
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
    pub fn check_branch<F>(
        net1: NodeId,
        net2: NodeId,
        span: Span,
        fold: &mut Fold<F>,
    ) -> Option<DisciplineId>
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        let net1 = &fold.hir[net1];
        let net2 = &fold.hir[net2];
        let discipline1 = net1.discipline?;
        let discipline2 = net2.discipline?;

        if discipline1 == discipline2 {
            Some(discipline1)
        } else {
            fold.errors.add(DisciplineMismatch(
                NetInfo {
                    discipline: fold.hir[discipline1].ident.name,
                    name: net1.ident.name,
                    declaration: fold.hir[net1.sctx].span,
                },
                NetInfo {
                    discipline: fold.hir[discipline2].ident.name,
                    name: net2.ident.name,
                    declaration: fold.hir[net2.sctx].span,
                },
                span,
            ));

            None
        }
    }

    pub fn resolve_branch_probe_call<F>(
        &mut self,
        nature: NatureAccess,
        arguments: &IndexSlice<CallArg, [ExpressionId]>,
        fold: &mut Fold<F>,
    ) -> Option<(DisciplineAccess, BranchId)>
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        self.handle_branch_probe_args(
            nature.into_ident(&fold),
            arguments,
            |resolver, fold, net, span| {
                let branch = resolver.unnamed_branch_to_ground(span, net, fold);
                let access =
                    Self::resolve_discipline_access(fold, nature, fold.hir[net].discipline?)?;
                Some((access, branch))
            },
            |resolver, fold, hi, lo, span| {
                let discipline = Self::check_branch(hi, lo, span, fold)?;
                let branch = resolver.unnamed_branch(span, hi, lo, fold, false);
                let access = Self::resolve_discipline_access(fold, nature, discipline)?;
                Some((access, branch))
            },
            |_, fold, branch, _| {
                let discipline = fold.hir[fold.hir[branch].hi].discipline?; // Branches are checked previously .No need to recheck here
                let access = Self::resolve_discipline_access(fold, nature, discipline)?;
                Some((access, branch))
            },
            |resolver, fold, port, _span| {
                let discipline = fold.hir[fold.hir[port].node].discipline?;
                let branch = resolver.resolve_port_access(fold, nature, discipline, port);
                Some((DisciplineAccess::Flow, branch))
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
        arguments: &IndexSlice<CallArg, [ExpressionId]>,
        handle_node: impl FnOnce(&mut Self, &mut Fold<'a, F>, NodeId, Span) -> T,
        handle_unnamed_branch: impl FnOnce(&mut Self, &mut Fold<'a, F>, NodeId, NodeId, Span) -> T,
        handle_branch: impl FnOnce(&mut Self, &mut Fold<'a, F>, BranchId, Span) -> T,
        handle_port: impl FnOnce(&mut Self, &mut Fold<'a, F>, PortId, Span) -> T,
        fold: &mut Fold<'a, F>,
    ) -> Option<T>
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        match arguments.as_raw_slice() {
            [branch] => {
                let expr = &fold.ast[*branch];
                let ident = Self::reinterpret_expression_as_identifier(expr, fold)?;

                resolve_hierarchical!( fold;
                    ident as
                        Node(_,id)    => handle_node(self, fold, id, expr.span),
                        Port(id)   => handle_node(self, fold, fold.hir[id].node, expr.span),
                        Branch(id) => handle_branch(self, fold, id, expr.span),
                        PortBranch(id) => {
                            handle_port(self, fold, self.port_branche_probes[id],expr.span)
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
                        Node(_,id) => id,
                        Port(id)   => fold.hir[id].node

                );

                let net2 = Self::reinterpret_expression_as_identifier(net2, fold)?;
                let net2 = resolve_hierarchical!(fold;
                    net2 as
                        Node(_,id) => id,
                        Port(id)   => fold.hir[id].node
                );

                if let (Some(net1), Some(net2)) = (net1, net2) {
                    Some(handle_unnamed_branch(self, fold, net1, net2, span1.extend(span2)))
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
        F: Fn(Symbol) -> AllowedOperations,
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
    ) -> Option<(NodeId, NodeId)>
    where
        F: Fn(Symbol) -> AllowedOperations,
    {
        let hi = resolve_hierarchical!(fold; hi_ident as
            Node(_,id) => id,
            Port(id) => fold.hir[id].node
        );

        let (lo, span) = if let Some(lo_ident) = lo_ident {
            let span = hi_ident.span().extend(lo_ident.span());
            let lo = resolve_hierarchical!(fold; lo_ident as
                Node(_,id) => id,
                Port(id) => fold.hir[id].node
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
            NodeId::from_raw_unchecked(0)
        };

        Some((hi, lo))
    }
}
