/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use ahash::AHashMap;

use crate::ast::NetType::GROUND;
use crate::ast_lowering::ast_to_hir_fold::Fold;
use crate::ast_lowering::error::{Error, NetInfo, Type};
use crate::hir::Net;
use crate::hir::{Branch, BranchDeclaration, DisciplineAccess};
use crate::ir::*;
use crate::ir::{Push, SafeRangeCreation};
use crate::parser::error::Unsupported::DefaultDiscipline;
use crate::symbol::{keywords, Ident};
use crate::{ast, Ast, Span};
use rustc_hash::FxHashMap;

/// Handles branch resolution which is more complicated because unnamed branches exist and discipline comparability has to be enforced
/// # Safety
/// Branch resolution may only take place after all nets and ports have been folded! UB might occur otherwise.
pub struct BranchResolver<'tag> {
    unnamed_branches: FxHashMap<(NetId<'tag>, NetId<'tag>), BranchId<'tag>>,
    unnamed_port_branches: FxHashMap<PortId<'tag>, BranchId<'tag>>,
    implicit_grounds: FxHashMap<DisciplineId<'tag>, NetId<'tag>>,
}

impl<'tag, 'lt> BranchResolver<'tag> {
    pub fn new(ast: &'lt Ast<'tag>) -> Self {
        Self {
            unnamed_port_branches: FxHashMap::with_capacity_and_hasher(16, Default::default()),
            unnamed_branches: FxHashMap::with_capacity_and_hasher(32, Default::default()),
            implicit_grounds: FxHashMap::with_capacity_and_hasher(
                ast.disciplines.len as usize,
                Default::default(),
            ),
        }
    }
    /// Resolves a DisciplineAccess (for example `V(b)` or `V(x,y)`)
    ///
    /// # Arguments
    ///
    /// * fold - The calling fold which is used for name resolution and error handling
    ///
    /// * nature_name - The identifier of the nature (for example `V` in the case of `V(X,Y)`)
    ///
    /// * discipline - The id of the Discipline of a BranchAccess ( that has been resolved using [`resolve_branch_access`](crate::ast_lowering::branch_resolution::BranchResolver::resolve_discipline_access)
    ///
    ///
    /// # Safety
    /// This is only save to call
    ///
    ///
    pub fn resolve_discipline_access(
        &mut self,
        fold: &mut Fold<'tag, 'lt>,
        nature_name: &Ident,
        discipline: DisciplineId<'tag>,
    ) -> Option<DisciplineAccess> {
        match nature_name.name {
            keywords::FLOW => Some(DisciplineAccess::Flow),
            keywords::POTENTIAL => Some(DisciplineAccess::Potential),
            _ => {
                resolve!(fold; nature_name as
                    Nature(id) => {
                        return Self::resolve_nature_access(fold,id,discipline);
                    }
                );
                None
            }
        }
    }

    pub fn resolve_nature_access(
        fold: &mut Fold<'tag, 'lt>,
        id: NatureId<'tag>,
        discipline: DisciplineId<'tag>,
    ) -> Option<DisciplineAccess> {
        match id {
            id if Some(id) == fold.hir[discipline].contents.flow_nature => {
                Some(DisciplineAccess::Flow)
            }
            id if Some(id) == fold.hir[discipline].contents.potential_nature => {
                Some(DisciplineAccess::Potential)
            }
            _ => {
                fold.error(Error {
                    source: fold.hir[id].contents.name.span,
                    error_type: Type::NatureNotPotentialOrFlow(
                        fold.hir[id].contents.name.name,
                        discipline,
                    ),
                });
                None
            }
        }
    }

    /// Resolves a branch access such as `(NET1,NET2)`,`(<PORT>)` or `(BRANCH)`
    ///
    /// # Arguments
    ///
    /// * fold - The calling fold which is used for name resolution and error handling
    ///
    /// * branch_access - A reference to an Ast node for a branch access call
    ///
    ///
    /// # Returns
    /// The Id of the resolved branch and its Discipline (if the resolution succeeded)
    pub fn resolve_branch_access(
        &mut self,
        fold: &mut Fold<'tag, 'lt>,
        branch_access: &Node<ast::BranchAccess>,
    ) -> Option<(BranchId<'tag>, DisciplineId<'tag>)> {
        match branch_access.contents {
            ast::BranchAccess::Implicit(ref branch) => {
                let (branch, discipline) = self.resolve_branch(fold, branch)?;
                match branch {
                    Branch::Port(port) => match self.unnamed_port_branches.get(&port) {
                        Some(id) => return Some((*id, discipline)),
                        None => {
                            let branch_id = fold.hir.push(AttributeNode {
                                attributes: fold.hir.empty_range_from_end(),
                                source: branch_access.source,
                                contents: BranchDeclaration {
                                    name: Ident::from_str_and_span(
                                        format!(
                                            "branch (<{}>)",
                                            fold.hir[fold.hir[port].net].contents.name
                                        )
                                        .as_str(),
                                        branch_access.source,
                                    ),
                                    branch,
                                },
                            });
                            self.unnamed_port_branches.insert(port, branch_id);
                            return Some((branch_id, discipline));
                        }
                    },

                    Branch::Nets(net1, net2) => match self.unnamed_branches.get(&(net1, net2)) {
                        Some(id) => return Some((*id, discipline)),
                        None => {
                            let branch_id = fold.hir.push(AttributeNode {
                                attributes: fold.hir.empty_range_from_end(),
                                source: branch_access.source,
                                contents: BranchDeclaration {
                                    name: Ident::from_str_and_span(
                                        format!(
                                            "branch ({},{})",
                                            fold.hir[net1].contents.name.name.as_str(),
                                            fold.hir[net2].contents.name.name.as_str()
                                        )
                                        .as_str(),
                                        branch_access.source,
                                    ),
                                    branch,
                                },
                            });
                            self.unnamed_branches.insert((net1, net2), branch_id);
                            return Some((branch_id, discipline));
                        }
                    },
                }
            }

            ast::BranchAccess::Explicit(ref name) => {
                resolve_hierarchical!(fold; name as Branch(id) => {
                    let discipline = match fold.hir[id].contents.branch {
                        Branch::Port(portid) => {
                            fold.hir[fold.hir[portid].net].contents.discipline
                        }
                        Branch::Nets(net1, _) => fold.hir[net1].contents.discipline
                    };
                    return Some((id,discipline))
                })
            }
        }

        None
    }

    /// Resolves a branch such as (NET1,NET2) or (<PORT>)
    ///
    /// # Arguments
    ///
    /// * fold - The calling fold which is used for name resolution and error handling
    ///
    /// * branch - An Ast node describing a branch
    ///
    ///
    /// # Returns
    /// The Id of the resolved branch and its Discipline  (if the resolution succeeded)

    pub fn resolve_branch(
        &mut self,
        fold: &mut Fold<'tag, 'lt>,
        branch: &ast::Branch,
    ) -> Option<(Branch<'tag>, DisciplineId<'tag>)> {
        match branch {
            ast::Branch::Port(ref port) => {
                resolve_hierarchical!(fold; port as Port(port_id) => {
                    return Some((Branch::Port(port_id),fold.hir[fold.hir[port_id].net].contents.discipline));
                });
            }
            ast::Branch::NetToGround(ref net_ident) => {
                let mut net = None;
                resolve_hierarchical!(fold; net_ident as
                    Net(id) => {
                        net = Some(id);
                    },
                    Port(id) => {
                        net = Some(fold.hir[id].net);
                    }
                );
                if let Some(net) = net {
                    let discipline = fold.hir[net].contents.discipline;
                    let ground_net =
                        *self.implicit_grounds.entry(discipline).or_insert_with(|| {
                            fold.hir.push(AttributeNode {
                                contents: Net {
                                    name: Ident::from_str(
                                        format!("Implicit Ground_{:?}", discipline).as_str(),
                                    ),
                                    discipline,
                                    signed: false,
                                    net_type: GROUND,
                                },
                                source: Span::new_short_empty_span(0),
                                attributes: fold.hir.empty_range_from_end(),
                            })
                        });
                    return Some((Branch::Nets(net, ground_net), discipline));
                }
            }

            ast::Branch::Nets(ref net1, ref net2) => {
                let mut first_net = None;
                resolve_hierarchical!(fold; net1 as
                    Net(id) => {
                        first_net = Some(id);
                    },
                    Port(id) => {
                        first_net = Some(fold.hir[id].net);
                    }
                );

                let mut second_net = None;
                resolve_hierarchical!(fold; net2 as
                    Net(second_id) => {
                        second_net = Some(second_id)
                    },
                    Port(second_id) => {
                        second_net = Some(fold.hir[second_id].net)
                    }
                );

                if let (Some(first_net), Some(second_net)) = (first_net, second_net) {
                    if fold.hir[first_net].contents.discipline
                        != fold.hir[second_net].contents.discipline
                    {
                        fold.error(Error {
                            error_type: Type::DisciplineMismatch(
                                NetInfo {
                                    discipline: fold.hir[first_net].contents.discipline,
                                    name: fold.hir[first_net].contents.name.name,
                                    declaration: fold.hir[first_net].source,
                                },
                                NetInfo {
                                    discipline: fold.hir[second_net].contents.discipline,
                                    name: fold.hir[second_net].contents.name.name,
                                    declaration: fold.hir[second_net].source,
                                },
                            ),
                            source: net1.span().extend(net2.span()),
                        });
                    } else {
                        //doesn't matter which nets discipline we use since we asserted that they are equal
                        return Some((
                            Branch::Nets(first_net, second_net),
                            fold.hir[first_net].contents.discipline,
                        ));
                    }
                }
            }
        }

        None
    }
}
