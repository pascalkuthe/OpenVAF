//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::compact_arena::{Idx16, TinyHeapArena};
use crate::ir::mir::ControlFlowGraph;
use crate::mir::control_flow_graph::{BasicBlockId, Terminator};
use bitflags::_core::mem::MaybeUninit;
use bitflags::_core::ops::IndexMut;
use std::ops::Index;

#[cfg(feature = "graph_debug")]
mod debug;

pub type DominatorTreeId<'tag> = Idx16<'tag>;

pub struct DominatorTree<'tag, 'cfg> {
    data: TinyHeapArena<'tag, DominatorTreeNode<'tag, 'cfg>>, //TODO only allocate as much memory as actually needed
}

impl<'tag, 'cfg> DominatorTree<'tag, 'cfg> {
    pub fn start(&self) -> DominatorTreeId<'tag> {
        unsafe {
            //This is save since we assume that a dominator tree was properly and as such has at least one node
            self.data.end()
        }
    }
    pub fn end(&self) -> DominatorTreeId<'tag> {
        unsafe {
            //This is save since we assume that a dominator tree was properly and as such has at least one node
            self.data.start()
        }
    }

    pub fn from_cfg<'mir>(
        cfg: &ControlFlowGraph<'cfg, 'mir>,
        mut allocator: TinyHeapArena<'tag, DominatorTreeNode<'tag, 'cfg>>,
    ) -> Self {
        cfg.for_all_blocks(|block| {
            let node_type = match cfg.blocks[block].terminator {
                Terminator::Merge(branch) => {
                    DominatorTreeNodeType::Leaf(Parent(MaybeUninit::uninit()))
                }
                Terminator::End => DominatorTreeNodeType::Leaf(Parent(MaybeUninit::uninit())),
                Terminator::Split {
                    condition,
                    true_block,
                    false_block,
                    merge,
                } => {
                    if merge == block {
                        DominatorTreeNodeType::Root(DominatorTreeBranches {
                            main_child: unsafe { false_block.transmute() },
                            true_child: unsafe {
                                Some((true_block.transmute(), MaybeUninit::uninit()))
                            },
                            false_child: None,
                        })
                    } else if false_block == merge {
                        DominatorTreeNodeType::Root(DominatorTreeBranches {
                            main_child: unsafe { merge.transmute() },
                            true_child: unsafe {
                                Some((true_block.transmute(), MaybeUninit::uninit()))
                            },
                            false_child: None,
                        })
                    } else if true_block == merge {
                        DominatorTreeNodeType::Root(DominatorTreeBranches {
                            main_child: unsafe { merge.transmute() },
                            true_child: unsafe {
                                Some((true_block.transmute(), MaybeUninit::uninit()))
                            },
                            false_child: None,
                        })
                    } else {
                        DominatorTreeNodeType::Root(DominatorTreeBranches {
                            main_child: unsafe { merge.transmute() },
                            true_child: unsafe {
                                Some((true_block.transmute(), MaybeUninit::uninit()))
                            },
                            false_child: Some((
                                unsafe { false_block.transmute() },
                                MaybeUninit::uninit(),
                            )),
                        })
                    }
                }
                Terminator::Goto(next) => DominatorTreeNodeType::Root(DominatorTreeBranches {
                    main_child: unsafe { next.transmute() },
                    true_child: None,
                    false_child: None,
                }),
            };

            allocator.add(DominatorTreeNode {
                node_type,
                basic_block: block,
            });
        });

        let mut res = DominatorTree { data: allocator };
        res.init_tree(res.start());
        res
    }

    fn init_tree(&mut self, start: DominatorTreeId<'tag>) -> DominatorTreeId<'tag> {
        let mut current = start;
        loop {
            match self[current].node_type {
                DominatorTreeNodeType::Root(branches) => {
                    let branches = self.init_branches(current, branches);
                    let next = branches.main_child;
                    self[current].node_type = DominatorTreeNodeType::Root(branches);
                    current = next;
                }

                DominatorTreeNodeType::Branch(parent, branches) => {
                    let branches = self.init_branches(current, branches);
                    let next = branches.main_child;
                    self[current].node_type = DominatorTreeNodeType::Branch(parent, branches);
                    current = next;
                }

                DominatorTreeNodeType::Leaf(_) => {
                    return current;
                }
            }
        }
    }

    fn init_branches(
        &mut self,
        node: DominatorTreeId<'tag>,
        mut branches: DominatorTreeBranches<'tag>,
    ) -> DominatorTreeBranches<'tag> {
        // TODO refactor the following two conditions into a function

        if let Some((true_child, ref mut true_branch_main_leaf)) = branches.true_child {
            let false_main_leaf = match self[true_child].node_type {
                DominatorTreeNodeType::Leaf(ref mut parent) => {
                    parent.0 = MaybeUninit::new(node);
                    true_child
                }

                DominatorTreeNodeType::Root(branches) => {
                    self[true_child].node_type =
                        DominatorTreeNodeType::Branch(Parent(MaybeUninit::new(node)), branches);
                    self.init_tree(true_child)
                }

                DominatorTreeNodeType::Branch(_, _) => unreachable_unchecked!(format!(
                    "Node already initialized {:?} \n {:?}",
                    true_child, self[true_child]
                )),
            };

            *true_branch_main_leaf = MaybeUninit::new(false_main_leaf);
        }

        if let Some((false_child, ref mut false_branch_main_leaf)) = branches.false_child {
            let false_main_leaf = match self[false_child].node_type {
                DominatorTreeNodeType::Leaf(ref mut parent) => {
                    parent.0 = MaybeUninit::new(node);
                    false_child
                }

                DominatorTreeNodeType::Root(branches) => {
                    self[false_child].node_type =
                        DominatorTreeNodeType::Branch(Parent(MaybeUninit::new(node)), branches);
                    self.init_tree(false_child)
                }

                DominatorTreeNodeType::Branch(_, _) => unreachable_unchecked!(format!(
                    "Node already initialized {:?} \n {:?}",
                    false_child, self[false_child]
                )),
            };

            *false_branch_main_leaf = MaybeUninit::new(false_main_leaf);
        }

        let main_child = branches.main_child;
        match self[branches.main_child].node_type {
            DominatorTreeNodeType::Leaf(ref mut parent) => {
                parent.0 = MaybeUninit::new(node);
            }

            DominatorTreeNodeType::Root(branches) => {
                self[main_child].node_type =
                    DominatorTreeNodeType::Branch(Parent(MaybeUninit::new(node)), branches);
            }

            DominatorTreeNodeType::Branch(parent, branches) => unreachable_unchecked!(format!(
                "Node {} already initialized with parent ({}) and branches {:?}",
                branches.main_child,
                parent.id(),
                branches
            )),
        };

        branches
    }
}

impl<'tag, 'mir> Index<DominatorTreeId<'tag>> for DominatorTree<'tag, 'mir> {
    type Output = DominatorTreeNode<'tag, 'mir>;

    fn index(&self, index: DominatorTreeId<'tag>) -> &Self::Output {
        &self.data[index]
    }
}

impl<'tag, 'mir> IndexMut<DominatorTreeId<'tag>> for DominatorTree<'tag, 'mir> {
    fn index_mut(&mut self, index: DominatorTreeId<'tag>) -> &mut Self::Output {
        &mut self.data[index]
    }
}

#[derive(Debug)]
pub struct DominatorTreeNode<'tag, 'cfg> {
    pub node_type: DominatorTreeNodeType<'tag>,
    pub basic_block: BasicBlockId<'cfg>,
}
#[derive(Copy, Clone, Debug)]
pub struct Parent<'tag>(MaybeUninit<DominatorTreeId<'tag>>);
impl<'tag> Parent<'tag> {
    #[inline(always)]
    pub fn id(self) -> DominatorTreeId<'tag> {
        unsafe { self.0.assume_init() }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum DominatorTreeNodeType<'tag> {
    Root(DominatorTreeBranches<'tag>),
    Leaf(Parent<'tag>),
    Branch(Parent<'tag>, DominatorTreeBranches<'tag>),
}

#[derive(Copy, Clone, Debug)]
pub struct DominatorTreeBranches<'tag> {
    pub main_child: DominatorTreeId<'tag>,
    true_child: Option<(DominatorTreeId<'tag>, MaybeUninit<DominatorTreeId<'tag>>)>,
    false_child: Option<(DominatorTreeId<'tag>, MaybeUninit<DominatorTreeId<'tag>>)>,
}
impl<'tag> DominatorTreeBranches<'tag> {
    #[inline(always)]
    pub fn true_child(&self) -> Option<(DominatorTreeId<'tag>, DominatorTreeId<'tag>)> {
        self.true_child
            .map(|(start, end)| (start, unsafe { end.assume_init() }))
    }

    #[inline(always)]
    pub fn false_child(&self) -> Option<(DominatorTreeId<'tag>, DominatorTreeId<'tag>)> {
        self.false_child
            .map(|(start, end)| (start, unsafe { end.assume_init() }))
    }
}
