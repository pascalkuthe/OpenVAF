//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::compact_arena::{invariant_lifetime, TinyHeapArena};
use crate::ir::mir::ControlFlowGraph;
use crate::mir::control_flow_graph::{BasicBlockId, Terminator};
use bitflags::_core::mem::MaybeUninit;
use bitflags::_core::ops::IndexMut;
use std::ops::Index;
mod visit;

#[cfg(feature = "graph_debug")]
mod debug;

#[derive(Debug)]
pub struct DominatorTree<'cfg> {
    data: TinyHeapArena<'cfg, DominatorTreeNode<'cfg>>, //TODO only allocate as much memory as actually needed
    end: BasicBlockId<'cfg>,
}

impl<'cfg> DominatorTree<'cfg> {
    pub fn from_cfg<'mir>(cfg: &ControlFlowGraph<'cfg, 'mir>) -> Self {
        let mut allocator = unsafe { TinyHeapArena::new(invariant_lifetime(), cfg.block_count()) };
        cfg.for_all_blocks(|block| {
            let node = match cfg.blocks[block].terminator {
                Terminator::Merge(branch) => DominatorTreeNode::Leaf(Parent(MaybeUninit::uninit())),
                Terminator::End => DominatorTreeNode::Leaf(Parent(MaybeUninit::uninit())),
                Terminator::Split {
                    condition,
                    true_block,
                    false_block,
                    merge,
                } => {
                    if merge == block {
                        DominatorTreeNode::Root(DominatorTreeBranches {
                            main_child: false_block,
                            true_child: Some((true_block, MaybeUninit::uninit())),
                            false_child: None,
                        })
                    } else if false_block == merge {
                        DominatorTreeNode::Root(DominatorTreeBranches {
                            main_child: merge,
                            true_child: Some((true_block, MaybeUninit::uninit())),
                            false_child: None,
                        })
                    } else if true_block == merge {
                        DominatorTreeNode::Root(DominatorTreeBranches {
                            main_child: merge,
                            true_child: Some((true_block, MaybeUninit::uninit())),
                            false_child: None,
                        })
                    } else {
                        DominatorTreeNode::Root(DominatorTreeBranches {
                            main_child: merge,
                            true_child: Some((true_block, MaybeUninit::uninit())),
                            false_child: Some((false_block, MaybeUninit::uninit())),
                        })
                    }
                }
                Terminator::Goto(next) => DominatorTreeNode::Root(DominatorTreeBranches {
                    main_child: next,
                    true_child: None,
                    false_child: None,
                }),
            };

            allocator.add(node);
        });

        let mut res = DominatorTree {
            data: allocator,
            end: cfg.end(),
        };
        res.init_tree(cfg.start());
        res
    }

    fn init_tree(&mut self, start: BasicBlockId<'cfg>) -> BasicBlockId<'cfg> {
        let mut current = start;
        loop {
            match self[current] {
                DominatorTreeNode::Root(branches) => {
                    let branches = self.init_branches(current, branches);
                    let next = branches.main_child;
                    self[current] = DominatorTreeNode::Root(branches);
                    current = next;
                }

                DominatorTreeNode::Branch(parent, branches) => {
                    let branches = self.init_branches(current, branches);
                    let next = branches.main_child;
                    self[current] = DominatorTreeNode::Branch(parent, branches);
                    current = next;
                }

                DominatorTreeNode::Leaf(_) => {
                    return current;
                }
            }
        }
    }

    fn init_branches(
        &mut self,
        node: BasicBlockId<'cfg>,
        mut branches: DominatorTreeBranches<'cfg>,
    ) -> DominatorTreeBranches<'cfg> {
        // TODO refactor the following two conditions into a function

        if let Some((true_child, ref mut true_branch_main_leaf)) = branches.true_child {
            let false_main_leaf = match self[true_child] {
                DominatorTreeNode::Leaf(ref mut parent) => {
                    parent.0 = MaybeUninit::new(node);
                    true_child
                }

                DominatorTreeNode::Root(branches) => {
                    self[true_child] =
                        DominatorTreeNode::Branch(Parent(MaybeUninit::new(node)), branches);
                    self.init_tree(true_child)
                }

                DominatorTreeNode::Branch(_, _) => unreachable_unchecked!(format!(
                    "Node already initialized {:?} \n {:?}",
                    true_child, self[true_child]
                )),
            };

            *true_branch_main_leaf = MaybeUninit::new(false_main_leaf);
        }

        if let Some((false_child, ref mut false_branch_main_leaf)) = branches.false_child {
            let false_main_leaf = match self[false_child] {
                DominatorTreeNode::Leaf(ref mut parent) => {
                    parent.0 = MaybeUninit::new(node);
                    false_child
                }

                DominatorTreeNode::Root(branches) => {
                    self[false_child] =
                        DominatorTreeNode::Branch(Parent(MaybeUninit::new(node)), branches);
                    self.init_tree(false_child)
                }

                DominatorTreeNode::Branch(_, _) => unreachable_unchecked!(format!(
                    "Node already initialized {:?} \n {:?}",
                    false_child, self[false_child]
                )),
            };

            *false_branch_main_leaf = MaybeUninit::new(false_main_leaf);
        }

        let main_child = branches.main_child;
        match self[branches.main_child] {
            DominatorTreeNode::Leaf(ref mut parent) => {
                parent.0 = MaybeUninit::new(node);
            }

            DominatorTreeNode::Root(branches) => {
                self[main_child] =
                    DominatorTreeNode::Branch(Parent(MaybeUninit::new(node)), branches);
            }

            DominatorTreeNode::Branch(parent, branches) => unreachable_unchecked!(format!(
                "Node {} already initialized with parent ({}) and branches {:?}",
                branches.main_child,
                parent.id(),
                branches
            )),
        };

        branches
    }

    pub fn clone_for_subcfg<'newtag, 'mir>(
        &self,
        cfg: &ControlFlowGraph<'newtag, 'mir>,
    ) -> DominatorTree<'newtag> {
        assert!(cfg.block_count() <= self.data.len());
        let mut data = unsafe { TinyHeapArena::new(invariant_lifetime(), self.data.len()) };
        self.data.clone_into(&mut data);
        let mut res = DominatorTree {
            data: unsafe { std::mem::transmute(data) },
            end: unsafe { self.end.transmute() },
        };
        res
    }

    pub fn reborrow_for_subcfg<'newtag, 'mir>(
        &self,
        cfg: &ControlFlowGraph<'newtag, 'mir>,
    ) -> &DominatorTree<'newtag> {
        assert!(cfg.block_count() <= self.data.len());
        unsafe { std::mem::transmute(self) }
    }
}

impl<'cfg> Index<BasicBlockId<'cfg>> for DominatorTree<'cfg> {
    type Output = DominatorTreeNode<'cfg>;

    fn index(&self, index: BasicBlockId<'cfg>) -> &Self::Output {
        &self.data[index]
    }
}

impl<'cfg> IndexMut<BasicBlockId<'cfg>> for DominatorTree<'cfg> {
    fn index_mut(&mut self, index: BasicBlockId<'cfg>) -> &mut Self::Output {
        &mut self.data[index]
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Parent<'tag>(MaybeUninit<BasicBlockId<'tag>>);
impl<'tag> Parent<'tag> {
    #[inline(always)]
    pub fn id(self) -> BasicBlockId<'tag> {
        unsafe { self.0.assume_init() }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum DominatorTreeNode<'cfg> {
    Root(DominatorTreeBranches<'cfg>),
    Leaf(Parent<'cfg>),
    Branch(Parent<'cfg>, DominatorTreeBranches<'cfg>),
}

impl<'cfg> DominatorTreeNode<'cfg> {
    pub fn parent(&self) -> Option<BasicBlockId<'cfg>> {
        match self {
            Self::Root(_) => None,
            Self::Leaf(parent) | Self::Branch(parent, _) => Some(parent.id()),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct DominatorTreeBranches<'tag> {
    pub main_child: BasicBlockId<'tag>,
    true_child: Option<(BasicBlockId<'tag>, MaybeUninit<BasicBlockId<'tag>>)>,
    false_child: Option<(BasicBlockId<'tag>, MaybeUninit<BasicBlockId<'tag>>)>,
}

impl<'tag> DominatorTreeBranches<'tag> {
    #[inline(always)]
    pub fn true_child(&self) -> Option<(BasicBlockId<'tag>, BasicBlockId<'tag>)> {
        self.true_child
            .map(|(start, end)| (start, unsafe { end.assume_init() }))
    }

    #[inline(always)]
    pub fn false_child(&self) -> Option<(BasicBlockId<'tag>, BasicBlockId<'tag>)> {
        self.false_child
            .map(|(start, end)| (start, unsafe { end.assume_init() }))
    }
}
