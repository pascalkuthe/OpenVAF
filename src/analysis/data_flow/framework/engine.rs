//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::data_flow::framework::graph::Graph;
use crate::analysis::data_flow::framework::{Analysis, DataFlowGraph};
use crate::analysis::dominator_tree::DominatorTreeNode;
use crate::analysis::DominatorTree;
use crate::mir::control_flow_graph::{BasicBlockId, Terminator};
use crate::mir::ControlFlowGraph;
use fixedbitset::FixedBitSet as BitSet;
use std::mem::swap;

// A wrapper around bitset takes care of the Conversion between usize and BasicBlockId
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Clone)]
pub struct DirtyList {
    pub data: BitSet,
}

impl DirtyList {
    #[inline]
    pub fn new(cfg_block_count: u16) -> Self {
        let mut data = BitSet::with_capacity(cfg_block_count as usize);
        data.set_range(.., true);
        Self { data }
    }

    #[inline]
    pub fn contains(&self, block: BasicBlockId) -> bool {
        self.data.contains(block.index() as usize)
    }

    #[inline]
    pub fn insert(&mut self, block: BasicBlockId) {
        self.data.insert(block.index() as usize)
    }

    #[inline]
    pub fn remove(&mut self, block: BasicBlockId) -> bool {
        if self.contains(block) {
            self.data.set(block.index() as usize, false);
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.data.clear()
    }
}

pub trait Engine<'lt, 'cfg: 'lt, 'mir: 'lt, A: Analysis<'lt, 'cfg, 'mir>>: Sized {
    type Args;
    fn iterate_to_fixpoint(self) -> DataFlowGraph<'cfg>;
    fn new_with_inital_set(
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        analysis: &'lt mut A,
        inital_set: &BitSet,
        args: Self::Args,
    ) -> Self;

    fn new_with_empty_dfg(
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        analysis: &'lt mut A,
        args: Self::Args,
    ) -> Self {
        let dfg = DataFlowGraph::new(&*cfg, analysis.set_size());
        let mut dirty_list = DirtyList::new(cfg.block_count());
        dirty_list.data.set_range(.., true);
        Self::new(cfg, dfg, dirty_list, analysis, args)
    }

    fn new(
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        dfg: DataFlowGraph<'cfg>,
        dirty_list: DirtyList,
        analysis: &'lt mut A,
        args: Self::Args,
    ) -> Self;
}

pub struct ForwardEngine<'lt, 'cfg: 'lt, 'mir, A: Analysis<'lt, 'cfg, 'mir>> {
    analysis: &'lt mut A,
    cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
    dfg: DataFlowGraph<'cfg>,

    dirty_list: DirtyList,
    temporary_set: BitSet,
}

impl<'lt, 'cfg: 'lt, 'mir: 'lt, A: Analysis<'lt, 'cfg, 'mir>> Engine<'lt, 'cfg, 'mir, A>
    for ForwardEngine<'lt, 'cfg, 'mir, A>
{
    type Args = ();

    #[inline]
    fn iterate_to_fixpoint(mut self) -> Graph<'cfg> {
        loop {
            self.transverse_cfg(self.cfg.start(), None);
            // println!("{:#?}", self.dirty_list.data);
            if self.dirty_list.data.as_slice().iter().all(|&val| val == 0) {
                break;
            }
        }
        self.dfg
    }

    #[inline]
    fn new_with_inital_set(
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        analysis: &'lt mut A,
        inital_set: &BitSet,
        _: (),
    ) -> Self {
        let mut res = Self::new_with_empty_dfg(cfg, analysis, ());
        res.dfg.in_sets[res.cfg.start()].union_with(inital_set);
        res
    }

    #[inline]
    fn new(
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        dfg: DataFlowGraph<'cfg>,
        dirty_list: DirtyList,
        analysis: &'lt mut A,
        _args: (),
    ) -> Self {
        Self {
            dirty_list,
            temporary_set: BitSet::with_capacity(analysis.set_size()),
            analysis,
            cfg,
            dfg,
        }
    }
}

impl<'lt, 'cfg: 'lt, 'mir: 'lt, A: Analysis<'lt, 'cfg, 'mir>> ForwardEngine<'lt, 'cfg, 'mir, A> {
    fn transverse_cfg(&mut self, start: BasicBlockId<'cfg>, end: Option<BasicBlockId<'cfg>>) {
        let dfg = &mut self.dfg;
        let temporary_set = &mut self.temporary_set;
        let dirty_list = &mut self.dirty_list;
        let analysis = &mut self.analysis;

        self.cfg.visit_mut_in_execution_order(|cfg, current, _| {
            if dirty_list.remove(current) {
                analysis.transfer_function(&dfg.in_sets[current], temporary_set, current, cfg);
                if temporary_set != &dfg.out_sets[current] {
                    swap(temporary_set, &mut dfg.out_sets[current]);
                    match cfg[current].terminator {
                        Terminator::End => return,

                        Terminator::Goto(next) | Terminator::Merge(next) => {
                            dirty_list.insert(next);
                            analysis.join(&mut dfg.in_sets[next], &dfg.out_sets[current])
                        }

                        Terminator::Split {
                            condition: _,
                            true_block,
                            false_block,
                            merge,
                        } => {
                            dirty_list.insert(true_block);
                            analysis.join(&mut dfg.in_sets[true_block], &dfg.out_sets[current]);
                            dirty_list.insert(false_block);
                            analysis.join(&mut dfg.in_sets[false_block], &dfg.out_sets[current]);
                        }
                    }
                }
                temporary_set.clear();
            }
        });
    }
}

pub struct BackwardEngine<'lt, 'cfg: 'lt, 'mir: 'lt, A: Analysis<'lt, 'cfg, 'mir>> {
    analysis: &'lt mut A,
    cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
    dtree: &'lt DominatorTree<'cfg>,
    dfg: DataFlowGraph<'cfg>,

    dirty_list: DirtyList,
    temporary_set: BitSet,
}

impl<'lt, 'cfg: 'lt, 'mir: 'lt, A: Analysis<'lt, 'cfg, 'mir>> Engine<'lt, 'cfg, 'mir, A>
    for BackwardEngine<'lt, 'cfg, 'mir, A>
{
    type Args = &'lt DominatorTree<'cfg>;

    #[inline]
    fn iterate_to_fixpoint(mut self) -> Graph<'cfg> {
        loop {
            self.transverse_cfg(self.cfg.end(), None);
            if self.dirty_list.data.as_slice().iter().all(|&val| val == 0) {
                break;
            }
        }
        self.dfg
    }

    #[inline]
    fn new_with_inital_set(
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        analysis: &'lt mut A,
        initial_set: &BitSet,
        dtree: &'lt DominatorTree<'cfg>,
    ) -> Self {
        let mut res = Self::new_with_empty_dfg(cfg, analysis, dtree);
        res.dfg.out_sets[res.cfg.end()].union_with(initial_set);
        res
    }

    #[inline]
    fn new(
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        dfg: DataFlowGraph<'cfg>,
        dirty_list: DirtyList,
        analysis: &'lt mut A,
        dtree: &'lt DominatorTree<'cfg>,
    ) -> Self {
        Self {
            dirty_list,
            temporary_set: BitSet::with_capacity(analysis.set_size()),
            analysis,
            cfg,
            dtree,
            dfg,
        }
    }
}

impl<'lt, 'cfg: 'lt, 'mir: 'lt, A: Analysis<'lt, 'cfg, 'mir>> BackwardEngine<'lt, 'cfg, 'mir, A> {
    fn transverse_cfg(&mut self, start: BasicBlockId<'cfg>, end: Option<BasicBlockId<'cfg>>) {
        let mut current = start;
        loop {
            match self.dtree[current] {
                DominatorTreeNode::Leaf(parent) => (),
                DominatorTreeNode::Root(branch) | DominatorTreeNode::Branch(_, branch) => {
                    self.dfg.out_sets[current].clear();

                    if let Some((start, end)) = branch.true_child() {
                        if self.dirty_list.contains(end) {
                            self.dfg.out_sets[end].clear();
                            self.dfg.out_sets[end].union_with(&self.dfg.in_sets[branch.main_child]);
                        }
                        self.transverse_cfg(end, Some(current));
                        self.dfg.out_sets[current].union_with(&self.dfg.in_sets[start]);
                    }

                    if let Some((start, end)) = branch.false_child() {
                        if self.dirty_list.contains(end) {
                            self.dfg.out_sets[end].clear();
                            self.dfg.out_sets[end].union_with(&self.dfg.in_sets[branch.main_child]);
                        }
                        self.transverse_cfg(end, Some(current));
                        self.dfg.out_sets[current].union_with(&self.dfg.in_sets[start]);
                    }

                    if branch.true_child().is_none() && branch.false_child().is_none() {
                        self.dfg.out_sets[current].union_with(&self.dfg.in_sets[branch.main_child]);
                    }
                }
            }

            if self.dirty_list.remove(current) {
                self.analysis.transfer_function(
                    &self.dfg.out_sets[current],
                    &mut self.temporary_set,
                    current,
                    self.cfg,
                );

                if self.temporary_set != self.dfg.in_sets[current] {
                    swap(&mut self.temporary_set, &mut self.dfg.in_sets[current]);
                    if let Some(parent) = self.dtree[current].parent() {
                        self.dirty_list.insert(parent)
                    }
                }
            }

            current = match self.dtree[current].parent() {
                None => return,
                parent if parent == end => return,
                Some(parent) => parent,
            }
        }
    }
}
