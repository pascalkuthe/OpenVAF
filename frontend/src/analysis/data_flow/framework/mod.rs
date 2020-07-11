//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

pub use engine::Engine;
pub use graph::Graph as DataFlowGraph;

mod engine;
mod graph;

use crate::data_structures::{BitSet, BitSetOperations, WorkQueue};
use crate::ir::cfg::{BasicBlockId, ControlFlowGraph};
use index_vec::{Idx, IndexVec};

pub trait GenKillAnalysis<'lt>: Sized {
    type SetType: Idx + From<usize>;
    type Direction: Direction<'lt>;

    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet<Self::SetType>,
        basic_bock: BasicBlockId,
        cfg: &ControlFlowGraph,
    );

    fn max_idx(&self) -> Self::SetType;
}

pub trait Analysis<'lt>: Sized {
    type SetType: Idx + From<usize>;
    type Direction: Direction<'lt>;

    fn transfer_function(
        &mut self,
        in_set: &BitSet<Self::SetType>,
        out_set: &mut BitSet<Self::SetType>,
        basic_bock: BasicBlockId,
        cfg: &ControlFlowGraph,
    );

    fn join(&mut self, inout_set: &mut BitSet<Self::SetType>, in_set: &BitSet<Self::SetType>) {
        inout_set.union_with(in_set);
    }

    fn max_idx(&self) -> Self::SetType;
}

pub trait Direction<'lt> {
    fn inital_work_queue(cfg: &ControlFlowGraph) -> WorkQueue<BasicBlockId>;
    fn propagate_result(bb: BasicBlockId, cfg: &ControlFlowGraph, apply: impl FnMut(BasicBlockId));
}

pub struct Forward;

impl<'lt> Direction<'lt> for Forward {
    fn inital_work_queue(cfg: &ControlFlowGraph) -> WorkQueue<BasicBlockId> {
        let mut res = WorkQueue::with_none(cfg.blocks.len_idx());
        for id in cfg.reverse_postorder() {
            res.insert(id);
        }
        res
    }

    fn propagate_result(
        bb: BasicBlockId,
        cfg: &ControlFlowGraph,
        mut apply: impl FnMut(BasicBlockId),
    ) {
        for bb in cfg.successors(bb) {
            apply(bb)
        }
    }
}

pub struct Backward;

impl<'lt> Direction<'lt> for Backward {
    fn inital_work_queue(cfg: &ControlFlowGraph) -> WorkQueue<BasicBlockId> {
        let mut res = WorkQueue::with_none(cfg.blocks.len_idx());
        for (id, _) in cfg.postorder_iter() {
            res.insert(id);
        }
        res
    }

    fn propagate_result(
        bb: BasicBlockId,
        cfg: &ControlFlowGraph,
        mut apply: impl FnMut(BasicBlockId),
    ) {
        for &bb in cfg.predecessors(bb) {
            apply(bb)
        }
    }
}

pub struct GenKillEngine<'lt, A: GenKillAnalysis<'lt>> {
    analysis: &'lt mut A,
    pub transfer_functions: IndexVec<BasicBlockId, GenKillSet<A::SetType>>,
}

impl<'lt, A: GenKillAnalysis<'lt>> GenKillEngine<'lt, A> {
    pub fn new(cfg: &ControlFlowGraph, analysis: &'lt mut A) -> Self {
        let gen_kill_set = GenKillSet::new(analysis.max_idx());
        let transfer_functions = cfg
            .blocks
            .indices()
            .map(|bb| {
                let mut transfer_function = gen_kill_set.clone();
                analysis.transfer_function(&mut transfer_function, bb, cfg);
                transfer_function
            })
            .collect();

        Self {
            analysis,
            transfer_functions,
        }
    }
}

impl<'lt, A: GenKillAnalysis<'lt>> Analysis<'lt> for GenKillEngine<'lt, A> {
    type SetType = A::SetType;
    type Direction = A::Direction;

    fn transfer_function(
        &mut self,
        in_set: &BitSet<Self::SetType>,
        out_set: &mut BitSet<Self::SetType>,
        basic_bock: BasicBlockId,
        _: &ControlFlowGraph,
    ) {
        out_set.clear();
        out_set.union_with(in_set);
        out_set.difference_with(&self.transfer_functions[basic_bock].kill);
        out_set.union_with(&self.transfer_functions[basic_bock].gen);
    }
    fn max_idx(&self) -> Self::SetType {
        self.analysis.max_idx()
    }
}

#[derive(Clone)]
pub struct GenKillSet<I: Idx + From<usize>> {
    pub gen: BitSet<I>,
    pub kill: BitSet<I>,
}

impl<I: Idx + From<usize>> GenKillSet<I> {
    #[inline]
    pub fn new(max_idx: I) -> Self {
        let gen = BitSet::new_empty(max_idx);
        Self {
            kill: gen.clone(),
            gen,
        }
    }

    #[inline]
    pub fn gen(&mut self, x: I) {
        self.gen.insert(x);
        self.kill.set(x, false);
    }

    #[inline]
    pub fn kill(&mut self, x: I) {
        self.kill.insert(x);
        self.gen.set(x, false);
    }

    #[inline]
    pub fn kill_all<T>(&mut self, kill: &T)
    where
        T: BitSetOperations<I>,
    {
        self.kill.union_with(kill);
        self.gen.difference_with(kill);
    }

    #[inline]
    pub fn gen_all<T>(&mut self, gen: &T)
    where
        T: BitSetOperations<I>,
    {
        self.gen.union_with(gen);
        self.kill.difference_with(gen);
    }
}
