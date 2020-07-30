//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! This module impliments a general data flow framework that allows to easily implement multiple data flow analysis
//! Since OpenVAF is not an optimizing compiler (delegating that responsibility to rusc/LLVM) it doesn't require an particulary advanced Data flow framework
//! As such this framework is currently limited to **bitset problems**
//! If more advanced analysis are required in the future this could be extended using traits/generics

pub use engine::Engine;
pub use graph::Graph as DataFlowGraph;

mod engine;
mod graph;

use crate::analysis::data_flow::framework::graph::Graph;
use crate::data_structures::{BitSet, BitSetOperations, WorkQueue};
use crate::ir::cfg::{BasicBlockId, ControlFlowGraph};
use index_vec::{Idx, IndexVec};

/// This trait forms the core of the Data flow framework
/// Every Data flow analysis needs to implement this trait
pub trait Analysis: Sized {
    /// The kind of set to use for this data flow analysis (for example a BitSet)
    type Set: Clone + PartialEq;

    /// The direction in which this data flow analysis should be solved (eg. Backward/Postorder or Forward/Reverse Postorder)
    type Direction: Direction;

    /// The transfer function of the data flow analysis
    /// This should be a pure function from `in_set` to `out_set` (the cfg and basic block can however be used)
    ///
    /// This function is called during every iteration in the solver.
    /// Nontrivial implementations will therefore have notable performance costs.
    ///
    /// Many pratical data flow analysis can be realized as a (gen kill)[GenKillAnalysis] analysis instead
    ///
    fn transfer_function(
        &mut self,
        in_set: &Self::Set,
        out_set: &mut Self::Set,
        basic_bock: BasicBlockId,
        cfg: &ControlFlowGraph,
    );

    /// The join function of the data flow analysis
    /// This should be a pure function of in_set and inout_set
    ///
    /// Generally a set unition is appropriate here but somtimes intersection might be required

    fn join(&mut self, src: BasicBlockId, dst: BasicBlockId, graph: &mut DataFlowGraph<Self::Set>);

    /// Create a new empty set
    /// This shall be a pure function in the sense that it should not make a difference whether
    /// the resulting set is cloned or this function is called again
    fn new_set(&self) -> Self::Set;
    fn setup_entry(&mut self, block: BasicBlockId, graph: &mut DataFlowGraph<Self::Set>);
}

/// The dirction in which a data flow analysis should be performed
pub trait Direction {
    /// The work queue with which the engine starts the solution process
    /// This should be **all** relevant blocks in correct order
    fn inital_work_queue(cfg: &ControlFlowGraph) -> WorkQueue<BasicBlockId>;

    /// Propagate the outset of `bb` to the insets of dependent nodes using `apply(dependent_block)`
    fn propagate_result(bb: BasicBlockId, cfg: &ControlFlowGraph, apply: impl FnMut(BasicBlockId));

    fn setup_entry(cfg: &ControlFlowGraph, apply: impl FnMut(BasicBlockId));
}

/// [Reverse postorder](crate::cfg::transversal::ReversePostorder) DFA [direction](Direction)
pub struct Forward;

impl<'lt> Direction for Forward {
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

    fn setup_entry(cfg: &ControlFlowGraph, mut apply: impl FnMut(BasicBlockId)) {
        apply(cfg.start())
    }
}

/// [Postorder](crate::cfg::transversal::Postorder) DFA [direction](Direction)
pub struct Backward;

impl<'lt> Direction for Backward {
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

    fn setup_entry(cfg: &ControlFlowGraph, mut apply: impl FnMut(BasicBlockId)) {
        apply(cfg.end())
    }
}

/// Gen kill analysis is a special form of data flow analysis
/// that is often used in practice
///
/// Gen kill analysis have seperable transfer functions
/// That is their transfer function can be reduced to a gen and kill set.
/// These sets are always added and removed from the inset to produce the outset
///
/// The join function of GenKillAnalysis is currently always set union in this implimentation
///
pub trait GenKillAnalysis: Sized {
    /// The typed used for the bitset
    type SetType: Idx + From<usize>;

    /// See [`Analysis::Direction`]
    type Direction: Direction;

    /// The transfer function is called **once before** the analysis begins
    /// to generate the `gen_kill_set`.
    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet<Self::SetType>,
        basic_bock: BasicBlockId,
        cfg: &ControlFlowGraph,
    );

    /// See [`Analysis::max_idx`]
    fn max_idx(&self) -> Self::SetType;

    fn setup_entry(&mut self, block: BasicBlockId, graph: &mut Graph<BitSet<Self::SetType>>);
}

/// A `GenKillEngine` is a wrapper around an `GenKillAnalysis`
/// which impliments [`Analysis`] and can be solved using the DFA engine
pub struct GenKillEngine<'lt, A: GenKillAnalysis> {
    analysis: &'lt mut A,
    pub transfer_functions: IndexVec<BasicBlockId, GenKillSet<A::SetType>>,
}

impl<'lt, A: GenKillAnalysis> GenKillEngine<'lt, A> {
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

impl<'lt, A: GenKillAnalysis> Analysis for GenKillEngine<'lt, A> {
    type Set = BitSet<A::SetType>;
    type Direction = A::Direction;

    fn transfer_function(
        &mut self,
        in_set: &Self::Set,
        out_set: &mut Self::Set,
        basic_bock: BasicBlockId,
        _: &ControlFlowGraph,
    ) {
        out_set.clear();
        out_set.union_with(in_set);
        out_set.difference_with(&self.transfer_functions[basic_bock].kill);
        out_set.union_with(&self.transfer_functions[basic_bock].gen);
    }

    fn join(&mut self, src: BasicBlockId, dst: BasicBlockId, graph: &mut DataFlowGraph<Self::Set>) {
        graph.in_sets[dst].union_with(&graph.out_sets[src])
    }

    fn new_set(&self) -> Self::Set {
        BitSet::new_empty(self.analysis.max_idx())
    }

    fn setup_entry(&mut self, block: BasicBlockId, graph: &mut Graph<Self::Set>) {
        self.analysis.setup_entry(block, graph)
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
