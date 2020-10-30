//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! This module impliments a general data flow framework that allows to easily implement multiple data flow analysis

use crate::cfg::{BasicBlock, ControlFlowGraph, START_BLOCK};
use crate::CallType;
use openvaf_data_structures::index_vec::{index_vec, Idx, IndexVec};
use openvaf_data_structures::{BitSet, BitSetOperations, WorkQueue};
use std::mem::swap;
use tracing::trace_span;

/// This trait forms the core of the Data flow framework
/// Every Data flow analysis needs to implement this trait
pub trait Analysis<C: CallType>: Sized {
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
        basic_bock: BasicBlock,
        cfg: &ControlFlowGraph<C>,
    );

    /// The join function of the data flow analysis
    /// This should be a pure function of `src` and `dst`
    ///
    /// Generally a set union is appropriate here but sometimes intersection might be required

    fn join(&mut self, src: BasicBlock, dst: BasicBlock, graph: &mut DfGraph<Self::Set>);

    /// Create a new empty set
    /// This shall be a pure function in the sense that it should not make a difference whether
    /// the resulting set is cloned or this function is called again
    fn new_set(&self) -> Self::Set;
    fn setup_entry(&mut self, block: BasicBlock, graph: &mut DfGraph<Self::Set>);
}

/// The dirction in which a data flow analysis should be performed
pub trait Direction {
    /// The work queue with which the engine starts the solution process
    /// This should be **all** relevant blocks in correct order
    fn inital_work_queue<C: CallType>(cfg: &ControlFlowGraph<C>) -> WorkQueue<BasicBlock>;

    /// Propagate the outset of `bb` to the insets of dependent nodes using `apply(dependent_block)`
    fn propagate_result<C: CallType>(
        bb: BasicBlock,
        cfg: &ControlFlowGraph<C>,
        apply: impl FnMut(BasicBlock),
    );

    fn setup_entry<C: CallType>(cfg: &ControlFlowGraph<C>, apply: impl FnMut(BasicBlock));
}

/// [Reverse postorder](crate::cfg::transversal::ReversePostorder) DFA [direction](Direction)
pub struct Forward;

impl<'lt> Direction for Forward {
    fn inital_work_queue<C: CallType>(cfg: &ControlFlowGraph<C>) -> WorkQueue<BasicBlock> {
        let mut res = WorkQueue::with_none(cfg.blocks.len_idx());
        for id in cfg.reverse_postorder() {
            res.insert(id);
        }
        res
    }

    fn propagate_result<C: CallType>(
        bb: BasicBlock,
        cfg: &ControlFlowGraph<C>,
        mut apply: impl FnMut(BasicBlock),
    ) {
        for bb in cfg.successors(bb) {
            apply(bb)
        }
    }

    fn setup_entry<C: CallType>(_: &ControlFlowGraph<C>, mut apply: impl FnMut(BasicBlock)) {
        apply(START_BLOCK)
    }
}

/// [Postorder](crate::cfg::transversal::Postorder) DFA [direction](Direction)
pub struct Backward;

impl<'lt> Direction for Backward {
    fn inital_work_queue<C: CallType>(cfg: &ControlFlowGraph<C>) -> WorkQueue<BasicBlock> {
        let mut res = WorkQueue::with_none(cfg.blocks.len_idx());
        for (id, _) in cfg.postorder_iter() {
            res.insert(id);
        }
        res
    }

    fn propagate_result<C: CallType>(
        bb: BasicBlock,
        cfg: &ControlFlowGraph<C>,
        mut apply: impl FnMut(BasicBlock),
    ) {
        for &bb in cfg.predecessors(bb) {
            apply(bb)
        }
    }

    fn setup_entry<C: CallType>(cfg: &ControlFlowGraph<C>, mut apply: impl FnMut(BasicBlock)) {
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
/// The join function of `GenKillAnalysis` is currently always set union in this implimentation
///
pub trait GenKillAnalysis<C: CallType>: Sized {
    /// The typed used for the bitset
    type SetType: Idx + From<usize>;

    /// See [`Analysis::Direction`]
    type Direction: Direction;

    /// The transfer function is called **once before** the analysis begins
    /// to generate the `gen_kill_set`.
    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet<Self::SetType>,
        basic_bock: BasicBlock,
        cfg: &ControlFlowGraph<C>,
    );

    /// The largest idx that a set may contain
    fn max_idx(&self) -> Self::SetType;

    fn setup_entry(&mut self, block: BasicBlock, graph: &mut DfGraph<BitSet<Self::SetType>>);
}

/// A `GenKillEngine` is a wrapper around an `GenKillAnalysis`
/// which impliments [`Analysis`] and can be solved using the DFA engine
pub struct GenKillEngine<'lt, C: CallType, A: GenKillAnalysis<C>> {
    analysis: &'lt mut A,
    pub transfer_functions: IndexVec<BasicBlock, GenKillSet<A::SetType>>,
}

impl<'lt, C: CallType, A: GenKillAnalysis<C>> GenKillEngine<'lt, C, A> {
    pub fn new(cfg: &ControlFlowGraph<C>, analysis: &'lt mut A) -> Self {
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

impl<'lt, C: CallType, A: GenKillAnalysis<C>> Analysis<C> for GenKillEngine<'lt, C, A> {
    type Set = BitSet<A::SetType>;
    type Direction = A::Direction;

    fn transfer_function(
        &mut self,
        in_set: &Self::Set,
        out_set: &mut Self::Set,
        basic_bock: BasicBlock,
        _: &ControlFlowGraph<C>,
    ) {
        out_set.clear();
        out_set.union_with(in_set);
        out_set.difference_with(&self.transfer_functions[basic_bock].kill);
        out_set.union_with(&self.transfer_functions[basic_bock].gen);
    }

    fn join(&mut self, src: BasicBlock, dst: BasicBlock, graph: &mut DfGraph<Self::Set>) {
        graph.in_sets[dst].union_with(&graph.out_sets[src])
    }

    fn new_set(&self) -> Self::Set {
        BitSet::new_empty(self.analysis.max_idx())
    }

    fn setup_entry(&mut self, block: BasicBlock, graph: &mut DfGraph<Self::Set>) {
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

/// A worklist based DFA solver
pub struct Engine<'lt, C: CallType, A: Analysis<C>> {
    pub analysis: &'lt mut A,
    pub dfg: DfGraph<A::Set>,
    pub cfg: &'lt ControlFlowGraph<C>,
}

impl<'lt, A: Analysis<C>, C: CallType> Engine<'lt, C, A> {
    pub fn new(cfg: &'lt ControlFlowGraph<C>, analysis: &'lt mut A) -> Self {
        Self {
            dfg: DfGraph::new(analysis.new_set(), cfg),
            analysis,
            cfg,
        }
    }

    /// Runs the `analysis` until a fix point is reached
    ///
    /// A fixepoint is reachched when no more elements are in the worklist
    /// The [worklist](openvaf_data_structures::WorkQueue) is initialized using [`Direction:inital_work_queue`](crate::dfa_framework::Direction::inital_work_queue)
    /// Basic blocks are then removed, their transfer function is executed and they are propagated to any dependent blocks where their outset is joined into their inset.
    /// If the destination insets changed the elements are added back to the worklist
    ///
    #[must_use]
    pub fn iterate_to_fixpoint(mut self) -> DfGraph<A::Set> {
        let mut worklist =
            <<A as Analysis<C>>::Direction as Direction>::inital_work_queue(self.cfg);

        <<A as Analysis<C>>::Direction as Direction>::setup_entry(self.cfg, |entry| {
            self.analysis.setup_entry(entry, &mut self.dfg)
        });

        let mut temporary_set = self.analysis.new_set();

        while let Some(bb) = worklist.pop() {
            let span = trace_span!(target: "DataFlowEngine", "Iteration", block = bb.index());
            let _enter = span.enter();

            self.analysis.transfer_function(
                &self.dfg.in_sets[bb],
                &mut temporary_set,
                bb,
                self.cfg,
            );

            if temporary_set != self.dfg.out_sets[bb] {
                swap(&mut temporary_set, &mut self.dfg.out_sets[bb]);

                A::Direction::propagate_result(bb, self.cfg, |dst| {
                    worklist.insert(dst);
                    self.analysis.join(bb, dst, &mut self.dfg)
                });
            }
        }
        self.dfg
    }
}

pub struct DfGraph<Set> {
    pub in_sets: IndexVec<BasicBlock, Set>,
    pub out_sets: IndexVec<BasicBlock, Set>,
}

impl<Set: Clone> DfGraph<Set> {
    pub fn new<C: CallType>(empty_set: Set, cfg: &ControlFlowGraph<C>) -> Self {
        let in_sets = index_vec![empty_set;cfg.blocks.len()];
        Self {
            out_sets: in_sets.clone(),
            in_sets,
        }
    }
}
