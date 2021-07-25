/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! This module impliments a general data flow framework that allows to easily implement multiple data flow analysis

use crate::cfg::{BasicBlock, ControlFlowGraph, Location, LocationKind, Phi, PhiData, Terminator};
use crate::dfa::direciton::Direction;
use crate::dfa::engine::Engine;
use crate::dfa::lattice::JoinSemiLattice;
use crate::{CfgFunctions, RValue, Statement, StatementId};
use data_structures::{
    bit_set::{BitSet, FullBitSetOperations, HybridBitSet},
    index_vec::Idx,
};

// mod cursors;

use std::borrow::BorrowMut;
use std::cmp::Ordering;

pub use cursors::{GenKillResultsCursor, GenKillResultsRefCursor, ResultsCursor, ResultsRefCursor};
pub use engine::{GenKillResults, Results};
use std::fmt::Debug;
pub use visitor::{ResultsVisitable, ResultsVisitor, ResultsVisitorMut};

mod cursors;
pub mod direciton;
mod engine;
pub mod lattice;
pub mod visitor;

/// Define the domain of a dataflow problem.
///
/// This trait specifies the lattice on which this analysis operates (the domain) as well as its
/// initial value at the entry point of each basic block.
pub trait AnalysisDomain<C: CfgFunctions> {
    /// The type that holds the dataflow state at any given point in the program.
    type Domain: Clone + JoinSemiLattice + Debug;

    /// The direction of this analysis. Either `Forward` or `Backward`.
    type Direction: Direction;

    /// A descriptive name for this analysis. Used only for debugging.
    ///
    /// This name should be brief and contain no spaces, periods or other characters that are not
    /// suitable as part of a filename.
    const NAME: &'static str;

    /// The initial value of the dataflow state upon entry to each basic block.
    fn bottom_value(&self, cfg: &ControlFlowGraph<C>) -> Self::Domain;

    /// Mutates the initial value of the dataflow state upon entry to the `START_BLOCK`.
    ///
    /// For backward analyses, initial state besides the bottom value is not yet supported. Trying
    /// to mutate the initial state will result in a panic.
    fn initialize_start_block(&self, cfg: &ControlFlowGraph<C>, state: &mut Self::Domain);
}

/// Define the domain of a dataflow problem.
///
/// This trait specifies the lattice on which this analysis operates (the domain) as well as its
/// initial value at the entry point of each basic block.
pub trait GenKillAnalysisDomain<C: CfgFunctions> {
    /// The type that holds the dataflow state at any given point in the program.
    type Domain: Clone + JoinSemiLattice + GenKill<Self::Idx> + BorrowMut<BitSet<Self::Idx>> + Debug;

    type Idx: Idx;

    /// The direction of this analysis. Either `Forward` or `Backward`.
    type Direction: Direction;

    /// A descriptive name for this analysis. Used only for debugging.
    ///
    /// This name should be brief and contain no spaces, periods or other characters that are not
    /// suitable as part of a filename.
    const NAME: &'static str;

    /// The initial value of the dataflow state upon entry to each basic block.
    fn bottom_value(&self, cfg: &ControlFlowGraph<C>) -> Self::Domain;

    /// Mutates the initial value of the dataflow state upon entry to the `START_BLOCK`.
    ///
    /// For backward analyses, initial state besides the bottom value is not yet supported. Trying
    /// to mutate the initial state will result in a panic.
    fn initialize_start_block(&self, cfg: &ControlFlowGraph<C>, state: &mut Self::Domain);

    fn domain_size(&self, cfg: &ControlFlowGraph<C>) -> usize;
}

/// A dataflow problem with an arbitrarily complex transfer function.
///
/// # Convergence
///
/// When implementing this trait directly (not via [`GenKillAnalysis`]), it's possible to choose a
/// transfer function such that the analysis does not reach fixpoint. To guarantee convergence,
/// your transfer functions must maintain the following invariant:
///
/// > If the dataflow state **before** some point in the program changes to be greater
/// than the prior state **before** that point, the dataflow state **after** that point must
/// also change to be greater than the prior state **after** that point.
///
/// This invariant guarantees that the dataflow state at a given point in the program increases
/// monotonically until fixpoint is reached. Note that this monotonicity requirement only applies
/// to the same point in the program at different points in time. The dataflow state at a given
/// point in the program may or may not be greater than the state at any preceding point.
pub trait Analysis<C: CfgFunctions>: AnalysisDomain<C> {
    /// Init the state of block before the other analysis functions are called.
    #[inline(always)]
    fn init_block(&self, _cfg: &ControlFlowGraph<C>, _state: &mut Self::Domain) {}

    /// Updates the current dataflow state with the effect of evaluating a phi.
    #[inline(always)]
    fn apply_phi_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _state: &mut Self::Domain,
        _phi: &PhiData,
        _bb: BasicBlock,
        _idx: Phi,
    ) {
    }

    /// Updates the current dataflow state with the effect of evaluating a statement.
    #[inline(always)]
    fn apply_statement_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _state: &mut Self::Domain,
        _statement: &Statement<C>,
        _idx: StatementId,
        _bb: BasicBlock,
    ) {
    }

    /// Updates the current dataflow state with the effect of evaluating a terminator.
    #[inline(always)]
    fn apply_terminator_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _state: &mut Self::Domain,
        _terminator: &Terminator<C>,
        _bb: BasicBlock,
    ) {
    }

    /// Updates the current dataflow state with the effect of taking a particular branch in a
    /// `Split` terminator.
    ///
    /// Unlike the other edge-specific effects, which are allowed to mutate `Self::Domain`
    /// directly, overriders of this method should simply determine whether join should be performed along this edge
    ///
    /// FIXME: This class of effects is not supported for backward dataflow analyses.
    #[inline(always)]
    fn apply_edge_effects(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _block: BasicBlock,
        _state: &Self::Domain,
    ) -> bool {
        true
    }

    /// Updates the current dataflow state with the effect of taking a particular branch in a
    /// `Split` terminator.
    ///
    /// Unlike the other edge-specific effects, which are allowed to mutate `Self::Domain`
    /// directly, overriders of this method should simply determine whether join should be performed along this edge
    ///
    /// FIXME: This class of effects is not supported for backward dataflow analyses.
    #[inline(always)]
    fn apply_split_edge_effects(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _block: BasicBlock,
        _discr: &RValue<C>,
        _state: &Self::Domain,
        _edge_effects: &mut impl SplitEdgeEffects<Self::Domain>,
    ) {
    }

    /* Extension methods */

    /// Creates an `Engine` to find the fixpoint for this dataflow problem.
    ///
    /// You shouldn't need to override this outside this module, since the combination of the
    /// default impl and the one for all `A: GenKillAnalysis` will do the right thing.
    /// Its purpose is to enable method chaining like so:
    ///
    /// ```ignore (cross-crate-imports)
    /// let results = MyAnalysis::new(tcx, body)
    ///     .into_engine(tcx, body, def_id)
    ///     .iterate_to_fixpoint()
    ///     .into_results_cursor(body);
    /// ```
    fn into_engine(self, cfg: &ControlFlowGraph<C>) -> Engine<C, Self>
    where
        Self: Sized,
    {
        Engine::new_generic(cfg, self)
    }
}

/// A gen/kill dataflow problem.
///
/// Each method in this trait has a corresponding one in `Analysis`. However, these methods only
/// allow modification of the dataflow state via "gen" and "kill" operations. By defining transfer
/// functions for each statement in this way, the transfer function for an entire basic block can
/// be computed efficiently.
///
/// `Analysis` is automatically implemented for all implementers of `GenKillAnalysis`.
pub trait GenKillAnalysis<C: CfgFunctions>: GenKillAnalysisDomain<C> {
    /// Updates the current dataflow state with the effect of evaluating a phi.
    fn phi_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _trans: &mut impl GenKill<Self::Idx>,
        _phi: &PhiData,
        _bb: BasicBlock,
        _idx: Phi,
    ) {
    }

    /// Updates the current dataflow state with the effect of evaluating a statement.
    fn statement_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _trans: &mut impl GenKill<Self::Idx>,
        _statement: &Statement<C>,
        _idx: StatementId,
        _bb: BasicBlock,
    ) {
    }

    /// Updates the current dataflow state with the effect of evaluating a terminator.
    fn terminator_effect(
        &self,
        _cfg: &ControlFlowGraph<C>,
        _trans: &mut impl GenKill<Self::Idx>,
        _terminator: &Terminator<C>,
        _bb: BasicBlock,
    ) {
    }

    /* Extension methods */
    fn into_engine(self, cfg: &ControlFlowGraph<C>) -> Engine<C, GenKillAnalysisImpl<Self>>
    where
        Self: Sized,
    {
        Engine::new_gen_kill(cfg, self)
    }
}

pub struct GenKillAnalysisImpl<A>(A);

impl<A, C> AnalysisDomain<C> for GenKillAnalysisImpl<A>
where
    C: CfgFunctions,
    A: GenKillAnalysisDomain<C>,
{
    type Domain = A::Domain;
    type Direction = A::Direction;
    const NAME: &'static str = A::NAME;

    #[inline(always)]
    fn bottom_value(&self, cfg: &ControlFlowGraph<C>) -> Self::Domain {
        self.0.bottom_value(cfg)
    }

    #[inline(always)]
    fn initialize_start_block(&self, cfg: &ControlFlowGraph<C>, state: &mut Self::Domain) {
        self.0.initialize_start_block(cfg, state)
    }
}

impl<A, C> Analysis<C> for GenKillAnalysisImpl<A>
where
    C: CfgFunctions,
    A: GenKillAnalysis<C>,
{
    #[inline(always)]
    /// Updates the current dataflow state with the effect of evaluating a phi.
    fn apply_phi_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::Domain,
        phi: &PhiData,
        bb: BasicBlock,
        idx: Phi,
    ) {
        self.0.phi_effect(cfg, state, phi, bb, idx)
    }

    #[inline(always)]
    /// Updates the current dataflow state with the effect of evaluating a statement.
    fn apply_statement_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::Domain,
        statement: &Statement<C>,
        idx: StatementId,
        bb: BasicBlock,
    ) {
        self.0.statement_effect(cfg, state, statement, idx, bb)
    }

    #[inline(always)]
    /// Updates the current dataflow state with the effect of evaluating a terminator.
    fn apply_terminator_effect(
        &self,
        cfg: &ControlFlowGraph<C>,
        state: &mut Self::Domain,
        terminator: &Terminator<C>,
        bb: BasicBlock,
    ) {
        self.0.terminator_effect(cfg, state, terminator, bb)
    }

    #[inline(always)]
    /* Extension methods */
    fn into_engine(self, cfg: &ControlFlowGraph<C>) -> Engine<C, Self>
    where
        Self: Sized,
    {
        Engine::new_gen_kill(cfg, self.0)
    }
}

/// The legal operations for a transfer function in a gen/kill problem.
///
/// This abstraction exists because there are two different contexts in which we call the methods in
/// `GenKillAnalysis`. Sometimes we need to store a single transfer function that can be efficiently
/// applied multiple times, such as when computing the cumulative transfer function for each block.
/// These cases require a `GenKillSet`, which in turn requires two `BitSet`s of storage. Oftentimes,
/// however, we only need to apply an effect once. In *these* cases, it is more efficient to pass the
/// `BitSet` representing the state vector directly into the `*_effect` methods as opposed to
/// building up a `GenKillSet` and then throwing it away.
pub trait GenKill<T: Idx>: Debug {
    /// Inserts `elem` into the state vector.
    fn gen(&mut self, elem: T);

    /// Removes `elem` from the state vector.
    fn kill(&mut self, elem: T);

    /// Calls `gen` for each element in `elems`.
    fn gen_all(&mut self, elems: impl IntoIterator<Item = T>) {
        for elem in elems {
            self.gen(elem);
        }
    }

    /// Calls `kill` for each element in `elems`.
    fn kill_all(&mut self, elems: impl IntoIterator<Item = T>) {
        for elem in elems {
            self.kill(elem);
        }
    }

    /// Calls `gen` for each element in `elems`.
    fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>);

    /// Calls `kill` for each element in `elems`.
    fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>);
}

/// Stores a transfer function for a gen/kill problem.
///
/// Calling `gen`/`kill` on a `GenKillSet` will "build up" a transfer function so that it can be
/// applied multiple times efficiently. When there are multiple calls to `gen` and/or `kill` for
/// the same element, the most recent one takes precedence.
#[derive(Clone, Debug)]
pub struct GenKillSet<T: Idx> {
    gen: HybridBitSet<T>,
    kill: HybridBitSet<T>,
    domain_size: usize,
}

impl<T: Idx> GenKillSet<T> {
    /// Creates a new transfer function that will leave the dataflow state unchanged.
    pub fn identity(domain_size: usize) -> Self {
        GenKillSet { gen: HybridBitSet::new_empty(), kill: HybridBitSet::new_empty(), domain_size }
    }

    pub fn apply(&self, state: &mut BitSet<T>) {
        state.union(&self.gen);
        state.subtract(&self.kill);
    }
}

impl<T: Idx> GenKill<T> for GenKillSet<T> {
    fn gen(&mut self, elem: T) {
        self.gen.insert(elem, self.domain_size);
        self.kill.remove(elem);
    }

    fn kill(&mut self, elem: T) {
        self.kill.insert(elem, self.domain_size);
        self.gen.remove(elem);
    }

    fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>) {
        self.kill.subtract(elems);
        self.gen.union(elems, self.domain_size);
    }

    fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>) {
        self.kill.union(elems, self.domain_size);
        self.gen.subtract(elems);
    }
}

impl<T: Idx> GenKill<T> for BitSet<T> {
    fn gen(&mut self, elem: T) {
        self.insert(elem);
    }

    fn kill(&mut self, elem: T) {
        self.remove(elem);
    }

    fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>) {
        self.union(elems);
    }

    fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>) {
        self.subtract(elems);
    }
}

impl<T: Idx> GenKill<T> for lattice::Dual<BitSet<T>> {
    fn gen(&mut self, elem: T) {
        self.0.insert(elem);
    }

    fn kill(&mut self, elem: T) {
        self.0.remove(elem);
    }

    fn gen_set(&mut self, elems: &impl FullBitSetOperations<T>) {
        self.0.union(elems);
    }

    fn kill_set(&mut self, elems: &impl FullBitSetOperations<T>) {
        self.0.subtract(elems);
    }
}

//NOTE: DO NOT CHANGE VARIANT ORDER. The derived `Ord` impls rely on the current order.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Effect {
    Before,
    After,
}

impl Effect {
    pub const fn at_index(self, idx: usize) -> EffectIndex {
        EffectIndex { effect: self, idx }
    }

    pub fn at_location<C: CfgFunctions>(
        self,
        loc: Location,
        cfg: &ControlFlowGraph<C>,
    ) -> EffectIndex {
        let idx = match loc.kind {
            LocationKind::Phi(x) => x.index(),
            LocationKind::Statement(x) => x.index() + cfg.blocks[loc.block].phi_statements.len(),
            LocationKind::Terminator => {
                cfg.blocks[loc.block].phi_statements.len() + cfg.blocks[loc.block].statements.len()
            }
        };
        self.at_index(idx)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EffectIndex {
    idx: usize,
    effect: Effect,
}

impl EffectIndex {
    fn next_in_forward_order(self) -> Self {
        match self.effect {
            Effect::Before => Effect::After.at_index(self.idx),
            Effect::After => Effect::Before.at_index(self.idx + 1),
        }
    }

    fn next_in_backward_order(self) -> Self {
        match self.effect {
            Effect::Before => Effect::After.at_index(self.idx),
            Effect::After => Effect::Before.at_index(self.idx - 1),
        }
    }

    /// Returns `true` if the effect at `self` should be applied earlier than the effect at `other`
    /// in forward order.
    fn precedes_in_forward_order(self, other: Self) -> bool {
        let ord = self.idx.cmp(&other.idx).then_with(|| self.effect.cmp(&other.effect));
        ord == Ordering::Less
    }

    /// Returns `true` if the effect at `self` should be applied earlier than the effect at `other`
    /// in backward order.
    fn precedes_in_backward_order(self, other: Self) -> bool {
        let ord = other.idx.cmp(&self.idx).then_with(|| self.effect.cmp(&other.effect));
        ord == Ordering::Less
    }
}

/// A type that records the edge-specific effects for a `SwitchInt` terminator.
pub trait SplitEdgeEffects<D> {
    /// Calls `apply_edge_effect` for each outgoing edge from a `SwitchInt` terminator and
    /// records the results.
    fn apply(&mut self, apply_edge_effect: impl FnMut(&mut D, BasicBlock, bool) -> bool);
}
