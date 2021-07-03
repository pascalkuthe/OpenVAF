/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use super::Analysis;
use crate::cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, Phi, TerminatorKind};
use crate::dfa::visitor::{ResultsVisitable, ResultsVisitor, ResultsVisitorMut};
use crate::dfa::{Effect, EffectIndex, GenKillAnalysis, GenKillSet};
use crate::{CallType, StatementId};
use std::cmp::max;
use std::ops::RangeInclusive;

pub trait Direction {
    const IS_FORWARD: bool;

    /// Applies all effects between the given `EffectIndex`s.
    ///
    /// `effects.start()` must precede or equal `effects.end()` in this direction.
    fn apply_effects_in_range<A, C>(
        analysis: &A,
        cfg: &ControlFlowGraph<C>,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
        effects: RangeInclusive<EffectIndex>,
    ) where
        C: CallType,
        A: Analysis<C>;

    fn apply_effects_in_block<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
    ) where
        C: CallType,
        A: Analysis<C>;

    fn gen_kill_effects_in_block<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        state: &mut GenKillSet<A::Idx>,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
    ) where
        C: CallType,
        A: GenKillAnalysis<C>;

    fn visit_results_in_block<C, F, R>(
        state: &mut F,
        cfg: &ControlFlowGraph<C>,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
        results: &R,
        vis: &mut impl ResultsVisitor<C, FlowState = F>,
    ) where
        R: ResultsVisitable<C, FlowState = F>,
        C: CallType;

    fn visit_results_in_block_mut<C, F, R>(
        state: &mut F,
        cfg: &mut ControlFlowGraph<C>,
        block: BasicBlock,
        results: &R,
        vis: &mut impl ResultsVisitorMut<C, FlowState = F>,
    ) where
        R: ResultsVisitable<C, FlowState = F>,
        C: CallType;

    fn join_state_into_successors_of<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        exit_state: &mut A::Domain,
        block: (BasicBlock, &BasicBlockData<C>),
        propagate: impl FnMut(BasicBlock, &A::Domain),
    ) where
        C: CallType,
        A: Analysis<C>;
}

/// Dataflow that runs from the exit of a block (the terminator), to its entry (the first statement).
pub struct Backward;

impl Direction for Backward {
    const IS_FORWARD: bool = false;

    fn apply_effects_in_range<A, C>(
        analysis: &A,
        cfg: &ControlFlowGraph<C>,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
        effects: RangeInclusive<EffectIndex>,
    ) where
        C: CallType,
        A: Analysis<C>,
    {
        let (from, to) = (*effects.start(), *effects.end());
        let terminator_index = block_data.statements.len() + block_data.phi_statements.len();

        assert!(from.idx <= terminator_index);
        assert!(!to.precedes_in_backward_order(from));

        // Apply the terminator if necessary
        let mut end = if from.effect == Effect::Before {
            // normalize the from bound so that it is exclusive (as ranges usually are)
            if from.idx == terminator_index {
                // applay the terminator. After that the terminator idx can be treated as excluded
                analysis.apply_terminator_effect(cfg, state, block_data.terminator(), block);
                from.idx
            } else {
                // Range to the next idx
                from.idx + 1
            }
        } else {
            // range is already eclsuvie (After exlucdes the indx itself at the start bound)
            from.idx
        };

        let start = if to.effect == Effect::Before {
            // Effect::Before meas inclusive so we increase by one to make the start inclusive
            to.idx + 1
        } else {
            to.idx
        };

        if end > block_data.phi_statements.len() {
            let stmnt_start =
                StatementId::new(start.saturating_sub(block_data.phi_statements.len()));
            let stmnt_end = StatementId::new(end - block_data.phi_statements.len());
            // Handle all statements
            for (idx, stmnt) in block_data.statements[stmnt_start..stmnt_end]
                .iter_enumerated()
                .rev()
            {
                analysis.apply_statement_effect(cfg, state, stmnt, idx, block);
            }
            end = block_data.phi_statements.len()
        }

        if start < block_data.phi_statements.len() {
            let start = Phi::new(start);
            let end = Phi::new(end);
            // Handle all statements
            for (phi, phi_data) in block_data.phi_statements[start..end]
                .iter_enumerated()
                .rev()
            {
                analysis.apply_phi_effect(cfg, state, phi_data, block, phi);
            }
        }
    }

    fn apply_effects_in_block<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
    ) where
        C: CallType,
        A: Analysis<C>,
    {
        analysis.init_block(cfg, state);
        let terminator = block_data.terminator();
        analysis.apply_terminator_effect(cfg, state, terminator, block);

        for (statement_index, stmnt) in block_data.statements.iter_enumerated().rev() {
            analysis.apply_statement_effect(cfg, state, stmnt, statement_index, block);
        }

        for (statement_index, phi) in block_data.phi_statements.iter_enumerated().rev() {
            analysis.apply_phi_effect(cfg, state, phi, block, statement_index);
        }
    }

    fn gen_kill_effects_in_block<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        state: &mut GenKillSet<A::Idx>,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
    ) where
        C: CallType,
        A: GenKillAnalysis<C>,
    {
        let terminator = block_data.terminator();
        analysis.terminator_effect(cfg, state, terminator, block);

        for (statement_index, stmnt) in block_data.statements.iter_enumerated().rev() {
            analysis.statement_effect(cfg, state, stmnt, statement_index, block);
        }

        for (statement_index, phi) in block_data.phi_statements.iter_enumerated().rev() {
            analysis.phi_effect(cfg, state, phi, block, statement_index);
        }
    }

    fn visit_results_in_block<C, F, R>(
        state: &mut F,
        cfg: &ControlFlowGraph<C>,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
        results: &R,
        vis: &mut impl ResultsVisitor<C, FlowState = F>,
    ) where
        R: ResultsVisitable<C, FlowState = F>,
        C: CallType,
    {
        assert!(block < cfg.blocks.len_idx());

        results.reset_to_block_entry(cfg, state, block);
        vis.visit_block_end(state, block_data, block);

        vis.visit_terminator_before_effect(state, block_data.terminator(), block);
        results.reconstruct_terminator_effect(cfg, state, block_data.terminator(), block);
        vis.visit_terminator_after_effect(state, block_data.terminator(), block);

        for (phi, phi_data) in cfg.blocks[block].phi_statements.iter_enumerated().rev() {
            vis.visit_phi_before_effect(state, phi_data, block, phi);
            results.reconstruct_phi_effect(cfg, state, phi_data, block, phi);
            vis.visit_phi_after_effect(state, phi_data, block, phi);
        }

        for (id, stmnt) in cfg.blocks[block].statements.iter_enumerated().rev() {
            vis.visit_statement_before_effect(state, stmnt, block, id);
            results.reconstruct_statement_effect(cfg, state, stmnt, block, id);
            vis.visit_statement_after_effect(state, stmnt, block, id);
        }

        vis.visit_block_start(state, block_data, block)
    }

    fn visit_results_in_block_mut<C, F, R>(
        state: &mut F,
        cfg: &mut ControlFlowGraph<C>,
        block: BasicBlock,
        results: &R,
        vis: &mut impl ResultsVisitorMut<C, FlowState = F>,
    ) where
        R: ResultsVisitable<C, FlowState = F>,
        C: CallType,
    {
        assert!(block < cfg.blocks.len_idx());

        results.reset_to_block_entry(cfg, state, block);
        vis.visit_block_end(state, &mut cfg.blocks[block], block);

        vis.visit_terminator_before_effect(state, cfg.blocks[block].terminator_mut(), block);
        results.reconstruct_terminator_effect(cfg, state, cfg.blocks[block].terminator(), block);
        vis.visit_terminator_after_effect(state, cfg.blocks[block].terminator_mut(), block);

        for phi in cfg.blocks[block].phi_statements.indices().rev() {
            vis.visit_phi_before_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .phi_statements
                        .raw
                        .get_unchecked_mut(phi.index())
                },
                block,
                phi,
            );
            results.reconstruct_phi_effect(
                cfg,
                state,
                unsafe {
                    cfg.blocks[block]
                        .phi_statements
                        .raw
                        .get_unchecked(phi.index())
                },
                block,
                phi,
            );
            vis.visit_phi_after_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .phi_statements
                        .raw
                        .get_unchecked_mut(phi.index())
                },
                block,
                phi,
            );
        }

        for stmnt in cfg.blocks[block].statements.indices().rev() {
            vis.visit_statement_before_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .statements
                        .raw
                        .get_unchecked_mut(stmnt.index())
                },
                block,
                stmnt,
            );
            results.reconstruct_statement_effect(
                cfg,
                state,
                unsafe {
                    cfg.blocks[block]
                        .statements
                        .raw
                        .get_unchecked(stmnt.index())
                },
                block,
                stmnt,
            );
            vis.visit_statement_after_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .statements
                        .raw
                        .get_unchecked_mut(stmnt.index())
                },
                block,
                stmnt,
            );
        }

        vis.visit_block_start(state, &mut cfg.blocks[block], block)
    }

    fn join_state_into_successors_of<A, C>(
        _analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        exit_state: &mut A::Domain,
        block: (BasicBlock, &BasicBlockData<C>),
        mut propagate: impl FnMut(BasicBlock, &A::Domain),
    ) where
        C: CallType,
        A: Analysis<C>,
    {
        for pred in cfg.predecessors(block.0).iter().copied() {
            propagate(pred, exit_state)
        }
    }
}

/// Dataflow that runs from the entry of a block (the first statement), to its exit (terminator).
pub struct Forward;

impl Direction for Forward {
    const IS_FORWARD: bool = true;

    fn apply_effects_in_range<A, C>(
        analysis: &A,
        cfg: &ControlFlowGraph<C>,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
        effects: RangeInclusive<EffectIndex>,
    ) where
        C: CallType,
        A: Analysis<C>,
    {
        let (from, to) = (*effects.start(), *effects.end());
        let terminator_index = block_data.statements.len() + block_data.phi_statements.len();

        assert!(from.idx <= terminator_index);
        assert!(!to.precedes_in_forward_order(from));

        // Apply the terminator if necessary
        let mut start = if from.effect == Effect::After {
            // normalize the from bound so that the start of the range is inslucive (as ranges usually are)
            from.idx + 1
        } else {
            from.idx
        };

        let end = if to.effect == Effect::After {
            if to.idx == terminator_index {
                // the terminator is already past the end of the statements. No need to increment here. Terminator is checked seperately later
                to.idx
            } else {
                // normalize the to bound so that the end of the range is exclusive (as ranges usually are)
                to.idx + 1
            }
        } else {
            to.idx
        };

        if start < block_data.phi_statements.len() {
            let phi_start = Phi::new(start);
            let phi_end = max(Phi::new(end), block_data.phi_statements.len_idx());
            // Handle all phis
            for (phi, phi_data) in block_data.phi_statements[phi_start..phi_end].iter_enumerated() {
                analysis.apply_phi_effect(cfg, state, phi_data, block, phi);
            }

            start = block_data.phi_statements.len()
        }

        if end > block_data.phi_statements.len() {
            let start = StatementId::new(start - block_data.phi_statements.len());
            let end = StatementId::new(end - block_data.phi_statements.len());
            // Handle all statements
            for (idx, stmnt) in block_data.statements[start..end].iter_enumerated() {
                analysis.apply_statement_effect(cfg, state, stmnt, idx, block);
            }
        }

        if to.idx == terminator_index && to.effect == Effect::After {
            analysis.apply_terminator_effect(cfg, state, block_data.terminator(), block);
        }
    }

    fn apply_effects_in_block<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
    ) where
        C: CallType,
        A: Analysis<C>,
    {
        analysis.init_block(cfg, state);
        for (statement_index, phi) in block_data.phi_statements.iter_enumerated() {
            analysis.apply_phi_effect(cfg, state, phi, block, statement_index);
        }

        for (statement_index, stmnt) in block_data.statements.iter_enumerated() {
            analysis.apply_statement_effect(cfg, state, stmnt, statement_index, block);
        }

        let terminator = block_data.terminator();
        analysis.apply_terminator_effect(cfg, state, terminator, block);
    }

    fn gen_kill_effects_in_block<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        state: &mut GenKillSet<A::Idx>,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
    ) where
        C: CallType,
        A: GenKillAnalysis<C>,
    {
        for (statement_index, phi) in block_data.phi_statements.iter_enumerated() {
            analysis.phi_effect(cfg, state, phi, block, statement_index);
        }

        for (statement_index, stmnt) in block_data.statements.iter_enumerated() {
            analysis.statement_effect(cfg, state, stmnt, statement_index, block);
        }

        let terminator = block_data.terminator();
        analysis.terminator_effect(cfg, state, terminator, block);
    }

    fn visit_results_in_block<C, F, R>(
        state: &mut F,
        cfg: &ControlFlowGraph<C>,
        block: BasicBlock,
        block_data: &BasicBlockData<C>,
        results: &R,
        vis: &mut impl ResultsVisitor<C, FlowState = F>,
    ) where
        R: ResultsVisitable<C, FlowState = F>,
        C: CallType,
    {
        assert!(block < cfg.blocks.len_idx());

        results.reset_to_block_entry(cfg, state, block);

        vis.visit_block_start(state, block_data, block);

        for (id, stmnt) in cfg.blocks[block].statements.iter_enumerated() {
            vis.visit_statement_before_effect(state, stmnt, block, id);
            results.reconstruct_statement_effect(cfg, state, stmnt, block, id);
            vis.visit_statement_after_effect(state, stmnt, block, id);
        }

        for (phi, phi_data) in cfg.blocks[block].phi_statements.iter_enumerated() {
            vis.visit_phi_before_effect(state, phi_data, block, phi);
            results.reconstruct_phi_effect(cfg, state, phi_data, block, phi);
            vis.visit_phi_after_effect(state, phi_data, block, phi);
        }

        vis.visit_terminator_before_effect(state, block_data.terminator(), block);
        results.reconstruct_terminator_effect(cfg, state, block_data.terminator(), block);
        vis.visit_terminator_after_effect(state, block_data.terminator(), block);

        vis.visit_block_end(state, block_data, block);
    }

    fn visit_results_in_block_mut<C, F, R>(
        state: &mut F,
        cfg: &mut ControlFlowGraph<C>,
        block: BasicBlock,
        results: &R,
        vis: &mut impl ResultsVisitorMut<C, FlowState = F>,
    ) where
        R: ResultsVisitable<C, FlowState = F>,
        C: CallType,
    {
        assert!(block < cfg.blocks.len_idx());

        results.reset_to_block_entry(cfg, state, block);
        vis.visit_block_start(state, &mut cfg.blocks[block], block);

        for stmnt in cfg.blocks[block].statements.indices() {
            vis.visit_statement_before_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .statements
                        .raw
                        .get_unchecked_mut(stmnt.index())
                },
                block,
                stmnt,
            );
            results.reconstruct_statement_effect(
                cfg,
                state,
                unsafe {
                    cfg.blocks[block]
                        .statements
                        .raw
                        .get_unchecked(stmnt.index())
                },
                block,
                stmnt,
            );
            vis.visit_statement_after_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .statements
                        .raw
                        .get_unchecked_mut(stmnt.index())
                },
                block,
                stmnt,
            );
        }

        for phi in cfg.blocks[block].phi_statements.indices() {
            vis.visit_phi_before_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .phi_statements
                        .raw
                        .get_unchecked_mut(phi.index())
                },
                block,
                phi,
            );
            results.reconstruct_phi_effect(
                cfg,
                state,
                unsafe {
                    cfg.blocks[block]
                        .phi_statements
                        .raw
                        .get_unchecked(phi.index())
                },
                block,
                phi,
            );
            vis.visit_phi_after_effect(
                state,
                unsafe {
                    cfg.blocks[block]
                        .phi_statements
                        .raw
                        .get_unchecked_mut(phi.index())
                },
                block,
                phi,
            );
        }

        vis.visit_terminator_before_effect(state, cfg.blocks[block].terminator_mut(), block);
        results.reconstruct_terminator_effect(cfg, state, cfg.blocks[block].terminator(), block);
        vis.visit_terminator_after_effect(state, cfg.blocks[block].terminator_mut(), block);

        vis.visit_block_end(state, &mut cfg.blocks[block], block);
    }

    fn join_state_into_successors_of<A, C>(
        analysis: &mut A,
        cfg: &ControlFlowGraph<C>,
        exit_state: &mut A::Domain,
        block: (BasicBlock, &BasicBlockData<C>),
        mut propagate: impl FnMut(BasicBlock, &A::Domain),
    ) where
        C: CallType,
        A: Analysis<C>,
    {
        match block.1.terminator().kind {
            TerminatorKind::Goto(succ) => {
                if analysis.apply_edge_effects(cfg, block.0, exit_state) {
                    propagate(succ, exit_state)
                }
            }
            TerminatorKind::Split {
                ref condition,
                true_block,
                false_block,
                ..
            } => {
                let mut applier = SplitEdgeEffectApplier {
                    exit_state,
                    true_block,
                    false_block,
                    propagate,
                    effects_applied: false,
                };

                analysis.apply_split_edge_effects(
                    cfg,
                    block.0,
                    condition,
                    exit_state,
                    &mut applier,
                );

                let SplitEdgeEffectApplier {
                    exit_state,
                    mut propagate,
                    effects_applied,
                    ..
                } = applier;

                if !effects_applied {
                    propagate(false_block, exit_state);
                    propagate(true_block, exit_state);
                }
            }
            TerminatorKind::End => {}
        }
    }
}

struct SplitEdgeEffectApplier<'a, D, F> {
    exit_state: &'a D,
    true_block: BasicBlock,
    false_block: BasicBlock,
    propagate: F,

    effects_applied: bool,
}

impl<D, F> super::SplitEdgeEffects<D> for SplitEdgeEffectApplier<'_, D, F>
where
    D: Clone,
    F: FnMut(BasicBlock, &D),
{
    fn apply(&mut self, mut apply_edge_effect: impl FnMut(&mut D, BasicBlock, bool) -> bool) {
        assert!(!self.effects_applied);

        let mut tmp = self.exit_state.clone();
        if apply_edge_effect(&mut tmp, self.true_block, true) {
            (self.propagate)(self.true_block, &tmp);
        }

        tmp.clone_from(&self.exit_state);
        if apply_edge_effect(&mut tmp, self.false_block, false) {
            (self.propagate)(self.false_block, &tmp);
        }

        self.effects_applied = true;
    }
}
