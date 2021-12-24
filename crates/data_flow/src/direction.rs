use std::cmp::min;
use std::ops::RangeInclusive;

use cfg::{BasicBlock, BasicBlockData, ControlFlowGraph, InstIdx, PhiIdx, Terminator};

use super::Analysis;
use crate::visitor::{ResultsVisitable, ResultsVisitor, ResultsVisitorMut};
use crate::{Effect, EffectIndex, GenKillAnalysis, GenKillSet};

pub trait Direction {
    const IS_FORWARD: bool;

    /// Applies all effects between the given `EffectIndex`s.
    ///
    /// `effects.start()` must precede or equal `effects.end()` in this direction.
    fn apply_effects_in_range<A>(
        analysis: &A,
        cfg: &ControlFlowGraph,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData,
        effects: RangeInclusive<EffectIndex>,
    ) where
        A: Analysis;

    fn apply_effects_in_block<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData,
    ) where
        A: Analysis;

    fn gen_kill_effects_in_block<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        state: &mut GenKillSet<A::Idx>,
        block: BasicBlock,
        block_data: &BasicBlockData,
    ) where
        A: GenKillAnalysis;

    fn visit_results_in_block<F, R>(
        state: &mut F,
        cfg: &ControlFlowGraph,
        block: BasicBlock,
        block_data: &BasicBlockData,
        results: &R,
        vis: &mut impl ResultsVisitor<FlowState = F>,
    ) where
        R: ResultsVisitable<FlowState = F>;

    fn visit_results_in_block_mut<F, R>(
        state: &mut F,
        cfg: &mut ControlFlowGraph,
        block: BasicBlock,
        results: &R,
        vis: &mut impl ResultsVisitorMut<FlowState = F>,
    ) where
        R: ResultsVisitable<FlowState = F>;

    fn join_state_into_successors_of<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        exit_state: &mut A::Domain,
        block: (BasicBlock, &BasicBlockData),
        propagate: impl FnMut(BasicBlock, &A::Domain),
    ) where
        A: Analysis;
}

/// Dataflow that runs from the exit of a block (the terminator), to its entry (the first
/// instruction).
pub struct Backward;

impl Direction for Backward {
    const IS_FORWARD: bool = false;

    fn apply_effects_in_range<A>(
        analysis: &A,
        cfg: &ControlFlowGraph,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData,
        effects: RangeInclusive<EffectIndex>,
    ) where
        A: Analysis,
    {
        let (from, to) = (*effects.start(), *effects.end());
        let terminator_index = block_data.instructions.len() + block_data.phis.len();

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

        if end > block_data.phis.len() {
            let stmnt_start = InstIdx::from(start.saturating_sub(block_data.phis.len()));
            let stmnt_end = InstIdx::from(end - block_data.phis.len());
            // Handle all statements
            for (idx, stmnt) in
                block_data.instructions[stmnt_start..stmnt_end].iter_enumerated().rev()
            {
                analysis.apply_instr_effect(cfg, state, stmnt, idx, block);
            }
            end = block_data.phis.len()
        }

        if start < block_data.phis.len() {
            let start = PhiIdx::from(start);
            let end = PhiIdx::from(end);
            // Handle all statements
            for (phi, phi_data) in block_data.phis[start..end].iter_enumerated().rev() {
                analysis.apply_phi_effect(cfg, state, phi_data, block, phi);
            }
        }
    }

    fn apply_effects_in_block<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData,
    ) where
        A: Analysis,
    {
        if !analysis.init_block(cfg, state) {
            return;
        }
        let terminator = block_data.terminator();
        analysis.apply_terminator_effect(cfg, state, terminator, block);

        for (statement_index, stmnt) in block_data.instructions.iter_enumerated().rev() {
            analysis.apply_instr_effect(cfg, state, stmnt, statement_index, block);
        }

        for (statement_index, phi) in block_data.phis.iter_enumerated().rev() {
            analysis.apply_phi_effect(cfg, state, phi, block, statement_index);
        }
    }

    fn gen_kill_effects_in_block<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        state: &mut GenKillSet<A::Idx>,
        block: BasicBlock,
        block_data: &BasicBlockData,
    ) where
        A: GenKillAnalysis,
    {
        let terminator = block_data.terminator();
        analysis.terminator_effect(cfg, state, terminator, block);

        for (instr_idx, instr) in block_data.instructions.iter_enumerated().rev() {
            analysis.instruction_effect(cfg, state, instr, instr_idx, block);
        }

        for (statement_index, phi) in block_data.phis.iter_enumerated().rev() {
            analysis.phi_effect(cfg, state, phi, block, statement_index);
        }
    }

    fn visit_results_in_block<F, R>(
        state: &mut F,
        cfg: &ControlFlowGraph,
        block: BasicBlock,
        block_data: &BasicBlockData,
        results: &R,
        vis: &mut impl ResultsVisitor<FlowState = F>,
    ) where
        R: ResultsVisitable<FlowState = F>,
    {
        assert!(block < cfg.blocks.next_key());

        results.reset_to_block_entry(cfg, state, block);
        vis.visit_block_end(state, block_data, block);

        vis.visit_terminator_before_effect(state, block_data.terminator(), block);
        results.reconstruct_terminator_effect(cfg, state, block_data.terminator(), block);
        vis.visit_terminator_after_effect(state, block_data.terminator(), block);

        for (phi, phi_data) in cfg.blocks[block].phis.iter_enumerated().rev() {
            vis.visit_phi_before_effect(state, phi_data, block, phi);
            results.reconstruct_phi_effect(cfg, state, phi_data, block, phi);
            vis.visit_phi_after_effect(state, phi_data, block, phi);
        }

        for (id, stmnt) in cfg.blocks[block].instructions.iter_enumerated().rev() {
            vis.visit_instruction_before_effect(state, stmnt, block, id);
            results.reconstruct_instruction_effect(cfg, state, stmnt, block, id);
            vis.visit_instruction_after_effect(state, stmnt, block, id);
        }

        vis.visit_block_start(state, block_data, block)
    }

    fn visit_results_in_block_mut<F, R>(
        state: &mut F,
        cfg: &mut ControlFlowGraph,
        block: BasicBlock,
        results: &R,
        vis: &mut impl ResultsVisitorMut<FlowState = F>,
    ) where
        R: ResultsVisitable<FlowState = F>,
    {
        assert!(block < cfg.blocks.next_key());

        results.reset_to_block_entry(cfg, state, block);
        vis.visit_block_end(state, &mut cfg.blocks[block], block);

        vis.visit_terminator_before_effect(state, cfg.blocks[block].terminator_mut(), block);
        results.reconstruct_terminator_effect(cfg, state, cfg.blocks[block].terminator(), block);
        vis.visit_terminator_after_effect(state, cfg.blocks[block].terminator_mut(), block);

        for instr in cfg.blocks[block].instructions.keys().rev() {
            vis.visit_instruction_before_effect(
                state,
                unsafe { cfg.blocks[block].instructions.get_unchecked_mut(instr) },
                block,
                instr,
            );
            results.reconstruct_instruction_effect(
                cfg,
                state,
                unsafe { cfg.blocks[block].instructions.get_unchecked(instr) },
                block,
                instr,
            );
            vis.visit_instruction_after_effect(
                state,
                unsafe { cfg.blocks[block].instructions.get_unchecked_mut(instr) },
                block,
                instr,
            );
        }

        for phi in cfg.blocks[block].phis.keys().rev() {
            vis.visit_phi_before_effect(
                state,
                unsafe { cfg.blocks[block].phis.get_unchecked_mut(phi) },
                block,
                phi,
            );
            results.reconstruct_phi_effect(
                cfg,
                state,
                unsafe { cfg.blocks[block].phis.get_unchecked(phi) },
                block,
                phi,
            );
            vis.visit_phi_after_effect(
                state,
                unsafe { cfg.blocks[block].phis.get_unchecked_mut(phi) },
                block,
                phi,
            );
        }

        vis.visit_block_start(state, &mut cfg.blocks[block], block)
    }

    fn join_state_into_successors_of<A>(
        _analysis: &mut A,
        cfg: &ControlFlowGraph,
        exit_state: &mut A::Domain,
        block: (BasicBlock, &BasicBlockData),
        mut propagate: impl FnMut(BasicBlock, &A::Domain),
    ) where
        A: Analysis,
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

    fn apply_effects_in_range<A>(
        analysis: &A,
        cfg: &ControlFlowGraph,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData,
        effects: RangeInclusive<EffectIndex>,
    ) where
        A: Analysis,
    {
        let (from, to) = (*effects.start(), *effects.end());
        let terminator_index = block_data.instructions.len() + block_data.phis.len();

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

        if start < block_data.phis.len() {
            let phi_start = PhiIdx::from(start);
            let phi_end = min(PhiIdx::from(end), block_data.phis.next_key());
            // Handle all phis
            for (phi, phi_data) in block_data.phis[phi_start..phi_end].iter_enumerated() {
                analysis.apply_phi_effect(cfg, state, phi_data, block, phi);
            }

            start = block_data.phis.len()
        }

        if end > block_data.phis.len() {
            let start = InstIdx::from(start - block_data.phis.len());
            let end = InstIdx::from(end - block_data.phis.len());
            // Handle all statements
            for (idx, stmnt) in block_data.instructions[start..end].iter_enumerated() {
                analysis.apply_instr_effect(cfg, state, stmnt, idx, block);
            }
        }

        if to.idx == terminator_index && to.effect == Effect::After {
            analysis.apply_terminator_effect(cfg, state, block_data.terminator(), block);
        }
    }

    fn apply_effects_in_block<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        state: &mut A::Domain,
        block: BasicBlock,
        block_data: &BasicBlockData,
    ) where
        A: Analysis,
    {
        if !analysis.init_block(cfg, state) {
            return;
        }
        for (statement_index, phi) in block_data.phis.iter_enumerated() {
            analysis.apply_phi_effect(cfg, state, phi, block, statement_index);
        }

        for (statement_index, stmnt) in block_data.instructions.iter_enumerated() {
            analysis.apply_instr_effect(cfg, state, stmnt, statement_index, block);
        }

        let terminator = block_data.terminator();
        analysis.apply_terminator_effect(cfg, state, terminator, block);
    }

    fn gen_kill_effects_in_block<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        state: &mut GenKillSet<A::Idx>,
        block: BasicBlock,
        block_data: &BasicBlockData,
    ) where
        A: GenKillAnalysis,
    {
        for (phi_idx, phi) in block_data.phis.iter_enumerated() {
            analysis.phi_effect(cfg, state, phi, block, phi_idx);
        }

        for (instr_idx, instr) in block_data.instructions.iter_enumerated() {
            analysis.instruction_effect(cfg, state, instr, instr_idx, block);
        }

        let terminator = block_data.terminator();
        analysis.terminator_effect(cfg, state, terminator, block);
    }

    fn visit_results_in_block<F, R>(
        state: &mut F,
        cfg: &ControlFlowGraph,
        block: BasicBlock,
        block_data: &BasicBlockData,
        results: &R,
        vis: &mut impl ResultsVisitor<FlowState = F>,
    ) where
        R: ResultsVisitable<FlowState = F>,
    {
        assert!(block < cfg.blocks.next_key());

        if !results.reset_to_block_entry(cfg, state, block) {
            return;
        }

        vis.visit_block_start(state, block_data, block);

        for (id, phi) in cfg.blocks[block].phis.iter_enumerated() {
            vis.visit_phi_before_effect(state, phi, block, id);
            results.reconstruct_phi_effect(cfg, state, phi, block, id);
            vis.visit_phi_after_effect(state, phi, block, id);
        }

        for (id, instr) in cfg.blocks[block].instructions.iter_enumerated() {
            vis.visit_instruction_before_effect(state, instr, block, id);
            results.reconstruct_instruction_effect(cfg, state, instr, block, id);
            vis.visit_instruction_after_effect(state, instr, block, id);
        }

        vis.visit_terminator_before_effect(state, block_data.terminator(), block);
        results.reconstruct_terminator_effect(cfg, state, block_data.terminator(), block);
        vis.visit_terminator_after_effect(state, block_data.terminator(), block);

        vis.visit_block_end(state, block_data, block);
    }

    fn visit_results_in_block_mut<F, R>(
        state: &mut F,
        cfg: &mut ControlFlowGraph,
        block: BasicBlock,
        results: &R,
        vis: &mut impl ResultsVisitorMut<FlowState = F>,
    ) where
        R: ResultsVisitable<FlowState = F>,
    {
        assert!(block < cfg.blocks.next_key());

        if !results.reset_to_block_entry(cfg, state, block) {
            return;
        }
        vis.visit_block_start(state, &mut cfg.blocks[block], block);

        for phi in cfg.blocks[block].phis.keys() {
            vis.visit_phi_before_effect(
                state,
                unsafe { cfg.blocks[block].phis.get_unchecked_mut(phi) },
                block,
                phi,
            );
            results.reconstruct_phi_effect(
                cfg,
                state,
                unsafe { cfg.blocks[block].phis.get_unchecked(phi) },
                block,
                phi,
            );
            vis.visit_phi_after_effect(
                state,
                unsafe { cfg.blocks[block].phis.get_unchecked_mut(phi) },
                block,
                phi,
            );
        }

        for stmnt in cfg.blocks[block].instructions.keys() {
            vis.visit_instruction_before_effect(
                state,
                unsafe { cfg.blocks[block].instructions.get_unchecked_mut(stmnt) },
                block,
                stmnt,
            );
            results.reconstruct_instruction_effect(
                cfg,
                state,
                unsafe { cfg.blocks[block].instructions.get_unchecked(stmnt) },
                block,
                stmnt,
            );
            vis.visit_instruction_after_effect(
                state,
                unsafe { cfg.blocks[block].instructions.get_unchecked_mut(stmnt) },
                block,
                stmnt,
            );
        }

        vis.visit_terminator_before_effect(state, cfg.blocks[block].terminator_mut(), block);
        results.reconstruct_terminator_effect(cfg, state, cfg.blocks[block].terminator(), block);
        vis.visit_terminator_after_effect(state, cfg.blocks[block].terminator_mut(), block);

        vis.visit_block_end(state, &mut cfg.blocks[block], block);
    }

    fn join_state_into_successors_of<A>(
        analysis: &mut A,
        cfg: &ControlFlowGraph,
        exit_state: &mut A::Domain,
        block: (BasicBlock, &BasicBlockData),
        mut propagate: impl FnMut(BasicBlock, &A::Domain),
    ) where
        A: Analysis,
    {
        match *block.1.terminator() {
            Terminator::Goto(succ) => {
                if analysis.apply_edge_effects(cfg, block.0, exit_state) {
                    propagate(succ, exit_state)
                }
            }
            Terminator::Split { ref condition, true_block, false_block, .. } => {
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

                let SplitEdgeEffectApplier { exit_state, mut propagate, effects_applied, .. } =
                    applier;

                if !effects_applied {
                    propagate(false_block, exit_state);
                    propagate(true_block, exit_state);
                }
            }
            Terminator::Ret => {}
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

        tmp.clone_from(self.exit_state);
        if apply_edge_effect(&mut tmp, self.false_block, false) {
            (self.propagate)(self.false_block, &tmp);
        }

        self.effects_applied = true;
    }
}
