//! Random access inspection of the results of a dataflow analysis.

use std::borrow::Borrow;
use std::cmp::Ordering;

use cfg::{BasicBlock, ControlFlowGraph, Location, LocationKind};

use crate::{Analysis, Direction, Effect, EffectIndex, GenKillAnalysisImpl, Results};

/// A `ResultsCursor` that borrows the underlying `Results`.
pub type ResultsRefCursor<'a, A> = ResultsCursor<A, &'a Results<A>>;

pub type GenKillResultsCursor<'a, A> =
    ResultsCursor<GenKillAnalysisImpl<A>, Results<GenKillAnalysisImpl<A>>>;
pub type GenKillResultsRefCursor<'a, A> =
    ResultsCursor<GenKillAnalysisImpl<A>, &'a Results<GenKillAnalysisImpl<A>>>;

/// Allows random access inspection of the results of a dataflow analysis.
///
/// This cursor only has linear performance within a basic block when its statements are visited in
/// the same order as the `DIRECTION` of the analysis. In the worst case—when statements are
/// visited in *reverse* order—performance will be quadratic in the number of statements in the
/// block. The order in which basic blocks are inspected has no impact on performance.
///
/// A `ResultsCursor` can either own (the default) or borrow the dataflow results it inspects. The
/// type of ownership is determined by `R` (see `ResultsRefCursor` above).
#[derive(Debug, Clone)]
pub struct ResultsCursor<A, R = Results<A>>
where
    A: Analysis,
{
    results: R,
    state: A::Domain,

    pos: CursorPosition,

    /// Indicates that `state` has been modified with a custom effect.
    ///
    /// When this flag is set, we need to reset to an entry set before doing a seek.
    state_needs_reset: bool,
}

impl<A, R> ResultsCursor<A, R>
where
    A: Analysis,
    R: Borrow<Results<A>>,
{
    /// Returns a new cursor that can inspect `results`.
    pub fn new(cfg: &ControlFlowGraph, results: R) -> Self {
        let bottom_value = results.borrow().analysis.bottom_value(cfg);
        ResultsCursor {
            results,

            // Initialize to the `bottom_value` and set `state_needs_reset` to tell the cursor that
            // it needs to reset to block entry before the first seek. The cursor position is
            // immaterial.
            state_needs_reset: true,
            state: bottom_value,
            pos: CursorPosition::block_entry(cfg.entry()),
        }
    }

    /// Returns the underlying `Results`.
    pub fn results(&self) -> &Results<A> {
        self.results.borrow()
    }

    /// Returns the `Analysis` used to generate the underlying `Results`.
    pub fn analysis(&self) -> &A {
        &self.results.borrow().analysis
    }

    /// Returns the dataflow state at the current location.
    pub fn get(&self) -> &A::Domain {
        &self.state
    }

    pub fn finish(self) -> A::Domain {
        self.state
    }

    /// Resets the cursor to hold the entry set for the given basic block.
    ///
    /// For forward dataflow analyses, this is the dataflow state prior to the first statement.
    ///
    /// For backward dataflow analyses, this is the dataflow state after the terminator.
    pub(super) fn seek_to_block_entry(&mut self, block: BasicBlock, cfg: &ControlFlowGraph) {
        self.state.clone_from(self.results.borrow().entry_set_for_block(block));
        self.results.borrow().analysis.init_block(cfg, &mut self.state);
        self.pos = CursorPosition::block_entry(block);
        self.state_needs_reset = false;
    }

    /// Resets the cursor to hold the state prior to the first statement in a basic block.
    ///
    /// For forward analyses, this is the entry set for the given block.
    ///
    /// For backward analyses, this is the state that will be propagated to its
    /// predecessors (ignoring edge-specific effects).
    pub fn seek_to_block_start(&mut self, block: BasicBlock, cfg: &ControlFlowGraph) {
        if A::Direction::IS_FORWARD {
            self.seek_to_block_entry(block, cfg)
        } else {
            self.seek(Effect::After.at_index(0), block, cfg)
        }
    }

    /// Resets the cursor to hold the state after the terminator in a basic block.
    ///
    /// For backward analyses, this is the entry set for the given block.
    ///
    /// For forward analyses, this is the state that will be propagated to its
    /// successors (ignoring edge-specific effects).
    pub fn seek_to_block_end(&mut self, block: BasicBlock, cfg: &ControlFlowGraph) {
        if A::Direction::IS_FORWARD {
            self.seek_after_effect(Location { bb: block, kind: LocationKind::Terminator }, cfg)
        } else {
            self.seek_to_block_entry(block, cfg)
        }
    }

    // /// Resets the cursor to hold the state after the terminator at the exit block of the cfg.
    // pub fn seek_to_exit_block_end(&mut self, cfg: &ControlFlowGraph) {
    //     self.seek_to_block_end(cfg.exit(), cfg)
    // }

    /// Advances the cursor to hold the dataflow state at `target` after its effect is
    /// applied.
    pub fn seek_after_effect(&mut self, target: Location, cfg: &ControlFlowGraph) {
        self.seek(Effect::After.at_location(target, cfg), target.bb, cfg)
    }

    /// Advances the cursor to hold the dataflow state at `target` after its effect is
    /// applied.
    pub fn seek_before_effect(&mut self, target: Location, cfg: &ControlFlowGraph) {
        self.seek(Effect::Before.at_location(target, cfg), target.bb, cfg)
    }

    fn seek(&mut self, target: EffectIndex, block: BasicBlock, cfg: &ControlFlowGraph) {
        // Reset to the entry of the target block if any of the following are true:
        //   - A custom effect has been applied to the cursor state.
        //   - We are in a different block than the target.
        //   - We are in the same block but have advanced past the target effect.
        if self.state_needs_reset || self.pos.block != block {
            self.seek_to_block_entry(block, cfg);
        } else if let Some(curr_effect) = self.pos.curr_effect_index {
            let mut ord = curr_effect.idx.cmp(&target.idx);
            if !A::Direction::IS_FORWARD {
                ord = ord.reverse()
            }

            match ord.then_with(|| curr_effect.effect.cmp(&target.effect)) {
                Ordering::Equal => return,
                Ordering::Greater => self.seek_to_block_entry(block, cfg),
                Ordering::Less => {}
            }
        }

        // At this point, the cursor is in the same block as the target location at an earlier
        // statement.
        debug_assert_eq!(block, self.pos.block);

        let block_data = &cfg.blocks[block];
        let next_effect = if A::Direction::IS_FORWARD {
            self.pos
                .curr_effect_index
                .map_or_else(|| Effect::Before.at_index(0), EffectIndex::next_in_forward_order)
        } else {
            self.pos.curr_effect_index.map_or_else(
                || Effect::Before.at_index(block_data.instructions.len()),
                EffectIndex::next_in_backward_order,
            )
        };

        let analysis = &self.results.borrow().analysis;

        A::Direction::apply_effects_in_range(
            analysis,
            cfg,
            &mut self.state,
            block,
            block_data,
            next_effect..=target,
        );

        self.pos = CursorPosition { block, curr_effect_index: Some(target) };
    }

    /// Applies `f` to the cursor's internal state.
    ///
    /// This can be used, e.g., to apply the call return effect directly to the cursor without
    /// creating an extra copy of the dataflow state.
    pub fn apply_custom_effect(&mut self, f: impl FnOnce(&A, &mut A::Domain)) {
        f(&self.results.borrow().analysis, &mut self.state);
        self.state_needs_reset = true;
    }
}

#[derive(Clone, Copy, Debug)]
struct CursorPosition {
    block: BasicBlock,
    curr_effect_index: Option<EffectIndex>,
}

impl CursorPosition {
    fn block_entry(block: BasicBlock) -> CursorPosition {
        CursorPosition { block, curr_effect_index: None }
    }
}
