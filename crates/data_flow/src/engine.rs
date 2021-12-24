//! A solver for dataflow problems.

use std::borrow::BorrowMut;
use std::fmt::{self, Debug, Formatter};

use cfg::{BasicBlock, BasicBlockData, ControlFlowGraph};
use stdx::iter::zip;
use typed_index_collections::TiVec;
use workqueue::WorkQueue;

use crate::direction::Direction;
use crate::lattice::JoinSemiLattice;
use crate::visitor::{visit_results, visit_results_mut};
use crate::{
    Analysis, GenKillAnalysis, GenKillAnalysisImpl, GenKillSet, ResultsCursor, ResultsRefCursor,
    ResultsVisitable, ResultsVisitor, ResultsVisitorMut,
};

pub type GenKillResults<A> = Results<GenKillAnalysisImpl<A>>;

/// A dataflow analysis that has converged to fixpoint.
#[derive(Clone)]
pub struct Results<A>
where
    A: Analysis,
{
    pub analysis: A,
    pub entry_sets: TiVec<BasicBlock, A::Domain>,
}

impl<A> Results<A>
where
    A: Analysis,
{
    /// Creates a `ResultsCursor` that can inspect these `Results`.
    pub fn into_results_cursor(self, cfg: &ControlFlowGraph) -> ResultsCursor<A> {
        ResultsCursor::new(cfg, self)
    }

    /// Creates a `ResultsCursor` that can inspect these `Results`.
    pub fn as_results_cursor(&self, cfg: &ControlFlowGraph) -> ResultsRefCursor<A> {
        ResultsRefCursor::new(cfg, self)
    }

    /// Gets the dataflow state for the given block.
    pub fn entry_set_for_block(&self, block: BasicBlock) -> &A::Domain {
        &self.entry_sets[block]
    }

    pub fn visit_in_blocks_with<'a>(
        &self,
        cfg: &'a ControlFlowGraph,
        blocks: impl IntoIterator<Item = (BasicBlock, &'a BasicBlockData)>,
        vis: &mut impl ResultsVisitor<FlowState = <Self as ResultsVisitable>::FlowState>,
    ) {
        visit_results(cfg, blocks, self, vis)
    }

    pub fn visit_in_blocks_with_mut(
        &self,
        cfg: &mut ControlFlowGraph,
        blocks: impl IntoIterator<Item = BasicBlock>,
        vis: &mut impl ResultsVisitorMut<FlowState = <Self as ResultsVisitable>::FlowState>,
    ) {
        visit_results_mut(cfg, blocks, self, vis)
    }

    pub fn visit_with(
        &self,
        cfg: &ControlFlowGraph,
        vis: &mut impl ResultsVisitor<FlowState = <Self as ResultsVisitable>::FlowState>,
    ) {
        self.visit_in_blocks_with(cfg, cfg.blocks.iter_enumerated(), vis)
    }

    pub fn visit_with_mut(
        &self,
        cfg: &mut ControlFlowGraph,
        vis: &mut impl ResultsVisitorMut<FlowState = <Self as ResultsVisitable>::FlowState>,
    ) {
        self.visit_in_blocks_with_mut(cfg, cfg.blocks.keys(), vis)
    }
}

impl<A> Debug for Results<A>
where
    A: Analysis,
    A::Domain: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.entry_sets, f)
    }
}

/// A solver for dataflow problems.
pub struct Engine<'a, A>
where
    A: Analysis,
{
    cfg: &'a ControlFlowGraph,
    entry_sets: TiVec<BasicBlock, A::Domain>,
    analysis: A,

    /// Cached, cumulative transfer functions for each block.
    //
    // FIXME: This boxed `Fn` trait object is invoked inside a tight loop for
    // gen/kill problems on cyclic CFGs. This is not ideal, but it doesn't seem to degrade
    // performance in practice. I've tried a few ways to avoid this, but they have downsides
    #[allow(clippy::type_complexity)]
    apply_trans_for_block: Option<Box<dyn Fn(BasicBlock, &mut A::Domain)>>,
}

impl<'a, A, T> Engine<'a, GenKillAnalysisImpl<A>>
where
    A: GenKillAnalysis<Idx = T>,
    T: From<usize> + Into<usize> + Copy + PartialEq + Debug + 'static,
{
    /// Creates a new `Engine` to solve a gen-kill dataflow problem.
    pub fn new_gen_kill(cfg: &'a ControlFlowGraph, mut analysis: A) -> Self {
        // If there are no back-edges in the control-flow graph, we only ever need to apply the
        // transfer function for each block exactly once (assuming that we process blocks in RPO).
        //
        // In this case, there's no need to compute the block transfer functions ahead of time.
        if !cfg.is_cyclic() {
            // debug!("Non Cyclical CFG! Gen Kill Transfer Functions are not cached");
            return Self::new(cfg, GenKillAnalysisImpl(analysis), None);
        }

        // debug!("Cyclical CFG! Caching gen kill sets");

        // Otherwise, compute and store the cumulative transfer function for each block.

        let identity = GenKillSet::identity(analysis.domain_size(cfg));
        let mut trans_for_block = TiVec::from(vec![identity; cfg.blocks.len()]);

        for ((block, block_data), trans) in zip(cfg.blocks.iter_enumerated(), &mut trans_for_block)
        {
            A::Direction::gen_kill_effects_in_block(&mut analysis, cfg, trans, block, block_data);
        }

        Self::new(
            cfg,
            GenKillAnalysisImpl(analysis),
            Some(Box::new(move |bb, dst| trans_for_block[bb].apply(dst.borrow_mut()))),
        )
    }
}

impl<'a, A, D> Engine<'a, A>
where
    A: Analysis<Domain = D>,
    D: Clone + JoinSemiLattice + Debug,
{
    /// Creates a new `Engine` to solve a dataflow problem with an arbitrary transfer
    /// function.
    ///
    /// Gen-kill problems should use `new_gen_kill`, which will coalesce transfer functions for
    /// better performance.
    pub fn new_generic(cfg: &'a ControlFlowGraph, analysis: A) -> Self {
        Self::new(cfg, analysis, None)
    }

    #[allow(clippy::type_complexity)]
    fn new(
        cfg: &'a ControlFlowGraph,
        analysis: A,
        apply_trans_for_block: Option<Box<dyn Fn(BasicBlock, &mut A::Domain)>>,
    ) -> Self {
        let bottom_value = analysis.bottom_value(cfg);
        let mut entry_sets = TiVec::from(vec![bottom_value; cfg.blocks.len()]);
        let start =
            if A::Direction::IS_FORWARD { cfg.entry() } else { cfg.blocks.last_key().unwrap() };

        analysis.initialize_start_block(cfg, &mut entry_sets[start]);

        Engine { cfg, entry_sets, analysis, apply_trans_for_block }
    }

    /// Computes the fixpoint for this dataflow problem and returns it.
    pub fn iterate_to_fixpoint(self) -> Results<A>
//  where
  //      A::Domain: DebugWithContext<A>,
    {
        let Engine { mut analysis, cfg, mut entry_sets, apply_trans_for_block, .. } = self;

        let mut dirty_queue: WorkQueue<BasicBlock> = WorkQueue::with_none(cfg.blocks.len());

        // Iterating the whole graph garuntees nice iteration order.
        // However it also forces DFA to run over the whole graph even for analysis modes where it could be avoided (eg Condition Constants where unreachable sets may never be visited at all)
        // TODO determine if improving this might be worthwhile (I would guess not but CC is very slow)
        if A::Direction::IS_FORWARD {
            dirty_queue.extend(cfg.reverse_postorder())
        } else {
            // Reverse post-order on the reverse CFG may generate a better iteration order for
            // backward dataflow analyses, but probably not enough to matter.
            dirty_queue.extend(cfg.postorder_iter().map(|(bb, _)| bb))
        }

        // `state` is not actually used between iterations;
        // this is just an optimization to avoid reallocating every iteration.
        let mut state = analysis.bottom_value(cfg);
        while let Some(bb) = dirty_queue.pop() {
            let bb_data = &cfg[bb];

            // Set the state to the entry state of the block.
            // This is equivalent to `state = entry_sets[bb].clone()`,
            // but it saves an allocation, thus improving compile times.
            state.clone_from(&entry_sets[bb]);

            // Apply the block transfer function, using the cached one if it exists.
            match &apply_trans_for_block {
                Some(apply) => apply(bb, &mut state),
                None => A::Direction::apply_effects_in_block(
                    &mut analysis,
                    cfg,
                    &mut state,
                    bb,
                    bb_data,
                ),
            }

            A::Direction::join_state_into_successors_of(
                &mut analysis,
                cfg,
                &mut state,
                (bb, bb_data),
                |target: BasicBlock, state: &A::Domain| {
                    let set_changed = entry_sets[target].join(state);
                    if set_changed {
                        dirty_queue.insert(target);
                    }
                },
            );
        }

        Results { analysis, entry_sets }
    }
}

// TODO pretty printing
