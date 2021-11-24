//! Lazily compute the reverse control-flow graph for the MIR.

use once_cell::sync::OnceCell;
use typed_index_collections::{TiSlice, TiVec};

use crate::{BasicBlock, BasicBlockData};

pub type Predecessors = TiVec<BasicBlock, Vec<BasicBlock>>;

#[derive(Clone, Debug, Default)]
pub struct PredecessorCache {
    cache: OnceCell<Predecessors>,
}

impl PredecessorCache {
    #[inline]
    pub fn new() -> Self {
        PredecessorCache { cache: OnceCell::new() }
    }

    /// Invalidates the predecessor cache.
    #[inline]
    pub fn invalidate(&mut self) {
        // Invalidating the predecessor cache requires mutating the CFG.
        // Because of this, we can assume that all callers of `invalidate` have a unique reference
        // to the CFG and thus to the predecessor cache. This means we never need to do synchronization when `invalidate` is called, we can
        // simply reinitialize the `OnceCell`.
        self.cache = OnceCell::new();
    }

    /// Returns the the predecessor graph for this CFG.
    #[inline]
    pub(super) fn compute(
        &self,
        basic_blocks: &TiSlice<BasicBlock, BasicBlockData>,
    ) -> &Predecessors {
        self.cache.get_or_init(|| {
            // before optimizations every CFG Block has at most two predecessors
            let mut preds = TiVec::from(vec![Vec::with_capacity(2); basic_blocks.len()]);
            for (id, bb) in basic_blocks.iter_enumerated() {
                for succ in bb.successors() {
                    preds[succ].push(id);
                }
            }

            preds
        })
    }
}
