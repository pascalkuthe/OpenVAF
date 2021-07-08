/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! Lazily compute the reverse control-flow graph for the MIR.

use crate::cfg::{BasicBlock, BasicBlockData};
use crate::CfgFunctions;
use openvaf_data_structures::index_vec::{index_vec, IndexVec};
use openvaf_data_structures::sync::OnceCell;

pub type Predecessors = IndexVec<BasicBlock, Vec<BasicBlock>>;

#[derive(Clone, Debug, Default)]
pub struct PredecessorCache {
    cache: OnceCell<Predecessors>,
}

impl PredecessorCache {
    #[inline]
    pub fn new() -> Self {
        PredecessorCache {
            cache: OnceCell::new(),
        }
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
    pub(super) fn compute<C: CfgFunctions>(
        &self,
        basic_blocks: &IndexVec<BasicBlock, BasicBlockData<C>>,
    ) -> &Predecessors {
        self.cache.get_or_init(|| {
            // by default every CFG Block has at most two predecessors
            let mut preds = index_vec![Vec::with_capacity(2);basic_blocks.len()];
            for (id, bb) in basic_blocks.iter_enumerated() {
                for succ in bb.successors() {
                    preds[succ].push(id);
                }
            }

            preds
        })
    }
}
