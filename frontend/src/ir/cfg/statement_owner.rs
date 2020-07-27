/*

 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
*/

//! Lazily compute the `StatementId` -> `BasicBlockId` mapping

use crate::cfg::ControlFlowGraph;
use crate::ir::cfg::BasicBlockId;
use crate::ir::ids::StatementId;
use index_vec::{index_vec, IndexVec};
use once_cell::unsync::OnceCell;

pub type StatementOwners = IndexVec<StatementId, Option<BasicBlockId>>;

#[derive(Clone, Debug, Default)]
pub(crate) struct StatementOwnerCache {
    cache: OnceCell<StatementOwners>,
    pub(crate) stmt_count: usize,
}

impl StatementOwnerCache {
    #[inline]
    pub(super) fn new(stmt_count: usize) -> Self {
        StatementOwnerCache {
            cache: OnceCell::new(),
            stmt_count,
        }
    }

    /// Invalidates the predecessor cache.
    #[inline]
    pub(super) fn invalidate_internal(&mut self) {
        // Invalidating the predecessor cache requires mutating the CFG.
        // Because of this, we can assume that all callers of `invalidate` have a unique reference
        // to the CFG and thus to the predecessor cache. This means we never need to do synchronization when `invalidate` is called, we can
        // simply reinitialize the `OnceCell`.
        self.cache = OnceCell::new();
    }

    #[inline]
    pub fn invalidate(&mut self, stmt_count: usize) {
        self.stmt_count = stmt_count;
        self.invalidate_internal();
    }

    /// Returns the the predecessor graph for this MIR.
    #[inline]
    pub(super) fn compute(&self, cfg: &ControlFlowGraph) -> &StatementOwners {
        self.cache.get_or_init(|| {
            let mut owner = index_vec![None;self.stmt_count];
            for (id, bb) in cfg.blocks.iter_enumerated() {
                for stmt in bb.statements.iter().copied() {
                    debug_assert_eq!(owner[stmt], None);
                    owner[stmt] = Some(id);
                }
            }
            owner
        })
    }
}
