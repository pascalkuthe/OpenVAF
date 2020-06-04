/*

 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************

 Adapted from https://github.com/rust-lang/rust src/librustc_middle/mir/predecessors.rs under MIT-License

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
    ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
    TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
    SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
    IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
*/

//! Lazily compute the reverse control-flow graph for the MIR.

use crate::ir::cfg::{BasicBlock, BasicBlockId};
use index_vec::*;
use once_cell::unsync::OnceCell;

pub type Predecessors = IndexVec<BasicBlockId, Vec<BasicBlockId>>;

#[derive(Clone, Debug)]
pub(crate) struct PredecessorCache {
    cache: OnceCell<Predecessors>,
}

impl PredecessorCache {
    #[inline]
    pub(super) fn new() -> Self {
        PredecessorCache {
            cache: OnceCell::new(),
        }
    }

    /// Invalidates the predecessor cache.
    #[inline]
    pub(super) fn invalidate(&mut self) {
        // Invalidating the predecessor cache requires mutating the CFG.
        // Because of this, we can assume that all callers of `invalidate` have a unique reference
        // to the CFG and thus to the predecessor cache. This means we never need to do synchronization when `invalidate` is called, we can
        // simply reinitialize the `OnceCell`.
        self.cache = OnceCell::new();
    }

    /// Returns the the predecessor graph for this MIR.
    #[inline]
    pub(super) fn compute(
        &self,
        basic_blocks: &IndexVec<BasicBlockId, BasicBlock>,
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
