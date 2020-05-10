//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::compact_arena::{invariant_lifetime, TinyHeapArena};
use crate::ir::mir::ControlFlowGraph;
use fixedbitset::FixedBitSet as BitSet;

pub struct Graph<'cfg> {
    pub in_sets: TinyHeapArena<'cfg, BitSet>,
    pub out_sets: TinyHeapArena<'cfg, BitSet>,
}

impl<'cfg> Graph<'cfg> {
    pub fn new<'mir>(cfg: &ControlFlowGraph<'cfg, 'mir>, set_size: usize) -> Self {
        unsafe {
            // This is save since we are creating associated data
            let data = TinyHeapArena::new_with(invariant_lifetime(), cfg.block_count(), || {
                BitSet::with_capacity(set_size)
            });
            Self {
                in_sets: data.clone(),
                out_sets: data,
            }
        }
    }

    pub fn reborrow_for_subcfg<'newtag, 'lt, 'mir>(
        &'lt self,
        cfg: &ControlFlowGraph<'newtag, 'mir>,
    ) -> &'lt Graph<'newtag> {
        assert!(cfg.block_count() <= self.in_sets.len());
        unsafe { std::mem::transmute(self) }
    }
}
