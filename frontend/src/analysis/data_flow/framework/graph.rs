//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::cfg::{BasicBlockId, ControlFlowGraph};
use crate::data_structures::BitSet;
use index_vec::{index_vec, Idx, IndexVec};

pub struct Graph<SetType: Idx + From<usize>> {
    pub in_sets: IndexVec<BasicBlockId, BitSet<SetType>>,
    pub out_sets: IndexVec<BasicBlockId, BitSet<SetType>>,
}

impl<SetType: Idx + From<usize>> Graph<SetType> {
    pub fn new(max_id: SetType, cfg: &ControlFlowGraph) -> Self {
        let in_sets = index_vec![BitSet::new_empty(max_id);cfg.blocks.len()];
        Self {
            out_sets: in_sets.clone(),
            in_sets,
        }
    }
}
