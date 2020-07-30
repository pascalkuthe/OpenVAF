//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::cfg::{BasicBlockId, ControlFlowGraph};
use index_vec::{index_vec, IndexVec};

pub struct Graph<Set> {
    pub in_sets: IndexVec<BasicBlockId, Set>,
    pub out_sets: IndexVec<BasicBlockId, Set>,
}

impl<Set: Clone> Graph<Set> {
    pub fn new(empty_set: Set, cfg: &ControlFlowGraph) -> Self {
        let in_sets = index_vec![empty_set;cfg.blocks.len()];
        Self {
            out_sets: in_sets.clone(),
            in_sets,
        }
    }
}
