//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::mem::swap;

use crate::analysis::data_flow::framework::{Analysis, DataFlowGraph, Direction};
use crate::data_structures::BitSet;
use crate::ir::cfg::ControlFlowGraph;
use log::trace;

/// A worklist based DFA solver
pub struct Engine<'lt, A: Analysis<'lt>> {
    pub analysis: &'lt mut A,
    pub dfg: DataFlowGraph<A::SetType>,
    pub cfg: &'lt ControlFlowGraph,
}

impl<'lt, A: Analysis<'lt>> Engine<'lt, A> {
    pub fn new(cfg: &'lt ControlFlowGraph, analysis: &'lt mut A) -> Self {
        Self {
            dfg: DataFlowGraph::new(analysis.max_idx(), cfg),
            analysis,
            cfg,
        }
    }

    /// Runs the `analysis` until a fix point is reached
    ///
    /// A fixepoint is reachched when no more elements are in the worklist
    /// The [worklist](crate::data_structures::WorkQueue) is initialized using [`Direction:inital_work_queue`](crate::analysis::data_flow::framework::Direction::inital_work_queue)
    /// Basic blocks are then removed, their transfer function is executed and they are propagated to any dependent blocks where their outset is joined into their inset.
    /// If the destination insets changed the elements are added back to the worklist
    ///
    #[must_use]
    pub fn iterate_to_fixpoint(mut self) -> DataFlowGraph<A::SetType> {
        let mut worklist =
            <<A as Analysis<'lt>>::Direction as Direction<'lt>>::inital_work_queue(self.cfg);
        let mut temporary_set = BitSet::new_empty(self.analysis.max_idx());
        trace!("Data Flow Engine: Starting with worklist: {:?}", worklist);
        while let Some(bb) = worklist.pop() {
            trace!("Data Flow Engine: Processing {:?}", bb);
            self.analysis.transfer_function(
                &self.dfg.in_sets[bb],
                &mut temporary_set,
                bb,
                self.cfg,
            );
            if temporary_set != self.dfg.out_sets[bb] {
                swap(&mut temporary_set, &mut self.dfg.out_sets[bb]);

                A::Direction::propagate_result(bb, self.cfg, |dst| {
                    worklist.insert(dst);
                    self.analysis
                        .join(&mut self.dfg.in_sets[dst], &self.dfg.out_sets[bb])
                });
            }
        }
        self.dfg
    }
}
