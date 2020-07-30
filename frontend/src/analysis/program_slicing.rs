//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::ProgramDependenceGraph;
use crate::cfg::ControlFlowGraph;
use crate::data_structures::{BitSet, HybridBitSet, WorkQueue};
use crate::ir::ids::{StatementId, VariableId};
use log::{debug, trace};
use std::collections::VecDeque;
use std::iter::FromIterator;

impl ControlFlowGraph {
    pub fn backward_variable_slice(&mut self, var: VariableId, pdg: &ProgramDependenceGraph) {
        self.backward_variable_slice_assuming_input(
            HybridBitSet::new_empty(StatementId::from_raw_unchecked(0)),
            var,
            pdg,
        )
    }

    pub fn backward_variable_slice_assuming_input(
        &mut self,
        input: HybridBitSet<StatementId>,
        var: VariableId,
        pdg: &ProgramDependenceGraph,
    ) {
        if let Some(relevant_stmts) = pdg.data_dependencies.assignments[var].clone() {
            self.backward_slice(relevant_stmts.into_dense(), input.into_dense(), pdg);
        } else {
            self.blocks.clear();
            self.statement_owner_cache
                .invalidate(pdg.data_dependencies.stmt_len_idx().index());
            self.predecessor_cache.invalidate();
        }
    }

    pub fn backward_variable_slice_with_variables_as_input(
        &mut self,
        input: impl Iterator<Item = VariableId>,
        var: VariableId,
        pdg: &ProgramDependenceGraph,
    ) {
        let mut input_statements = HybridBitSet::new_empty(pdg.data_dependencies.stmt_len_idx());
        for var in input {
            if let Some(assignments) = &pdg.data_dependencies.assignments[var] {
                input_statements.union_with(assignments)
            }
        }

        self.backward_variable_slice_assuming_input(input_statements, var, pdg)
    }

    pub fn backward_slice(
        &mut self,
        mut relevant_stmts: BitSet<StatementId>,
        assumed_stmts: BitSet<StatementId>,
        pdg: &ProgramDependenceGraph,
    ) {
        debug!(
            "Computing backwardslice wit intput {} and output {}",
            assumed_stmts, relevant_stmts
        );
        // The assumed stmts are marked as visited so they wont be inserted into the work queue
        let mut set = assumed_stmts;
        // The relevant stmts are added to the work queue
        set.grow(relevant_stmts.len_idx());
        let mut stmt_work_queue = WorkQueue {
            // Add all relevant stmts to the work queue
            deque: VecDeque::from_iter(relevant_stmts.ones()),
            set,
        };

        debug!(
            "Backwardslice: Inital stmt work-list: {:?}",
            stmt_work_queue
        );

        let mut bb_work_queue = WorkQueue::with_none(self.blocks.len_idx());

        loop {
            let mut done = true;

            if let Some(stmt) = stmt_work_queue.take() {
                relevant_stmts.insert(stmt);

                if let Some(data_dependencies) = &pdg.data_dependencies.stmt_use_def_chains[stmt] {
                    for data_dependency in data_dependencies.ones() {
                        stmt_work_queue.insert(data_dependency);
                    }
                }

                if let Some(control_dependencies) = &pdg.control_dependencies[self
                    .containing_block(stmt)
                    .expect("Mallformed cfg statement owners")]
                {
                    for control_dependency in control_dependencies.ones() {
                        bb_work_queue.insert(control_dependency);
                    }
                }

                trace!(
                    "Backwardslice: stmt work-list after iteration {:?}",
                    stmt_work_queue
                );

                done = false;
            }

            if let Some(bb) = bb_work_queue.take() {
                if let Some(data_dependencies) =
                    &pdg.data_dependencies.terminator_use_def_chains[bb]
                {
                    for data_dependency in data_dependencies.ones() {
                        stmt_work_queue.insert(data_dependency);
                    }
                }

                if let Some(control_dependencies) = &pdg.control_dependencies[bb] {
                    for control_dependency in control_dependencies.ones() {
                        bb_work_queue.insert(control_dependency);
                    }
                }

                done = false;
            }

            if done {
                break;
            }
        }

        for bb in self.blocks.iter_mut() {
            bb.statements.retain(|&stmt| relevant_stmts.contains(stmt))
        }
    }
}
