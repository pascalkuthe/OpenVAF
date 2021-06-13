/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::ProgramDependenceGraph;
use openvaf_data_structures::{BitSet, WorkQueue};
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::{impl_pass_span, CallType, Local};
use std::collections::VecDeque;
use std::iter::FromIterator;
use tracing::{debug, trace, trace_span};

pub struct BackwardSlice<'a> {
    pub relevant_locations: BitSet<IntLocation>,
    pub assumed_locations: BitSet<IntLocation>,
    pub pdg: &'a ProgramDependenceGraph,
    pub locations: &'a InternedLocations,
}
impl<'a> BackwardSlice<'a> {
    pub fn new(pdg: &'a ProgramDependenceGraph, locations: &'a InternedLocations) -> Self {
        Self {
            relevant_locations: BitSet::new_empty(locations.len_idx()),
            assumed_locations: BitSet::new_empty(locations.len_idx()),
            pdg,
            locations,
        }
    }
    pub fn assume_local(&mut self, local: Local) {
        self.assumed_locations
            .union_with(&self.pdg.data_dependencies.assignments[local]);
    }

    pub fn assuming_local(mut self, local: Local) -> Self {
        self.assume_local(local);
        self
    }

    pub fn assuming_locals(mut self, locals: impl IntoIterator<Item = Local>) -> Self {
        self.assume_locals(locals);
        self
    }

    pub fn assume_locals(&mut self, locals: impl IntoIterator<Item = Local>) {
        for local in locals {
            self.assume_local(local)
        }
    }

    pub fn requiring_local_in(
        mut self,
        local: Local,
        reaching_definitions: &BitSet<IntLocation>,
    ) -> Self {
        self.require_local_in(local, reaching_definitions);
        self
    }

    pub fn require_local_in(&mut self, local: Local, reaching_definitions: &BitSet<IntLocation>) {
        if let Some(assignments) = &self.pdg.data_dependencies.assignments[local] {
            self.relevant_locations.extend(
                assignments
                    .ones()
                    .filter(|x| reaching_definitions.contains(*x)),
            )
        }
    }

    pub fn requiring_locals_in(
        mut self,
        locals: impl IntoIterator<Item = Local>,
        reaching_definitions: &BitSet<IntLocation>,
    ) -> Self {
        self.require_locals_in(locals, reaching_definitions);
        self
    }

    pub fn require_locals_in(
        &mut self,
        locals: impl IntoIterator<Item = Local>,
        reaching_definitions: &BitSet<IntLocation>,
    ) {
        for local in locals {
            self.require_local_in(local, reaching_definitions)
        }
    }

    pub fn requiring_local_everywhere(mut self, local: Local) -> Self {
        self.require_local_everywhere(local);
        self
    }

    pub fn require_local_everywhere(&mut self, local: Local) {
        self.relevant_locations
            .union_with(&self.pdg.data_dependencies.assignments[local])
    }

    pub fn requiring_locals_everywhere(mut self, locals: impl IntoIterator<Item = Local>) -> Self {
        self.require_locals_everywhere(locals);
        self
    }

    pub fn require_locals_everywhere(&mut self, locals: impl IntoIterator<Item = Local>) {
        for local in locals {
            self.require_local_everywhere(local)
        }
    }
}

impl<'a, C: CallType> CfgPass<'_, C> for BackwardSlice<'a> {
    type Result = BitSet<IntLocation>;

    fn run(self, _cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let Self {
            mut relevant_locations,
            mut assumed_locations,
            pdg,
            locations,
        } = self;

        assumed_locations.union_with(&relevant_locations);

        // The relevant stmts are added to the work queue
        let mut work_queue = WorkQueue {
            // Add all relevant stmts to the work queue
            deque: VecDeque::from_iter(relevant_locations.ones()),
            // The assumed locations are marked as visited so they wont be inserted into the work queue
            set: assumed_locations,
        };

        while let Some(loc) = work_queue.take() {
            relevant_locations.insert(loc);
            let span = trace_span!("iteration", location = debug(locations[loc]));
            let _enter = span.enter();

            if let Some(data_dependencies) = &pdg.data_dependencies.use_def_chains[loc] {
                for data_dependency in data_dependencies.ones() {
                    trace!(
                        location = debug(&locations[data_dependency]),
                        "Data dependency",
                    );
                    work_queue.insert(data_dependency);
                }
            }

            if let Some(control_dependencies) = &pdg.control_dependencies[self.locations[loc].block]
            {
                for control_dependency in control_dependencies.ones() {
                    work_queue.insert(self.locations[control_dependency].terminator);
                }
            }
        }

        debug!(
            relevant_locations = debug(Vec::from_iter(
                relevant_locations.ones().map(|loc| locations[loc])
            )),
            "Backward slice finished"
        );

        relevant_locations
    }

    impl_pass_span!(self; "backward_slice",
        relevant = debug(Vec::from_iter(self.relevant_locations.ones().map(|loc|self.locations[loc]))),
        assumed = debug(Vec::from_iter(self.assumed_locations.ones().map(|loc|self.locations[loc]))),
    );
}
