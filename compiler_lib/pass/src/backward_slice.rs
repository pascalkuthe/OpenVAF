/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::program_dependence::ProgramDependenceGraph;
use openvaf_data_structures::bit_set::{BitSet, SparseBitMatrix};
use openvaf_data_structures::iter::Itertools;
use openvaf_data_structures::WorkQueue;
use openvaf_middle::cfg::{
    AnalysisPass, ControlFlowGraph, IntLocation, InternedLocations, LocationKind,
};
use openvaf_middle::{impl_pass_span, CfgFunctions, Local};
use std::borrow::Borrow;
use tracing::{debug, trace, trace_span};

pub struct BackwardSlice<'a, A>
where
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    pub relevant_locations: BitSet<IntLocation>,
    pub assumed_locations: BitSet<IntLocation>,
    pub pdg: &'a ProgramDependenceGraph<A>,
    pub locations: &'a InternedLocations,
}
impl<'a, A> BackwardSlice<'a, A>
where
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    pub fn new(pdg: &'a ProgramDependenceGraph<A>, locations: &'a InternedLocations) -> Self {
        Self {
            relevant_locations: BitSet::new_empty(locations.len()),
            assumed_locations: BitSet::new_empty(locations.len()),
            pdg,
            locations,
        }
    }
    pub fn assume_local(&mut self, local: Local) {
        if let Some(assignments) = self.pdg.get_assignments(local) {
            self.assumed_locations.union(assignments);
        }
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
        if let Some(assignments) = self.pdg.get_assignments(local) {
            self.relevant_locations.extend(
                assignments
                    .iter()
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
        if let Some(assignments) = self.pdg.get_assignments(local) {
            self.relevant_locations.union(assignments);
        }
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

impl<'a, C, A> AnalysisPass<'_, C> for BackwardSlice<'a, A>
where
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
    C: CfgFunctions,
{
    type Result = BitSet<IntLocation>;

    fn run(self, _cfg: &ControlFlowGraph<C>) -> Self::Result {
        let Self {
            mut relevant_locations,
            mut assumed_locations,
            pdg,
            locations,
        } = self;

        assumed_locations.union(&relevant_locations);

        // The relevant stmts are added to the work queue
        let mut work_queue = WorkQueue {
            // Add all relevant stmts to the work queue
            deque: relevant_locations.iter().collect(),
            // The assumed locations are marked as visited so they wont be inserted into the work queue
            set: assumed_locations,
        };

        while let Some(loc) = work_queue.take() {
            relevant_locations.insert(loc);
            let span = trace_span!("iteration", location = debug(locations[loc]));
            let _enter = span.enter();

            if let Some(data_dependencies) = &pdg.get_data_dependencies(loc) {
                for data_dependency in data_dependencies.iter() {
                    debug_assert_ne!(locations[data_dependency].kind, LocationKind::Terminator);
                    trace!(
                        location = debug(&locations[data_dependency]),
                        "Data dependency",
                    );
                    work_queue.insert(data_dependency);
                }
            }

            if let Some(control_dependencies) =
                &pdg.get_control_dependencies(self.locations[loc].block)
            {
                for control_dependency in control_dependencies.iter() {
                    work_queue.insert(self.locations[control_dependency].terminator);
                }
            }
        }

        debug!(
            relevant_locations = debug(
                relevant_locations
                    .iter()
                    .map(|loc| locations[loc])
                    .collect_vec()
            ),
            "Backward slice finished"
        );

        relevant_locations
    }

    impl_pass_span!(self; "backward_slice",
        relevant = debug(self.relevant_locations.iter().map(|loc|self.locations[loc]).collect_vec()),
        assumed = debug(self.assumed_locations.iter().map(|loc|self.locations[loc]).collect_vec()),
    );
}
