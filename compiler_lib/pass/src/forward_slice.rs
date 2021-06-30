/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::program_dependence::InvProgramDependenceGraph;
use itertools::Itertools;
use openvaf_data_structures::bit_set::{BitSet, SparseBitMatrix};
use openvaf_data_structures::WorkQueue;
use openvaf_middle::cfg::{
    AnalysisPass, ControlFlowGraph, IntLocation, InternedLocations, LocationKind,
};
use openvaf_middle::{impl_pass_span, CallType, Local};
use std::borrow::Borrow;
use tracing::{debug, trace, trace_span};

pub struct ForwardSlice<'a, A>
where
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    pub tainted_locations: BitSet<IntLocation>,
    pub pdg: &'a InvProgramDependenceGraph<A>,
    pub locations: &'a InternedLocations,
    pub taint_control_dependencies: bool,
}

impl<'a, A> ForwardSlice<'a, A>
where
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    pub fn new(pdg: &'a InvProgramDependenceGraph<A>, locations: &'a InternedLocations) -> Self {
        Self {
            tainted_locations: BitSet::new_empty(locations.len()),
            pdg,
            locations,
            taint_control_dependencies: true,
        }
    }
}

impl<'a, C, A> AnalysisPass<'_, C> for ForwardSlice<'a, A>
where
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
    C: CallType,
{
    type Result = BitSet<IntLocation>;

    fn run(self, _cfg: &ControlFlowGraph<C>) -> Self::Result {
        let Self {
            tainted_locations,
            pdg,
            locations,
            taint_control_dependencies,
        } = self;

        if taint_control_dependencies {
            Self::run_impl::<C, true>(tainted_locations, pdg, locations)
        } else {
            Self::run_impl::<C, false>(tainted_locations, pdg, locations)
        }
    }

    impl_pass_span!("Forward Slice");
}

impl<'a, A> ForwardSlice<'a, A>
where
    A: Borrow<SparseBitMatrix<Local, IntLocation>>,
{
    fn run_impl<C: CallType, const TAINT_CONTROL_DEPENDENCIES: bool>(
        tainted_locations: BitSet<IntLocation>,
        pdg: &InvProgramDependenceGraph<A>,
        locations: &InternedLocations,
    ) -> BitSet<IntLocation> {
        // Init the work que with the tainted locations
        let mut work_queue = WorkQueue {
            deque: tainted_locations.iter().collect(),
            set: tainted_locations,
        };

        while let Some(loc) = work_queue.take() {
            let span = trace_span!("iteration", location = debug(locations[loc]));
            let _enter = span.enter();

            if let Some(data_dependencents) = pdg.get_data_dependencies(loc) {
                for data_dependent_loc in data_dependencents.iter() {
                    trace!(
                        location = debug(&locations[data_dependent_loc]),
                        "Data dependent",
                    );
                    work_queue.insert(data_dependent_loc);
                }
            }

            if TAINT_CONTROL_DEPENDENCIES && matches!(locations[loc].kind, LocationKind::Terminator)
            {
                if let Some(control_dependencents) =
                    pdg.get_control_dependencies(locations[loc].block)
                {
                    for control_dependent_bb in control_dependencents.iter() {
                        let tainted_locations = (locations[control_dependent_bb].phi_start.raw()
                            ..=locations[control_dependent_bb].terminator.raw())
                            .map(IntLocation::from_raw_unchecked);

                        trace!(
                            locations = debug(
                                tainted_locations
                                    .clone()
                                    .map(|loc| locations[loc])
                                    .collect_vec()
                            ),
                            src = debug(locations[loc].block),
                            dst = debug(control_dependent_bb),
                            "Control dependent",
                        );
                        work_queue.extend(tainted_locations)
                    }
                }
            }
        }
        let mut removed_locations = work_queue.set;

        debug!(
            removed_locations = debug(
                removed_locations
                    .iter()
                    .map(|loc| locations[loc])
                    .collect_vec()
            ),
            "Forward slice finished"
        );

        removed_locations.inverse();
        removed_locations
    }
}
