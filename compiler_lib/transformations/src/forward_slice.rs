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
use openvaf_data_structures::{BitSet, WorkQueue};
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::{impl_pass_span, CallType};
use std::collections::VecDeque;
use std::iter::FromIterator;
use tracing::{debug, trace, trace_span};

pub struct ForwardSlice<'a> {
    pub tainted_locations: BitSet<IntLocation>,
    pub pdg: &'a InvProgramDependenceGraph,
    pub locations: &'a InternedLocations,
    pub taint_control_dependencies: bool,
}

impl<'a> ForwardSlice<'a> {
    pub fn new(pdg: &'a InvProgramDependenceGraph, locations: &'a InternedLocations) -> Self {
        Self {
            tainted_locations: BitSet::new_empty(locations.len_idx()),
            pdg,
            locations,
            taint_control_dependencies: true,
        }
    }
}

impl<'a, C: CallType> CfgPass<'_, C> for ForwardSlice<'a> {
    type Result = BitSet<IntLocation>;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let Self {
            tainted_locations,
            pdg,
            locations,
            taint_control_dependencies,
        } = self;

        if taint_control_dependencies {
            Self::run_impl::<C, true>(tainted_locations, pdg, locations, cfg)
        } else {
            Self::run_impl::<C, false>(tainted_locations, pdg, locations, cfg)
        }
    }

    impl_pass_span!(self; "forward_slice",
        tainted = debug(Vec::from_iter(self.tainted_locations.ones().map(|loc|self.locations[loc]))),
    );
}

impl<'a> ForwardSlice<'a> {
    fn run_impl<C: CallType, const TAINT_CONTROL_DEPENDENCIES: bool>(
        tainted_locations: BitSet<IntLocation>,
        pdg: &InvProgramDependenceGraph,
        locations: &InternedLocations,
        cfg: &mut ControlFlowGraph<C>,
    ) -> BitSet<IntLocation> {
        // Init the work que with the tainted locations
        let mut work_queue = WorkQueue {
            deque: VecDeque::from_iter(tainted_locations.ones()),
            set: tainted_locations,
        };

        while let Some(loc) = work_queue.take() {
            let span = trace_span!("iteration", location = debug(locations[loc]));
            let _enter = span.enter();

            if let Some(data_dependencents) = &pdg.data_dependencies.def_use_chains[loc] {
                for data_dependent_loc in data_dependencents.ones() {
                    trace!(
                        location = debug(&locations[data_dependent_loc]),
                        "Data dependent",
                    );
                    work_queue.insert(data_dependent_loc);
                }
            }

            if TAINT_CONTROL_DEPENDENCIES {
                if let Some(control_dependencents) = &pdg.control_dependencies[locations[loc].block]
                {
                    for control_dependent_bb in control_dependencents.ones() {
                        let mut loc = locations[control_dependent_bb].stmnt_start;
                        let tainted_blck_statements = cfg.blocks[control_dependent_bb]
                            .statements
                            .indices()
                            .map(|_| {
                                let res = loc;
                                loc += 1;
                                res
                            });
                        work_queue.extend(tainted_blck_statements)
                    }
                }
            }
        }
        let mut removed_locations = work_queue.set;

        debug!(
            removed_locations = debug(Vec::from_iter(
                removed_locations.ones().map(|loc| locations[loc])
            )),
            "Forward slice finished"
        );

        removed_locations.toggle_all();
        removed_locations
    }
}
