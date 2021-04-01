use crate::program_dependence::InvProgramDependenceGraph;
use openvaf_middle::cfg::{CfgPass, InternedLocations, ControlFlowGraph, Location, LocationKind, LocationId};
use openvaf_middle::{CallType, impl_pass_span, Local};
use openvaf_data_structures::{BitSet, SparseBitSetMatrix, WorkQueue};
use std::collections::VecDeque;
use std::iter::FromIterator;
use tracing::{trace_span, trace, debug};

pub struct ForwardSlice<'a> {
    pub tainted_locations: BitSet<LocationId>,
    pub assumed_locations: BitSet<LocationId>,
    pub assignments: &'a SparseBitSetMatrix<Local, LocationId>,
    pub pdg: &'a InvProgramDependenceGraph,
    pub locations: &'a InternedLocations,
}

impl<'a> ForwardSlice<'a> {
    pub fn new(pdg: &'a InvProgramDependenceGraph, locations: &'a InternedLocations, assignments: &'a SparseBitSetMatrix<Local, LocationId>) -> Self {
        Self {
            tainted_locations: BitSet::new_empty(locations.len_idx()),
            assumed_locations: BitSet::new_empty(locations.len_idx()),
            assignments,
            pdg,
            locations,
        }
    }

    pub fn assume_local(&mut self, local: Local) {
        self.assumed_locations
            .union_with(&self.assignments[local]);
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
}

impl<'a, C: CallType> CfgPass<'_, C> for ForwardSlice<'a> {
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let Self {
            mut tainted_locations,
            assumed_locations,
            assignments,
            pdg,
            locations
        } = self;


        let mut removed_locations = assumed_locations;
        removed_locations.union_with(&tainted_locations);

        // The relevant stmts are added to the work queue
        let mut work_queue = WorkQueue {
            // Add all relevant stmts to the work queue
            deque: VecDeque::from_iter(tainted_locations.ones()),
            // The assumed locations are marked as visited so they wont be inserted into the work queue
            set: tainted_locations,
        };

        while let Some(loc) = work_queue.take() {
            removed_locations.insert(loc);
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

            if let Some(control_dependencents) = &pdg.control_dependencies[self.locations[loc].block]
            {
                for control_dependent_bb in control_dependencents.ones() {
                    let mut loc = self.locations[control_dependent_bb].stmnt_start;
                    let tainted_blck_statements = cfg.blocks[control_dependent_bb].statements.indices().map(|_|{let res = loc; loc += 1; loc});
                    work_queue.extend(tainted_blck_statements)
                }
            }
        }

        debug!(
            removed_locations = debug(Vec::from_iter(
                removed_locations.ones().map(|loc| locations[loc])
            )),
            "Forward slice finished"
        );

        for (bb, data) in cfg.blocks.iter_mut_enumerated() {
            let block_location = &self.locations[bb];

            let mut loc = block_location.phi_start;
            data.phi_statements.retain(|_| {
                let res = !removed_locations.contains(loc);
                loc += 1;
                res
            });

            let mut loc = block_location.stmnt_start;
            data.statements.retain(|_| {
                let res =  !removed_locations.contains(loc);
                loc += 1;
                res
            })
        }
    }

    impl_pass_span!(self; "forward_slice",
        tainted = debug(Vec::from_iter(self.tainted_locations.ones().map(|loc|self.locations[loc]))),
        assumed = debug(Vec::from_iter(self.assumed_locations.ones().map(|loc|self.locations[loc]))),
    );
}
