use crate::program_dependence::InvProgramDependenceGraph;
use openvaf_data_structures::{BitSet, SparseBitSetMatrix, WorkQueue};
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, InternedLocations, LocationId};
use openvaf_middle::{impl_pass_span, CallType, Local};
use std::collections::VecDeque;
use std::iter::FromIterator;
use tracing::{debug, trace, trace_span};

pub struct ForwardSlice<'a> {
    pub tainted_locations: BitSet<LocationId>,
    pub pdg: &'a InvProgramDependenceGraph,
    pub locations: &'a InternedLocations,
}

impl<'a> ForwardSlice<'a> {
    pub fn new(
        pdg: &'a InvProgramDependenceGraph,
        locations: &'a InternedLocations,
    ) -> Self {
        Self {
            tainted_locations: BitSet::new_empty(locations.len_idx()),
            pdg,
            locations,
        }
    }
}

impl<'a, C: CallType> CfgPass<'_, C> for ForwardSlice<'a> {
    type Result = BitSet<LocationId>;

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let Self {
            tainted_locations,
            pdg,
            locations,
            ..
        } = self;


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

            if let Some(control_dependencents) =
                &pdg.control_dependencies[self.locations[loc].block]
            {
                for control_dependent_bb in control_dependencents.ones() {
                    let mut loc = self.locations[control_dependent_bb].stmnt_start;
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

    impl_pass_span!(self; "forward_slice",
        tainted = debug(Vec::from_iter(self.tainted_locations.ones().map(|loc|self.locations[loc]))),
    );
}
