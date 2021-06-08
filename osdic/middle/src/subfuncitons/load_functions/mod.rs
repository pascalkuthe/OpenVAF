use crate::frontend::GeneralOsdiCall;
use crate::subfuncitons::automatic_slicing::{TaintedLocations, WrittenVars};
use crate::subfuncitons::load_functions::dc_load::{DcLoadFunctionCall, GeneralToDcLoad};
use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::{Mir, VariableId};
use openvaf_transformations::{ForwardSlice, InvProgramDependenceGraph, Strip};

mod ac_load;
mod dc_load;

pub struct LoadFunctions {
    pub dc_load: ControlFlowGraph<DcLoadFunctionCall>,
    pub ac_load: ControlFlowGraph<GeneralOsdiCall>,
    // tran_load: ControlFlowGraph<GeneralOsdiCall>
    pub written_vars: BitSet<VariableId>,
}

impl LoadFunctions {
    pub fn new(
        mir: &Mir<GeneralOsdiCall>,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
        tainted: &TaintedLocations,
        assumed_locations: &BitSet<IntLocation>,
        locations: &InternedLocations,
        all_output_locations: &BitSet<IntLocation>,
        pdg: &InvProgramDependenceGraph,
    ) -> Self {
        let mut load_locations = assumed_locations.clone();
        load_locations.toggle_all();

        let (dc_load, written_vars) = {
            let mut dc_load = cfg.clone();

            dc_load.run_pass(Strip {
                retain: &load_locations,
                locations,
            });

            let written_vars =
                WrittenVars::find_in_cfg(mir.variables.len_idx(), &mut dc_load, locations);

            (dc_load.map(&mut GeneralToDcLoad), written_vars)
        };

        let dc_load_output = {
            load_locations.intersect_with(all_output_locations);
            load_locations
        };

        let mut ac_load = cfg.clone();

        // Convert locals from real to cmplx
        // Done before stripping to avoid build a new PDG

        let instructions_to_convert = ac_load.run_pass(ForwardSlice {
            tainted_locations: tainted.by_time_derivative.clone(),
            pdg,
            locations,
            taint_control_dependencies: false,
        });

        // for loc in instructions_to_convert.ones() {
        //     let loc = locations.locations[loc];
        //     let bb = &ac_load.blocks[loc.block];
        //
        //     let local = match loc.kind {
        //         LocationKind::Phi(phi) => bb.phi_statements[phi].dst,
        //         LocationKind::Statement(stmnt) => {
        //             if let StmntKind::Assignment(dst, _) = bb.statements[stmnt].0 {
        //                 dst
        //             } else {
        //                 continue;
        //             }
        //         }
        //         LocationKind::Terminator => continue,
        //     };
        //
        //     debug_assert_eq!(ac_load.locals[local].ty, Type::REAL);
        //     ac_load.locals[local].ty = Type::CMPLX
        // }
        //
        // for local in &mut ac_load.locals {
        //     if matches!(local.kind, LocalKind::Branch(_, _)) {
        //         local.ty = Type::CMPLX
        //     }
        // }

        let tainted_by_time_derivative = ac_load.run_pass(ForwardSlice {
            tainted_locations: tainted.by_time_derivative.clone(),
            pdg,
            locations,
            taint_control_dependencies: true,
        });

        let ac_locations = {
            let mut ac_assumed = dc_load_output;
            ac_assumed.union_with(assumed_locations);
            ac_assumed.difference_with(&tainted_by_time_derivative);
            ac_assumed.toggle_all();
            ac_assumed
        };

        Self {
            dc_load,
            ac_load,
            written_vars,
        }
    }
}
