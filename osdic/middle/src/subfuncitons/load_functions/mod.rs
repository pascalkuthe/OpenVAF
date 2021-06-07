use crate::frontend::GeneralOsdiCall;
use crate::subfuncitons::automatic_slicing::{TaintedLocations, WrittenVars};
use crate::subfuncitons::load_functions::dc_load::{DcLoadFunctionCall, GeneralToDcLoad};
use openvaf_data_structures::BitSet;
use openvaf_ir::Type;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::{LocalKind, Mir, VariableId};
use openvaf_transformations::Strip;

mod dc_load;
// mod ac_load;

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
    ) -> Self {
        let mut cfg = cfg.clone();

        let mut load_locations = assumed_locations.clone();
        load_locations.toggle_all();

        cfg.run_pass(Strip {
            retain: &load_locations,
            locations,
        });

        let written_vars = WrittenVars::find_in_cfg(mir.variables.len_idx(), &mut cfg, locations);

        let dc_load = cfg.clone().map(&mut GeneralToDcLoad);

        let dc_load_output = {
            load_locations.intersect_with(all_output_locations);
            load_locations
        };

        let mut ac_assumed = dc_load_output;
        ac_assumed.difference_with(&tainted.by_time_derivative);
        ac_assumed.union_with(assumed_locations);

        let ac_locations = {
            ac_assumed.toggle_all();
            ac_assumed
        };

        cfg.run_pass(Strip {
            retain: &ac_locations,
            locations,
        });

        let mut ac_load = cfg;
        for local in &mut ac_load.locals {
            if matches!(local.kind, LocalKind::Branch(_, _)) {
                local.ty = Type::CMPLX
            }
        }

        Self {
            dc_load,
            ac_load,
            written_vars,
        }
    }
}
