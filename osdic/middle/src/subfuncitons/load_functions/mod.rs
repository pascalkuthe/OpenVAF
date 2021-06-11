use crate::frontend::GeneralOsdiCall;
use crate::subfuncitons::automatic_slicing::{TaintedLocations, WrittenVars};
use crate::subfuncitons::load_functions::dc_load::{DcLoadFunctionCall, GeneralToDcLoad};
use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations, START_BLOCK};
use openvaf_middle::{Mir, VariableId, LocalKind, StmntKind, RValue, OperandData};
use openvaf_transformations::{ForwardSlice, InvProgramDependenceGraph, Strip, BackwardSlice, ProgramDependenceGraph};
use openvaf_ir::{Type, Spanned};
use crate::subfuncitons::optimize_cfg;
use crate::CircuitTopology;
use crate::subfuncitons::load_functions::ac_load::{GeneralToAcLoad, AcLoadInput};
use openvaf_middle::Derivative::Operand;

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
        pdg: &ProgramDependenceGraph,
        inv_pdg: &InvProgramDependenceGraph,
        topology: &CircuitTopology,
    ) -> Self {
        let mut dc_load = cfg.clone();

        let mut dc_load_output = all_output_locations.clone();
        dc_load_output.union_with(&tainted.by_branch_write);


        let mut dc_load_locations = dc_load.run_pass(BackwardSlice{
            relevant_locations: dc_load_output,
            assumed_locations: assumed_locations.clone(),
            pdg,
            locations
        });

        dc_load.run_pass(Strip {
            retain: &dc_load_locations,
            locations,
        });

        let (dc_load, written_vars) = {

            let written_vars =
                WrittenVars::find_in_cfg(mir.variables.len_idx(), &mut dc_load, locations);

            let mut dc_load = dc_load.map(&mut GeneralToDcLoad);
            optimize_cfg(&mut dc_load);

            (dc_load, written_vars)
        };

        let dc_load_output = {
            dc_load_locations.intersect_with(all_output_locations);
            dc_load_locations
        };

        let mut ac_load = cfg.clone();

        let mut ac_assumed = dc_load_output;
        ac_assumed.union_with(assumed_locations);
        ac_assumed.difference_with(&tainted.by_time_derivative);

        let ac_load_locations = ac_load.run_pass(BackwardSlice{
            relevant_locations: tainted.by_stamp_write.clone(),
            assumed_locations: ac_assumed,
            pdg,
            locations
        });

        ac_load.run_pass(Strip{
            retain: &ac_load_locations,
            locations
        });

        ac_load.map(&mut GeneralToAcLoad);
        optimize_cfg(&mut ac_load);

        for (local,dcl) in{
            ac_load.blocks[START_BLOCK].statements.splice(0..0, ac_load.locals.iter_enumerated().filter_map(|(local,decl)| if let LocalKind::Variable(var,)).map(||(StmntKind::Assignment(local,RValue::Use(Spanned{contents: OperandData::Read(AcLoadInput::Variable()),span})))))
        }

        Self {
            dc_load,
            ac_load,
            written_vars,
        }
    }
}
