/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::frontend::GeneralOsdiCall;
use crate::storage_locations::{StorageLocation, StorageLocations};
use crate::subfuncitons::automatic_slicing::{ReadVars, TaintedLocations, WrittenStorage};
use crate::subfuncitons::load_functions::ac_load::AcLoadFunctionCall;
use crate::subfuncitons::load_functions::dc_load::{DcLoadFunctionCall, GeneralToDcLoad};
use crate::{optimize_cfg, CircuitTopology};
use openvaf_data_structures::{BitSet, HashMap};
use openvaf_hir::SyntaxCtx;
use openvaf_ir::Spanned;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations, START_BLOCK};
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_middle::osdi_types::SimpleConstVal::Real;
use openvaf_middle::{
    ConstVal, LocalKind, Mir, OperandData, RValue, StmntKind, VariableId, VariableLocalKind,
};
use openvaf_session::sourcemap::span::DUMMY_SP;
use openvaf_transformations::{
    BackwardSlice, InvProgramDependenceGraph, ProgramDependenceGraph, Strip,
};

mod ac_load;
mod dc_load;

pub struct LoadFunctions {
    pub dc_load: ControlFlowGraph<DcLoadFunctionCall>,
    pub ac_load: ControlFlowGraph<AcLoadFunctionCall>,
    // tran_load: ControlFlowGraph<GeneralOsdiCall>
    pub written_storage: BitSet<StorageLocation>,
    pub read_storage: BitSet<StorageLocation>,
}

impl LoadFunctions {
    pub fn new(
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
        tainted: &TaintedLocations,
        assumed_locations: &BitSet<IntLocation>,
        locations: &InternedLocations,
        all_output_locations: &BitSet<IntLocation>,
        pdg: &ProgramDependenceGraph,
        inv_pdg: &InvProgramDependenceGraph,
        topology: &CircuitTopology,
        storage: &StorageLocations,
    ) -> Self {
        let mut dc_load = cfg.clone();

        let mut dc_load_output = all_output_locations.clone();
        dc_load_output.difference_with(assumed_locations);
        dc_load_output.union_with(&tainted.by_branch_write);

        let mut dc_load_locations = dc_load.run_pass(BackwardSlice {
            relevant_locations: dc_load_output,
            assumed_locations: assumed_locations.clone(),
            pdg,
            locations,
        });

        dc_load.run_pass(Strip {
            retain: &dc_load_locations,
            locations,
        });

        let (dc_load, written_vars, read_vars_dc) = {
            let mut dc_load = dc_load.map(&mut GeneralToDcLoad);

            optimize_cfg(&mut dc_load);

            let read_vars = ReadVars::find_in_cfg(storage, &mut dc_load);

            let written_vars = WrittenStorage::find_in_cfg(storage, &mut dc_load, locations);

            (dc_load, written_vars, read_vars)
        };

        let dc_load_output = {
            dc_load_locations.intersect_with(all_output_locations);
            dc_load_locations
        };

        let mut ac_load = Self::gen_ac_load(
            &cfg,
            tainted,
            assumed_locations,
            locations,
            dc_load_output,
            pdg,
            inv_pdg,
            topology,
        );

        let mut read_vars = ReadVars::find_in_cfg(storage, &mut ac_load);
        read_vars.union_with(&read_vars_dc);

        Self {
            dc_load,
            ac_load,
            written_storage: written_vars,
            read_storage: read_vars,
        }
    }
}
