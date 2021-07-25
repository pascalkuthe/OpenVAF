/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::storage_locations::{StorageLocation, StorageLocations};
use crate::subfuncitons::automatic_slicing::function_cfg_from_full_cfg;
use data_structures::index_vec::{IndexSlice, IndexVec};
use data_structures::{bit_set::BitSet, HashMap};
use hir::{Unknown, VariableId};
use ir::Type;
use middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use middle::derivatives::RValueAutoDiff;
use middle::dfa::lattice::FlatSet;
use middle::{
    COperand, COperandData, CallArg, CfgConversion, CfgFunctions, CfgInputs, Derivative, Mir,
    OperandData, ParameterInput, RValue, StmntKind,
};
use pass::program_dependence::{InvProgramDependenceGraph, ProgramDependenceGraph};
use session::sourcemap::Span;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use tracing::debug_span;

pub type ModelTempUpdateCfg = ControlFlowGraph<ModelTempUpdateCallType>;

#[derive(PartialEq, Eq, Clone)]
pub enum ModelTempUpdateCallType {}

impl CfgFunctions for ModelTempUpdateCallType {
    type I = ModelTempUpdateInput;

    fn const_fold(&self, _call: &[FlatSet]) -> FlatSet {
        match *self {}
    }

    fn derivative<C: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        match *self {}
    }
}

impl Display for ModelTempUpdateCallType {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl Debug for ModelTempUpdateCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ModelTempUpdateInput {
    Parameter(ParameterInput),
    Temperature,
}

impl Display for ModelTempUpdateInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param_input) => Display::fmt(param_input, f),
            Self::Temperature => write!(f, "$temperature"),
        }
    }
}

impl CfgInputs for ModelTempUpdateInput {
    fn derivative<C: CfgFunctions>(&self, _unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unreachable!() // No derivatives allows in the init function since that would require values that depend uponm voltages
    }

    fn ty<C: CfgFunctions>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(ParameterInput::Value(param)) => mir[*param].ty,
            Self::Parameter(ParameterInput::Given(_)) => Type::BOOL,
            Self::Temperature => Type::REAL,
        }
    }
}

struct GeneralToModelTempUpdate;

impl CfgConversion<GeneralOsdiCall, ModelTempUpdateCallType> for GeneralToModelTempUpdate {
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CfgFunctions>::I,
    ) -> COperandData<ModelTempUpdateCallType> {
        let input = match src {
            GeneralOsdiInput::Parameter(x) => ModelTempUpdateInput::Parameter(x),
            GeneralOsdiInput::Temperature => ModelTempUpdateInput::Temperature,
            _ => unreachable!(),
        };

        OperandData::Read(input)
    }

    fn map_call_val(
        &mut self,
        _call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        _span: Span,
    ) -> RValue<ModelTempUpdateCallType> {
        unreachable!()
    }

    fn map_call_stmnt(
        &mut self,
        call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        _span: Span,
    ) -> StmntKind<ModelTempUpdateCallType> {
        if matches!(call, GeneralOsdiCall::NodeCollapse(_, _)) {
            StmntKind::NoOp
        } else {
            unreachable!()
        }
    }
}

pub struct ModelTempUpdateFunction {
    pub cfg: ControlFlowGraph<ModelTempUpdateCallType>,
    pub written_storage: BitSet<StorageLocation>,
    pub read_storage: BitSet<StorageLocation>,
}

impl ModelTempUpdateFunction {
    pub fn new(
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
        tainted_locations: &BitSet<IntLocation>,
        assumed_locations: &BitSet<IntLocation>,
        locations: &InternedLocations,
        pdg: &ProgramDependenceGraph,
        inv_pdg: &InvProgramDependenceGraph,
        all_output_stmnts: &BitSet<IntLocation>,
        storage: &StorageLocations,
    ) -> (Self, BitSet<IntLocation>) {
        let _span = debug_span!("Model Temp Update Function Creation");
        let _enter = _span.enter();

        let (cfg, function_output_locations, written_storage, read_storage) =
            function_cfg_from_full_cfg(
                cfg,
                tainted_locations,
                Some(assumed_locations),
                all_output_stmnts,
                locations,
                inv_pdg,
                pdg,
                storage,
            );

        let cfg = cfg.map(&mut GeneralToModelTempUpdate);

        (
            Self {
                cfg,
                written_storage: written_storage,
                read_storage: read_storage,
            },
            function_output_locations,
        )
    }
}
