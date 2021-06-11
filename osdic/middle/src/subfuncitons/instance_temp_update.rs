use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::automatic_slicing::function_cfg_from_full_cfg;
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_data_structures::BitSet;
use openvaf_hir::Unknown;
use openvaf_ir::ids::{PortId, VariableId};
use openvaf_ir::Type;
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::{
    COperand, COperandData, CallArg, CallType, CallTypeConversion, Derivative, InputKind, Mir,
    OperandData, ParameterInput, RValue, StmntKind,
};
use openvaf_session::sourcemap::Span;
use openvaf_transformations::{InvProgramDependenceGraph, ProgramDependenceGraph};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Clone)]
pub enum InstanceTempUpdateCallType {}

impl CallType for InstanceTempUpdateCallType {
    type I = InstanceTempUpdateInput;

    fn const_fold(&self, _call: &[DiamondLattice]) -> DiamondLattice {
        match *self {}
    }
    fn derivative<C: CallType>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        match *self {}
    }
}

impl Display for InstanceTempUpdateCallType {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl Debug for InstanceTempUpdateCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstanceTempUpdateInput {
    Parameter(ParameterInput),
    PortConnected(PortId),
    Temperature,
}

impl Display for InstanceTempUpdateInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param_input) => Display::fmt(param_input, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::Temperature => write!(f, "$temperature"),
        }
    }
}

impl InputKind for InstanceTempUpdateInput {
    fn derivative<C: CallType>(&self, _unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unreachable!() // No derivatives allows in the init function since that would require values that depend uponm voltages
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(ParameterInput::Value(param)) => mir[*param].ty,
            Self::Parameter(ParameterInput::Given(_)) | Self::PortConnected(_) => Type::BOOL,
            Self::Temperature => Type::REAL,
        }
    }
}

pub struct GeneralToInstanceTempUpdate;

impl CallTypeConversion<GeneralOsdiCall, InstanceTempUpdateCallType>
    for GeneralToInstanceTempUpdate
{
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CallType>::I,
    ) -> COperandData<InstanceTempUpdateCallType> {
        let input = match src {
            GeneralOsdiInput::Parameter(x) => InstanceTempUpdateInput::Parameter(x),
            GeneralOsdiInput::PortConnected(port) => InstanceTempUpdateInput::PortConnected(port),
            GeneralOsdiInput::Temperature => InstanceTempUpdateInput::Temperature,
            _ => unreachable!(),
        };

        OperandData::Read(input)
    }

    fn map_call_val(
        &mut self,
        _call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        _span: Span,
    ) -> RValue<InstanceTempUpdateCallType> {
        unreachable!()
    }

    fn map_call_stmnt(
        &mut self,
        _call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        _span: Span,
    ) -> StmntKind<InstanceTempUpdateCallType> {
        unreachable!()
    }
}

pub struct InstanceTempUpdateFunction {
    pub cfg: ControlFlowGraph<InstanceTempUpdateCallType>,
    pub written_vars: BitSet<VariableId>,
}

impl InstanceTempUpdateFunction {
    pub fn new(
        mir: &Mir<GeneralOsdiCall>,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
        tainted_locations: &BitSet<IntLocation>,
        assumed_locations: &BitSet<IntLocation>,
        locations: &InternedLocations,
        pdg: &ProgramDependenceGraph,
        inv_pdg: &InvProgramDependenceGraph,
        all_output_stmnts: &BitSet<IntLocation>,
    ) -> (Self, BitSet<IntLocation>) {
        let (cfg, function_output_locations, written_vars) = function_cfg_from_full_cfg(
            mir,
            cfg,
            tainted_locations,
            Some(assumed_locations),
            all_output_stmnts,
            locations,
            inv_pdg,
            pdg,
        );

        let cfg = cfg.map(&mut GeneralToInstanceTempUpdate);

        (Self { cfg, written_vars }, function_output_locations)
    }
}
