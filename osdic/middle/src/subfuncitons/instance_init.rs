use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::automatic_slicing::function_cfg_from_full_cfg;
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_data_structures::BitSet;
use openvaf_hir::VariableId;
use openvaf_ir::ids::NetId;
use openvaf_ir::{PrintOnFinish, StopTaskKind};
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::{
    COperand, COperandData, CallArg, CallType, CallTypeConversion, Derivative, InputKind, Mir,
    OperandData, ParameterInput, PortId, RValue, StmntKind, Type, Unknown,
};
use openvaf_session::sourcemap::Span;
use openvaf_transformations::InvProgramDependenceGraph;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Clone)]
pub enum InstanceInitFunctionCallType {
    NodeCollapse(NetId, NetId),
    StopTask(StopTaskKind, PrintOnFinish),
}

impl CallType for InstanceInitFunctionCallType {
    type I = InstanceInitInput;

    fn const_fold(&self, _: &[DiamondLattice]) -> DiamondLattice {
        unreachable!()
    }

    fn derivative<C: CallType>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        unreachable!()
    }
}

impl Display for InstanceInitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Self::NodeCollapse(hi, lo) => write!(f, "$collapse({},{})", hi, lo),
            Self::StopTask(StopTaskKind::Stop, finish) => {
                write!(f, "$stop({:?})", finish)
            }
            Self::StopTask(StopTaskKind::Finish, finish) => {
                write!(f, "$finish({:?})", finish)
            }
        }
    }
}

impl Debug for InstanceInitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InstanceInitInput {
    Parameter(ParameterInput),
    PortConnected(PortId),
}

impl InputKind for InstanceInitInput {
    fn derivative<C: CallType>(&self, unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        if let Self::Parameter(input) = self {
            if matches!((unknown, input), (Unknown::Parameter(x), ParameterInput::Value(y)) if &x == y)
            {
                Derivative::One
            } else {
                Derivative::Zero
            }
        } else {
            Derivative::Zero
        }
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        if let Self::Parameter(input) = self {
            input.ty(mir)
        } else {
            Type::BOOL
        }
    }
}

impl Display for InstanceInitInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param_input) => Display::fmt(param_input, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
        }
    }
}

struct GeneralToInstanceInit;

impl CallTypeConversion<GeneralOsdiCall, InstanceInitFunctionCallType> for GeneralToInstanceInit {
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CallType>::I,
    ) -> COperandData<InstanceInitFunctionCallType> {
        match src {
            GeneralOsdiInput::Parameter(x) => OperandData::Read(InstanceInitInput::Parameter(x)),
            GeneralOsdiInput::PortConnected(port) => {
                OperandData::Read(InstanceInitInput::PortConnected(port))
            }
            _ => unreachable!(),
        }
    }

    fn map_call_val(
        &mut self,
        _call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        _span: Span,
    ) -> RValue<InstanceInitFunctionCallType> {
        unreachable!()
    }

    fn map_call_stmnt(
        &mut self,
        call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>, // args can be ignored because all valid calls dont have any
        span: Span,
    ) -> StmntKind<InstanceInitFunctionCallType> {
        match call {
            GeneralOsdiCall::StopTask(kind, print) => StmntKind::Call(
                InstanceInitFunctionCallType::StopTask(kind, print),
                IndexVec::new(),
                span,
            ),
            GeneralOsdiCall::NodeCollapse(hi, lo) => StmntKind::Call(
                InstanceInitFunctionCallType::NodeCollapse(hi, lo),
                IndexVec::new(),
                span,
            ),
            _ => unreachable!(),
        }
    }
}

pub struct InstanceInitFunction {
    pub cfg: ControlFlowGraph<InstanceInitFunctionCallType>,
    pub written_vars: BitSet<VariableId>,
}

impl InstanceInitFunction {
    pub fn new(
        mir: &Mir<GeneralOsdiCall>,
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
        tainted_locations: &BitSet<IntLocation>,
        assumed_locations: &BitSet<IntLocation>,
        locations: &InternedLocations,
        pdg: &InvProgramDependenceGraph,
        all_output_stmnts: &BitSet<IntLocation>,
    ) -> (Self, BitSet<IntLocation>) {
        let (cfg, function_output_locations, written_vars) = function_cfg_from_full_cfg(
            mir,
            cfg,
            tainted_locations,
            Some(assumed_locations),
            all_output_stmnts,
            locations,
            pdg,
        );

        let cfg = cfg.map(&mut GeneralToInstanceInit);

        (Self { cfg, written_vars }, function_output_locations)
    }
}
