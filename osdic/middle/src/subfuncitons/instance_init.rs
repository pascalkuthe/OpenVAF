use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::{create_subfunction_cfg, FindWrittenVars};
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_data_structures::BitSet;
use openvaf_hir::VariableId;
use openvaf_ir::ids::NetId;
use openvaf_ir::{PrintOnFinish, StopTaskKind};
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::{
    COperand, COperandData, CallArg, CallType, CallTypeConversion, Derivative, InputKind, Local,
    Mir, OperandData, ParameterInput, PortId, RValue, StmntKind, Type, Unknown,
};
use openvaf_session::sourcemap::Span;
use openvaf_transformations::{ForwardSlice, InvProgramDependenceGraph, Strip, Visit};
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
        _original: Local,
        _mir: &Mir<C>,
        _arg_derivative: impl FnMut(CallArg) -> Derivative<Self::I>,
    ) -> Derivative<Self::I> {
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

pub struct InstanceInit {
    pub cfg: ControlFlowGraph<InstanceInitFunctionCallType>,
    /// Variables that are unique to each instance
    pub instance_vars: BitSet<VariableId>,
}

impl InstanceInit {
    pub fn new(
        mir: &Mir<GeneralOsdiCall>,
        src_cfg: &ControlFlowGraph<GeneralOsdiCall>,
        tainted_locations: BitSet<IntLocation>,
        assumed_locations: &BitSet<IntLocation>,

        locations: &InternedLocations,
        pdg: &InvProgramDependenceGraph,
        output_stmnts: &BitSet<IntLocation>,
    ) -> (Self, BitSet<IntLocation>) {
        let mut instance_vars = BitSet::new_empty(mir.variables.len_idx());
        let (cfg, output_locations) = create_subfunction_cfg(
            src_cfg,
            tainted_locations,
            assumed_locations,
            locations,
            pdg,
            output_stmnts,
            &mut instance_vars,
        );
        let cfg: ControlFlowGraph<InstanceInitFunctionCallType> = cfg.map(&mut InstanceInitMapper);

        let res = Self { cfg, instance_vars };
        (res, output_locations)
    }
}

struct InstanceInitMapper;

impl CallTypeConversion<GeneralOsdiCall, InstanceInitFunctionCallType> for InstanceInitMapper {
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
