use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput, SimParamKind};
use crate::subfuncitons::load_functions::dc_load::DcLoadFunctionCall;
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_hir::{BranchId, NetId, Type, Unknown};
use openvaf_ir::convert::Convert;
use openvaf_ir::ids::{ParameterId, PortId};
use openvaf_ir::{PrintOnFinish, Spanned, StopTaskKind};
use openvaf_middle::cfg::ControlFlowGraph;
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::{
    BinOp, COperand, COperandData, CallArg, CallType, CallTypeConversion, ConstVal, Derivative,
    InputKind, Local, Mir, Operand, OperandData, ParameterInput, RValue, SimpleConstVal, StmntKind,
    VariableId,
};
use openvaf_session::sourcemap::{Span, StringLiteral};
use osdic_target::sim::LimFunction;
use std::convert::identity;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Clone)]
pub enum AcLoadFunctionCall {
    StopTask(StopTaskKind, PrintOnFinish),
}

impl CallType for AcLoadFunctionCall {
    type I = AcLoadInput;

    fn const_fold(&self, _call: &[DiamondLattice]) -> DiamondLattice {
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

impl Display for AcLoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // LoadFunctionCall::TimeDerivative => write!(f, "ddt"),
            AcLoadFunctionCall::StopTask(StopTaskKind::Stop, finish) => {
                write!(f, "$stop({:?})", finish)
            }
            AcLoadFunctionCall::StopTask(StopTaskKind::Finish, finish) => {
                write!(f, "$finish({:?})", finish)
            }
        }
    }
}

impl Debug for AcLoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OmegaKind {
    Real,
    Imaginary,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AcLoadInput {
    Parameter(ParameterInput),
    Variable(VariableId),
    PortConnected(PortId),
    SimParam(StringLiteral, SimParamKind),
    Voltage(NetId, NetId),
    Current(BranchId),
    PortFlow(PortId),
    Omega(OmegaKind), // Analysis frequency
    Temperature,
}

impl Display for AcLoadInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param) => Debug::fmt(param, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::SimParam(name, kind) => write!(f, "$simparam({}, {:?})", name, kind),
            Self::Current(branch) => write!(f, "flow({:?})", branch),
            Self::Voltage(hi, lo) => write!(f, "pot({:?}, {:?})", hi, lo),
            Self::PortFlow(port) => write!(f, "flow({:?})", port),
            Self::Temperature => f.write_str("$temp"),
            Self::Variable(var) => write!(f, "{:?}", var),
            Self::Omega(OmegaKind::Real) => write!(f, "$omega"),
            Self::Omega(OmegaKind::Imaginary) => write!(f, "1j * $omega"),
        }
    }
}

impl InputKind for AcLoadInput {
    fn derivative<C: CallType>(&self, unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unimplemented!()
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(ParameterInput::Value(param)) => mir[*param].ty,
            Self::Voltage(_, _)
            | Self::Current(_)
            | Self::PortFlow(_)
            | Self::Temperature
            | Self::SimParam(_, SimParamKind::RealOptional)
            | Self::SimParam(_, SimParamKind::Real)
            | Self::Omega(OmegaKind::Real) => Type::REAL,

            Self::Parameter(ParameterInput::Given(_))
            | Self::PortConnected(_)
            | Self::SimParam(_, SimParamKind::RealOptionalGiven) => Type::BOOL,
            Self::SimParam(_, SimParamKind::String) => Type::STRING,

            Self::Variable(var) => mir.variables[*var].ty,
            Self::Omega(OmegaKind::Imaginary) => Type::CMPLX,
        }
    }
}

pub struct GeneralToAcLoad;

impl CallTypeConversion<GeneralOsdiCall, AcLoadFunctionCall> for GeneralToAcLoad {
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CallType>::I,
    ) -> COperandData<AcLoadFunctionCall> {
        let res = match src {
            GeneralOsdiInput::Parameter(port) => AcLoadInput::Parameter(port),
            GeneralOsdiInput::PortConnected(conncted) => AcLoadInput::PortConnected(conncted),
            GeneralOsdiInput::SimParam(name, kind) => AcLoadInput::SimParam(name, kind),
            GeneralOsdiInput::Voltage(hi, lo) => AcLoadInput::Voltage(hi, lo),
            GeneralOsdiInput::Lim { hi, lo, .. } => AcLoadInput::Voltage(hi, lo),
            GeneralOsdiInput::Temperature => AcLoadInput::Temperature,
        };
        OperandData::Read(res)
    }

    fn map_call_val(
        &mut self,
        call: GeneralOsdiCall,
        mut args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        span: Span,
    ) -> RValue<AcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise => RValue::Use(Operand {
                span,
                contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0))),
            }),
            GeneralOsdiCall::TimeDerivative => {
                // BLOCK allow generating additional statements here
                todo!("Time derivatives of something other than charges")
                // debug_assert_eq!(args.len(),1);
                // let omega = Operand{ span, contents: OperandData::Read(AcLoadInput::Omega(OmegaKind::Imaginary))};
                // let admittance = omega * args[0]
                // RValue::BinaryOperation(Spanned{ span, contents: BinOp::Multiply }, admittance, voltage)
            }
            GeneralOsdiCall::SymbolicDerivativeOfTimeDerivative => {
                let omega = Operand {
                    span,
                    contents: OperandData::Read(AcLoadInput::Omega(OmegaKind::Imaginary)),
                };

                debug_assert_eq!(args.len(), 1);
                RValue::Use(omega)
            }

            GeneralOsdiCall::StopTask(_, _) | GeneralOsdiCall::NodeCollapse(_, _) => unreachable!(),
        }
    }

    fn map_call_stmnt(
        &mut self,
        call: GeneralOsdiCall,
        args: IndexVec<CallArg, COperand<DcLoadFunctionCall>>,
        span: Span,
    ) -> StmntKind<AcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::StopTask(kind, print) => StmntKind::Call(
                AcLoadFunctionCall::StopTask(kind, print),
                IndexVec::new(),
                span,
            ),
            GeneralOsdiCall::NodeCollapse(_, _) => StmntKind::NoOp,
            _ => unreachable!(),
        }
    }
}
