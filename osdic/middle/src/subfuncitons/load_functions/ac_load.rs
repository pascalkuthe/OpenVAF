use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use crate::subfuncitons::load_functions::dc_load::DcLoadFunctionCall;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_hir::{BranchId, NetId, Type, Unknown};
use openvaf_ir::convert::Convert;
use openvaf_ir::ids::{ParameterId, PortId};
use openvaf_ir::{PrintOnFinish, StopTaskKind};
use openvaf_middle::cfg::ControlFlowGraph;
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::{
    COperand, COperandData, CallArg, CallType, CallTypeConversion, ConstVal, Derivative, InputKind,
    Local, Mir, OperandData, RValue, SimpleConstVal, StmntKind, VariableId,
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
    type I = GeneralOsdiInput;

    fn const_fold(&self, call: &[DiamondLattice]) -> DiamondLattice {
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

pub struct GeneralToDcLoad;

impl CallTypeConversion<GeneralOsdiCall, DcLoadFunctionCall> for GeneralToDcLoad {
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CallType>::I,
    ) -> COperandData<DcLoadFunctionCall> {
        OperandData::Read(src)
    }

    fn map_call_val(
        &mut self,
        call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        span: Span,
    ) -> RValue<DcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise => RValue::Use(COperand {
                span,
                contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0))),
            }),
            GeneralOsdiCall::TimeDerivative => RValue::Use(COperand {
                span,
                contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0))),
            }),
            GeneralOsdiCall::StopTask(_, _) | GeneralOsdiCall::NodeCollapse(_, _) => unreachable!(),
        }
    }

    fn map_call_stmnt(
        &mut self,
        call: GeneralOsdiCall,
        args: IndexVec<CallArg, COperand<DcLoadFunctionCall>>,
        span: Span,
    ) -> StmntKind<DcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(kind, print) => {
                StmntKind::Call(DcLoadFunctionCall::StopTask(kind, print), args, span)
            }
            GeneralOsdiCall::NodeCollapse(_, _) => StmntKind::NoOp,
        }
    }
}
