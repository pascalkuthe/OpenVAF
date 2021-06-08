use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_ir::{PrintOnFinish, StopTaskKind};
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::{
    COperand, COperandData, CallArg, CallType, CallTypeConversion, ConstVal, Operand, OperandData,
    RValue, SimpleConstVal, StmntKind,
};
use openvaf_session::sourcemap::Span;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Clone)]
pub enum DcLoadFunctionCall {
    StopTask(StopTaskKind, PrintOnFinish),
}

impl CallType for DcLoadFunctionCall {
    type I = GeneralOsdiInput;

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

impl Display for DcLoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // LoadFunctionCall::TimeDerivative => write!(f, "ddt"),
            DcLoadFunctionCall::StopTask(StopTaskKind::Stop, finish) => {
                write!(f, "$stop({:?})", finish)
            }
            DcLoadFunctionCall::StopTask(StopTaskKind::Finish, finish) => {
                write!(f, "$finish({:?})", finish)
            }
        }
    }
}

impl Debug for DcLoadFunctionCall {
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
            GeneralOsdiCall::Noise
            | GeneralOsdiCall::SymbolicDerivativeOfTimeDerivative(_)
            | GeneralOsdiCall::TimeDerivative => RValue::Use(Operand {
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
            GeneralOsdiCall::TimeDerivative
            | GeneralOsdiCall::SymbolicDerivativeOfTimeDerivative(_) => unreachable!(),
            GeneralOsdiCall::StopTask(kind, print) => {
                StmntKind::Call(DcLoadFunctionCall::StopTask(kind, print), args, span)
            }
            GeneralOsdiCall::NodeCollapse(_, _) => StmntKind::NoOp,
        }
    }
}
