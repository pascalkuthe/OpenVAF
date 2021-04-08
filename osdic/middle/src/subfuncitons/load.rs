use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_diagnostics::ListFormatter;
use openvaf_hir::{BranchId, NetId, Type, Unknown};
use openvaf_ir::convert::Convert;
use openvaf_ir::ids::{ParameterId, PortId};
use openvaf_ir::{PrintOnFinish, StopTaskKind};
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::{
    COperand, COperandData, CallArg, CallType, CallTypeConversion,  ConstVal,
    Derivative, InputKind, Local, Mir, OperandData, RValue, SimpleConstVal, StmntKind,
};
use openvaf_session::sourcemap::{Span, StringLiteral};
use osdic_target::sim::LimFunction;
use std::convert::identity;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Clone)]
pub enum LoadFunctionCall {
    // TimeDerivative,
    StopTask(StopTaskKind, PrintOnFinish),
}

impl CallType for LoadFunctionCall {
    type I = GeneralOsdiInput;

    fn const_fold(&self, call: &[DiamondLattice]) -> DiamondLattice {
        match self {
            // derivative of constants are always zero no matter the analysis mode (nonsensical code maybe lint this?)
            // Self::TimeDerivative => call[0]
            //     .clone()
            //     .and_then(|_| DiamondLattice::Val(ConstVal::Scalar(SimpleConstVal::Real(0.0)))),
            // Self::TimeDerivative => DiamondLattice::Val(ConstVal::Scalar(SimpleConstVal::Real(0.0))), // todo transient
            LoadFunctionCall::StopTask(_, _) => unreachable!(),
        }
    }

    fn derivative<C: CallType>(
        &self,
        _original: Local,
        _mir: &Mir<C>,
        _arg_derivative: impl FnMut(CallArg) -> Derivative<Self::I>,
    ) -> Derivative<Self::I> {
        // TODO transient
        unreachable!()
    }
}

impl Display for LoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // LoadFunctionCall::TimeDerivative => write!(f, "ddt"),
            LoadFunctionCall::StopTask(StopTaskKind::Stop, finish) => {
                write!(f, "$stop({:?})", finish)
            }
            LoadFunctionCall::StopTask(StopTaskKind::Finish, finish) => {
                write!(f, "$finish({:?})", finish)
            }
        }
    }
}

impl Debug for LoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Convert<LoadFunctionCall> for GeneralOsdiCall {
    fn convert(self) -> LoadFunctionCall {
        match self {
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(kind, print) => LoadFunctionCall::StopTask(kind, print),
        }
    }
}

pub struct GeneralToLoad;

impl CallTypeConversion<GeneralOsdiCall, LoadFunctionCall> for GeneralToLoad {
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CallType>::I,
    ) -> COperandData<LoadFunctionCall> {
        OperandData::Read(src)
    }

    fn map_call_val(
        &mut self,
        call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        span: Span,
    ) -> RValue<LoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise => RValue::Use(COperand {
                span,
                contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0))),
            }),
            GeneralOsdiCall::TimeDerivative => RValue::Use(COperand {
                span,
                contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0))),
            }), // TODO transient analysis
            GeneralOsdiCall::StopTask(_, _) => unreachable!(),
        }
    }

    fn map_call_stmnt(
        &mut self,
        call: GeneralOsdiCall,
        args: IndexVec<CallArg, COperand<S>>,
        span: Span,
    ) -> StmntKind<LoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(kind, print) => {
                StmntKind::Call(LoadFunctionCall::StopTask(kind, print), args, span)
            }
        }
    }
}
