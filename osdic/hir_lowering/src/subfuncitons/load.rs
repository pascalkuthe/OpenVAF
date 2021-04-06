use openvaf_ir::{StopTaskKind, PrintOnFinish};
use openvaf_middle::{CallType, ConstVal, SimpleConstVal, Local, Mir, Derivative, CallArg, InputKind, CallTypeConvert, RValue, CallTypeConversion, COperand, StmntKind, COperandData, OperandData};
use openvaf_middle::const_fold::DiamondLattice;
use std::fmt::{Formatter, Display, Debug};
use std::fmt;
use openvaf_ir::ids::{ParameterId, PortId};
use openvaf_session::sourcemap::{StringLiteral, Span};
use openvaf_hir::{NetId, BranchId, Type, Unknown};
use osdic_target::sim::LimFunction;
use openvaf_diagnostics::ListFormatter;
use openvaf_ir::convert::Convert;
use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput};
use std::convert::identity;
use openvaf_data_structures::index_vec::IndexVec;

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



impl Convert<LoadFunctionCall> for GeneralOsdiCall{
    fn convert(self) -> LoadFunctionCall {
        match self{
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(kind, print) => LoadFunctionCall::StopTask(kind,print),
        }
    }
}

pub struct GeneralToLoad;

impl CallTypeConversion<GeneralOsdiCall, LoadFunctionCall> for GeneralToLoad{
    fn map_input(&mut self, src: <GeneralOsdiCall as CallType>::I) -> COperandData<LoadFunctionCall> {
        OperandData::Read(src)
    }

    fn map_call_val(&mut self, call: GeneralOsdiCall, _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>, span: Span) -> RValue<LoadFunctionCall> {
        match call{
            GeneralOsdiCall::Noise => RValue::Use(COperand{span, contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0)))}),
            GeneralOsdiCall::TimeDerivative => RValue::Use(COperand{span, contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0)))}), // TODO transient analysis
            GeneralOsdiCall::StopTask(_, _) => unreachable!()
        }
    }

    fn map_call_stmnt(&mut self, call: GeneralOsdiCall, args: IndexVec<CallArg, COperand<S>>, span: Span) -> StmntKind<LoadFunctionCall> {
        match call{
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(kind, print) => StmntKind::Call(LoadFunctionCall::StopTask(kind, print), args, span)
        }
    }
}