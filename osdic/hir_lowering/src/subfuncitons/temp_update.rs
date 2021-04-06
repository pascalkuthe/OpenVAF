use openvaf_middle::{CallType, Derivative, CallArg, CallTypeConversion, COperandData, InputKind, Mir, COperand, Local, RValue, StmntKind, OperandData};
use crate::frontend::{GeneralOsdiInput, GeneralOsdiCall};
use openvaf_middle::const_fold::DiamondLattice;
use std::fmt::{Formatter, Display, Debug};
use std::fmt;
use openvaf_ir::convert::Convert;
use openvaf_ir::ids::{ParameterId, PortId};
use openvaf_hir::Unknown;
use openvaf_ir::Type;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_session::sourcemap::Span;
use crate::subfuncitons::init::{InitFunctionCallType, InitInput};

#[derive(PartialEq, Eq, Clone)]
pub enum TempUpdateCallType {
}

impl CallType for TempUpdateCallType {
    type I = TempUpdateInput;

    fn const_fold(&self, call: &[DiamondLattice]) -> DiamondLattice {
        match self {}
    }

    fn derivative<C: CallType>(
        &self,
        _original: Local,
        _mir: &Mir<C>,
        _arg_derivative: impl FnMut(CallArg) -> Derivative<Self::I>,
    ) -> Derivative<Self::I> {
        match self{}
    }
}

impl Display for TempUpdateCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {}
    }
}

impl Debug for TempUpdateCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}



impl Convert<TempUpdateCallType> for GeneralOsdiCall{
    fn convert(self) -> TempUpdateCallType {
        match self{
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(_, _) => unreachable!(),
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum TempUpdateInput {
    Parameter(ParameterId),
    ParamGiven(ParameterId),
    PortConnected(PortId),
    Temperature
}

impl Display for TempUpdateInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param) => Debug::fmt(param, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::ParamGiven(param) => write!(f, "$param_given({:?})", param),
            Self::Temperature => write!(f, "$temperature"),
        }
    }
}

impl InputKind for TempUpdateInput {
    fn derivative<C: CallType>(&self, _unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unreachable!() // No derivatives allows in the init function since that would require values that depend uponm voltages
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(param) => mir[*param].ty,
            Self::ParamGiven(_)
            | Self::PortConnected(_) => Type::BOOL,
            Self::Temperature => Type::REAL
        }
    }
}

pub struct GeneralToTempUpdate;

impl CallTypeConversion<GeneralOsdiCall, TempUpdateCallType> for GeneralToTempUpdate {
    fn map_input(&mut self, src: <GeneralOsdiCall as CallType>::I) -> COperandData<TempUpdateCallType> {
        let input = match src{
            GeneralOsdiInput::Parameter(param) => TempUpdateInput::Parameter(param),
            GeneralOsdiInput::ParamGiven(param) => TempUpdateInput::ParamGiven(param),
            GeneralOsdiInput::PortConnected(port) => TempUpdateInput::PortConnected(port),
            GeneralOsdiInput::Temperature => TempUpdateInput::Temperature,
            _ => unreachable!(),
        };

        OperandData::Read(input)
    }

    fn map_call_val(&mut self, _call: GeneralOsdiCall, _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>, _span: Span) -> RValue<TempUpdateCallType> {
        unreachable!()
    }

    fn map_call_stmnt(&mut self, _call: GeneralOsdiCall, _args: IndexVec<CallArg, COperand<S>>, _span: Span) -> StmntKind<TempUpdateCallType> {
        unreachable!()
    }
}
