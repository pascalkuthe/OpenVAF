use openvaf_middle::{CallType, Derivative, CallArg, CallTypeConversion, COperandData, InputKind, Mir, COperand, Local, OperandData, RValue, StmntKind, TyRValue};
use crate::frontend::{GeneralOsdiInput, GeneralOsdiCall};
use openvaf_middle::const_fold::DiamondLattice;
use std::fmt::{Formatter, Display, Debug};
use std::fmt;
use openvaf_ir::convert::Convert;
use openvaf_ir::ids::{ParameterId, PortId};
use openvaf_hir::Unknown;
use openvaf_ir::{Type, Spanned};
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_session::sourcemap::Span;
use openvaf_middle::cfg::{ControlFlowGraph, BasicBlockData};
use openvaf_middle::cfg::builder::CfgBuilder;

#[derive(PartialEq, Eq, Clone)]
pub enum InitFunctionCallType {}

impl CallType for InitFunctionCallType {
    type I = GeneralOsdiInput;

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

impl Display for InitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
        }
    }
}

impl Debug for InitFunctionCallType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}



impl Convert<InitFunctionCallType> for GeneralOsdiCall{
    fn convert(self) -> InitFunctionCallType {
        match self{
            GeneralOsdiCall::Noise => unreachable!(),
            GeneralOsdiCall::TimeDerivative => unreachable!(),
            GeneralOsdiCall::StopTask(_, _) => unreachable!(),
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum InitInput {
    Parameter(ParameterId),
    ParamGiven(ParameterId),
    PortConnected(PortId),
}

impl Display for InitInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param) => Debug::fmt(param, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::ParamGiven(param) => write!(f, "$param_given({:?})", param),
        }
    }
}

impl InputKind for InitInput {
    fn derivative<C: CallType>(&self, _unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unreachable!() // No derivatives allows in the init function since that would require values that depend uponm voltages
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(param) => mir[*param].ty,
            Self::ParamGiven(_)
            | Self::PortConnected(_) => Type::BOOL,
        }
    }
}

pub struct GeneralToInit;

impl CallTypeConversion<GeneralOsdiCall, InitFunctionCallType> for GeneralToInit {
    fn map_input(&mut self, src: <GeneralOsdiCall as CallType>::I) -> COperandData<InitFunctionCallType> {
        let input = match src{
            GeneralOsdiInput::Parameter(param) => InitInput::Parameter(param),
            GeneralOsdiInput::ParamGiven(param) => InitInput::ParamGiven(param),
            GeneralOsdiInput::PortConnected(port) => InitInput::PortConnected(port),
            _ => unreachable!(),
        };

        OperandData::Read(input)
    }

    fn map_call_val(&mut self, call: GeneralOsdiCall, _args: IndexVec<CallArg, COperand<GeneralOsdiCall>>, span: Span) -> RValue<InitFunctionCallType> {
        unreachable!()
    }

    fn map_call_stmnt(&mut self, call: GeneralOsdiCall, args: IndexVec<CallArg, COperand<S>>, span: Span) -> StmntKind<InitFunctionCallType> {
        unreachable!()
    }
}

pub fn generate_param_init(mir: &Mir<GeneralOsdiCall>)->ControlFlowGraph<InitFunctionCallType>{
    let mut cfg = CfgBuilder::new_small();
    
    for (param, info) in mir.parameters.iter_enumerated(){
        let prev = cfg.current;
        
        cfg.enter_new_block();
        let read_val = RValue::Use(Spanned{contents: OperandData::Read(InitInput::Parameter(param)), span: mir.syntax_ctx[info.sctx].span});
        let read_val = cfg.assign_temporary(read_val, info.sctx);
        let true_blck = cfg.current;
        
        cfg.enter_new_block();
        cfg.cfg.insert_cfg()
        
        
        
    }
}
