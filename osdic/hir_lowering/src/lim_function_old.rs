use openvaf_hir_lowering::{ExpressionLowering, HirLowering, HirSystemFunctionCall, LocalCtx};
use openvaf_ir::{NoiseSource, PrintOnFinish, Spanned, StopTaskKind, Type, Unknown};
use openvaf_middle::cfg::ControlFlowGraph;
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::{
    CallType, Derivative, DisciplineAccess, InputKind, Mir, OperandData, Parameter, RValue,
    StmntKind,
};
use openvaf_session::sourcemap::Span;
use std::fmt;
use std::fmt::{Debug, Display, Formatter, Write};
use openvaf_data_structures::index_vec::IndexVec;

pub struct LimFunctionRegistry(IndexVec<FunctionId, Option<ControlFlowGraph<VerilogLimFunctionCall>>);

impl LimFunctionRegistry{
    pub fn insert_function(fun: FunctionId){

    }
}

pub struct VerilogLimFunction{
    header:
}



#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VerilogLimFunctionCall {}

impl CallType for VerilogLimFunctionCall {
    type I = VerilogLimFunctionInput;

    fn const_fold(&self, call: &[DiamondLattice]) -> DiamondLattice {
        match self {}
    }

    fn derivative<C: CallType>(
        &self,
        _original: Local,
        _mir: &Mir<C>,
        _arg_derivative: impl FnMut(Local) -> Derivative<Self::I>,
    ) -> Derivative<Self::I> {
        match self {}
    }
}

impl Display for VerilogLimFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {}
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SimParamKind {
    Real,
    RealOptional,
    RealOptionalGiven,
    String,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VerilogLimFunctionInput {
    Parameter(ParameterId),
    ParamGiven(ParameterId),
    PortConnected(PortId),
    SimParam(StringLiteral, SimParamKind),
    Temperature,
}

impl Display for VerilogLimFunctionInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param) => Debug::fmt(param, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::ParamGiven(param) => write!(f, "$param_given({:?})", param),
            Self::SimParam(name, kind) => write!(f, "$simparam({}, {:?})", name, kind),
            Self::Temperature => f.write_str("$temp"),
        }
    }
}

impl InputKind for VerilogLimFunctionInput {
    fn derivative<C: CallType>(&self, _unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unimplemented!(
            "Derivatives in lim functions make no sense (treat the same as a branch access)"
        )
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(param) => mir.parameters[param].ty,
            Self::ParamGiven(_)
            | Self::SimParam(_, SimParamKind::RealOptionalGiven)
            | Self::PortConnected(_) => Type::BOOL,
            Self::SimParam(_, SimParamKind::Real)
            | Self::SimParam(_, SimParamKind::RealOptional)
            | Self::Temperature => Type::REAL,
            Self::SimParam(_, SimParamKind::String) => Type::STRING,
        }
    }
}

impl ExpressionLowering for VerilogLimFunctionCall {
    fn port_flow<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        port: PortId,
        span: Span,
    ) -> Option<RValue<Self>> {
        unreachable!()
    }

    fn branch_access<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        access: DisciplineAccess,
        branch: _,
        span: Span,
    ) -> Option<RValue<Self>> {
        unreachable!()
    }

    fn parameter_ref<L: HirLowering>(
        _ctx: &mut LocalCtx<Self, L>,
        param: ParameterId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Use(Spanned {
            span,
            contents: OperandData::Read(VerilogLimFunctionInput::Parameter(parm)),
        }))
    }

    fn time_derivative<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        expr: _,
        span: Span,
    ) -> Option<RValue<Self>> {
        unreachable!()
    }

    fn noise<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        source: NoiseSource<_, ()>,
        name: Option<_>,
        span: Span,
    ) -> Option<RValue<Self>> {
        unreachable!()
    }

    fn system_function_call<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        call: &HirSystemFunctionCall,
        span: Span,
    ) -> Option<RValue<Self>> {
        let input = match call {
            HirSystemFunctionCall::ParameterGiven(param) => {
                VerilogLimFunctionInput::ParamGiven(param)
            }
            HirSystemFunctionCall::ParameterGiven(param) => {
                VerilogLimFunctionInput::ParamGiven(param)
            }
            HirSystemFunctionCall::Temperature => VerilogLimFunctionInput::Temperature,
            HirSystemFunctionCall::Vt(_temp) => todo!(),
            HirSystemFunctionCall::Simparam(_, _) => todo!(),
            HirSystemFunctionCall::SimparamStr(_) => todo!(),
            HirSystemFunctionCall::PortConnected(port) => {
                HirSystemFunctionCall::PortConnected(port)
            }
            _ => unreachable!(),
        };

        Some(RValue::Use(Spanned {
            span,
            contents: OperandData::Read(input),
        }))
    }

    fn stop_task<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        kind: StopTaskKind,
        finish: PrintOnFinish,
        span: Span,
    ) -> Option<StmntKind<Self>> {
        unreachable!()
    }
}
