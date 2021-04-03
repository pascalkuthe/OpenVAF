use crate::lim_function::LimFunctionCall;
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_diagnostics::ListFormatter;
use openvaf_hir::LimFunction;
use openvaf_hir_lowering::{
    AttributeCtx, ExpressionLowering, HirFold, HirLowering, HirSystemFunctionCall, LocalCtx,
};
use openvaf_ir::{Attribute, NoiseSource, PrintOnFinish, Spanned, StopTaskKind, Type, Unknown};
use openvaf_middle::cfg::{ControlFlowGraph, TerminatorKind};
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_middle::osdi_types::SimpleConstVal::Real;
use openvaf_middle::{
    CallType, ConstVal, Derivative, DisciplineAccess, Expression, InputKind, Mir, OperandData,
    RValue, RealConstCallType, SimpleConstVal, StmntKind,
};
use openvaf_session::sourcemap::string_literals::StringLiteral;
use openvaf_session::sourcemap::Span;
use std::fmt::{Debug, Display, Formatter, Write};
use std::{fmt, mem};
use term::Attr::Dim;
use osdic_target::sim::Simulator;

#[derive(PartialEq, Eq, Clone)]
pub enum GeneralOsdiCall {
    Noise,
    // TODO Noise
    TimeDerivative,
    StopTask(StopTaskKind, PrintOnFinish),
}

impl CallType for GeneralOsdiCall {
    type I = GeneralOsdiInput;

    fn const_fold(&self, call: &[DiamondLattice]) -> DiamondLattice {
        match self {
            Self::Noise => DiamondLattice::NotAConstant,
            // derivative of constants are always zero no matter the analysis mode (nonsensical code maybe lint this?)
            Self::TimeDerivative => call[0]
                .and_then(|_| DiamondLattice::Val(ConstVal::Scalar(SimpleConstVal::Real(0.0)))),
            GeneralOsdiCall::StopTask(_, _) => unreachable!()
        }
    }

    fn derivative<C: CallType>(
        &self,
        _original: Local,
        _mir: &Mir<C>,
        _arg_derivative: impl FnMut(Local) -> Derivative<Self::I>,
    ) -> Derivative<Self::I> {
        unreachable!("Derivative are analysis mode specific!")
    }
}

impl Display for GeneralOsdiCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GeneralOsdiCall::Noise => write!(f, "$noise"),
            GeneralOsdiCall::TimeDerivative => write!(f, "ddt"),
            GeneralOsdiCall::StopTask(StopTaskKind::Stop, finish) => {
                write!(f, "$stop({:?})", finish)
            }
            GeneralOsdiCall::StopTask(StopTaskKind::Finish, finish) => {
                write!(f, "$finish({:?})", finish)
            }
        }
    }
}

impl Debug for GeneralOsdiCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
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
pub enum GeneralOsdiInput {
    Parameter(ParameterId),
    ParamGiven(ParameterId),
    PortConnected(PortId),
    SimParam(StringLiteral, SimParamKind),
    Voltage(NetId, NetId),
    Current(BranchId),
    PortFlow(PortId),
    Lim {
        hi: NetId,
        lo: NetId,
        fun: LimFunctionCall,
        args: Vec<Local>,
    },
    Temperature,
}

impl Display for GeneralOsdiInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param) => Debug::fmt(param, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::ParamGiven(param) => write!(f, "$param_given({:?})", param),
            Self::SimParam(name, kind) => write!(f, "$simparam({}, {:?})", name, kind),
            Self::Current(branch) => write!(f, "flow({:?})", branch),
            Self::Voltage(hi, lo) => write!(f, "pot({:?}, {:?})", hi, lo),
            Self::Lim { hi, lo, fun, args } => write!(
                f,
                "$limit(pot({:?}, {:?}), {}, {} )",
                hi,
                lo,
                fun,
                ListFormatter(args.as_slice(), ",", ", ")
            ),
            Self::PortFlow(port) => write!(f, "flow({:?})", port),
            Self::Temperature => f.write_str("$temp"),
        }
    }
}

impl InputKind for GeneralOsdiInput {
    fn derivative<C: CallType>(&self, unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        match (self, unknown) {
            (Self::Parameter(x), Unknown::Parameter(y)) if *x == y => Derivative::One,

            (Self::Current(x), Unknown::Flow(y)) if *x == y => Derivative::One,

            (Self::Lim { hi, .. }, Unknown::NodePotential(node)) if hi == node => Derivative::One,

            (Self::Lim { lo, .. }, Unknown::NodePotential(node)) if lo == node => {
                Derivative::Operand(OperandData::Constant(Scalar(Real(-1.0))))
            }

            (Self::Voltage(hi, _), Unknown::NodePotential(node)) if hi == node => Derivative::One,

            (Self::Voltage(lo, _), Unknown::NodePotential(node)) if lo == node => {
                Derivative::Operand(OperandData::Constant(Scalar(Real(-1.0))))
            }

            _ => Derivative::Zero,
        }
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(param) => mir[*param].ty,
            Self::Voltage(_, _)
            | Self::Current(_)
            | Self::Lim { .. }
            | Self::PortFlow(_)
            | Self::Temperature
            | Self::SimParam(_, SimParamKind::RealOptional)
            | Self::SimParam(_, SimParamKind::Real) => Type::REAL,

            Self::ParamGiven(_)
            | Self::PortConnected(_)
            | Self::SimParam(_, SimParamKind::RealOptionalGiven) => Type::BOOL,
            Self::SimParam(_, SimParamKind::String) => Type::STRING,
        }
    }
}

impl ExpressionLowering for GeneralOsdiCall {
    fn port_flow<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        port: PortId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Use(Spanned {
            span,
            contents: OperandData::Read(GeneralOsdiInput::PortFlow(port)),
        }))
    }

    fn branch_access<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        access: DisciplineAccess,
        branch: BranchId,
        span: Span,
    ) -> Option<RValue<Self>> {
        // todo discipline/nature checks
        // todo check that a branch is either a current or a voltage src not both (fuck the standard in that regard)

        let input = match access {
            DisciplineAccess::Potential => {
                let branch = &ctx.fold.mir[branch];
                GeneralOsdiInput::Voltage(branch.hi, branch.lo)
            }
            DisciplineAccess::Flow => GeneralOsdiInput::Current(branch),
        };
        Some(RValue::Use(Spanned {
            span,
            contents: OperandData::Read(input),
        }))
    }

    fn parameter_ref<L: HirLowering>(
        _ctx: &mut LocalCtx<Self, L>,
        param: ParameterId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Use(Spanned {
            span,
            contents: OperandData::Read(GeneralOsdiInput::Parameter(param)),
        }))
    }

    fn time_derivative<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        expr: ExpressionId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Call(
            Self::TimeDerivative,
            index_vec![ctx.fold_real(expr)?],
            span,
        ))
    }

    fn noise<L: HirLowering>(
        _ctx: &mut LocalCtx<Self, L>,
        _source: NoiseSource<ExpressionId, ()>,
        name: Option<ExpressionId>,
        span: Span,
    ) -> Option<RValue<Self>> {
        todo!()
    }

    fn system_function_call<L: HirLowering>(
        ctx: &mut LocalCtx<Self, L>,
        call: &HirSystemFunctionCall,
        span: Span,
    ) -> Option<RValue<Self>> {
        let val = match call {
            HirSystemFunctionCall::Temperature => GeneralOsdiInput::Temperature,
            HirSystemFunctionCall::Lim { access: (DisciplineAccess::Potential, branch), fun: LimFunction::Native(name), args } => {

            }

            _ => todo!(),
        };

        Some(RValue::Use(Spanned {
            span,
            contents: OperandData::Read(val),
        }))
    }

    fn stop_task<L: HirLowering>(
        _ctx: &mut LocalCtx<Self, L>,
        kind: StopTaskKind,
        finish: PrintOnFinish,
        span: Span,
    ) -> Option<StmntKind<Self>> {
        Some(StmntKind::Call(
            Self::StopTask(kind, finish),
            IndexVec::new(),
            span,
        ))
    }
}

pub struct OsdiHirLowerIngCtx<'a> {
    pub sim: &'a Simulator
}

impl<'a> HirLowering for OsdiHirLowerIngCtx<'a> {
    type AnalogBlockExprLower = GeneralOsdiCall;

    fn handle_attribute(ctx: &mut HirFold<Self>, attr: &Attribute, src: AttributeCtx, sctx: _) {
        unimplemented!()
    }

    fn handle_statement_attribute<'a, 'h, C: ExpressionLowering>(
        ctx: &mut LocalCtx<'a, 'h, C, Self>,
        attr: &Attribute,
        stmt: StatementId,
        sctx: SyntaxCtx,
    ) {
        unimplemented!()
    }
}
