/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::convert::Into;
use std::fmt::{Debug, Display, Formatter};
use std::path::{Path, PathBuf};

use openvaf_hir_lowering::{
    lower_hir_userfacing, AttributeCtx, ExpressionLowering, HirFold, HirLowering,
    HirSystemFunctionCall, LocalCtx,
};
use openvaf_ir::ids::{BranchId, ExpressionId, NetId, ParameterId, PortId, StatementId, SyntaxCtx};
use openvaf_ir::{
    Attribute, NoiseSource, PrintOnFinish, Spanned, StopTaskKind, SystemFunctionCall, Unknown,
};

use openvaf_ast_lowering::{lower_ast_userfacing, AllowedReferences};
use openvaf_codegen_llvm::inkwell::values::{BasicValue, BasicValueEnum};
use openvaf_codegen_llvm::{CallTypeCodeGen, CfgCodegen, Intrinsic};
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::StandardPrinter;
use openvaf_middle::const_fold::DiamondLattice;
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_middle::osdi_types::SimpleConstVal::Real;
use openvaf_middle::{
    BinOp, COperand, CallArg, CallType, ConstVal, Derivative, DisciplineAccess, InputKind, Local,
    Mir, Operand, OperandData, ParameterCallType, ParameterInput, RValue, StmntKind, Type,
};
use openvaf_parser::{parse_user_facing, TokenStream};
use openvaf_preprocessor::{preprocess_user_facing, std_path};
use openvaf_session::sourcemap::{SourceMap, Span, StringLiteral};
use openvaf_session::Session;
use std::error::Error;
use std::fmt;
use std::sync::Arc;
use tracing::debug;

#[cfg(test)]
mod parser;

#[cfg(test)]
mod preprocessor;

#[cfg(test)]
mod middle;

#[cfg(test)]
mod code_gen;

#[cfg(test)]
mod osdic_middle;

//#[cfg(test)]
//mod extractions;

pub struct PrettyError {
    contents: Box<dyn Error>,
    sm: Arc<Session>,
}

impl Debug for PrettyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.contents, f)
    }
}

pub fn preprocess_test(file: impl Into<PathBuf>) -> Result<TokenStream, Box<dyn Error>> {
    let (sm, main_file) = SourceMap::new_with_mainfile(file)?;
    let res = preprocess_user_facing(
        sm,
        main_file,
        std_path(
            Path::new("std").join("constants.vams"),
            Path::new("std").join("disciplines.vams"),
        ),
    )?;
    Ok(res)
}
pub fn init_env_logger() {
    let filter = tracing_subscriber::EnvFilter::from_env("OPENVAF_TEST_LOG");
    let layer = tracing_tree::HierarchicalLayer::default()
        .with_indent_lines(true)
        .with_ansi(true)
        .with_wraparound(10)
        .with_verbose_entry(false)
        .with_indent_amount(3);

    use tracing_subscriber::layer::SubscriberExt;
    let subscriber = tracing_subscriber::Registry::default()
        .with(filter)
        .with(layer);
    tracing::subscriber::set_global_default(subscriber);
}

#[allow(unused_must_use)]
pub fn test_session<T>(
    file: impl Into<PathBuf>,
    test: impl FnOnce(TokenStream) -> Result<T, Box<dyn Error>>,
) -> Result<T, PrettyError> {
    init_env_logger();

    let session = Arc::new(Session::new());
    let res = session.run(|| test(preprocess_test(file)?));
    res.map_err(|err| PrettyError {
        contents: err,
        sm: session.clone(),
    })
}

#[allow(unused_must_use)]
fn middle_test<T>(
    file: impl Into<PathBuf>,
    test: impl FnOnce(Mir<Call>) -> Result<T, Box<dyn Error>>,
) -> Result<T, PrettyError> {
    test_session(file, |ts| {
        let ast = parse_user_facing(ts)?;

        let warnings = Linter::early_user_diagnostics::<StandardPrinter>()?;
        print!("{}", warnings);

        let hir = lower_ast_userfacing(ast, |_| AllowedReferences::None)?;
        let mir = lower_hir_userfacing(hir, &mut TestLowering)?;
        test(mir)
    })
}

struct TestLowering;

impl HirLowering for TestLowering {
    type AnalogBlockExprLower = Call;

    fn handle_attribute(_: &mut HirFold<Self>, _: &Attribute, _: AttributeCtx, _: SyntaxCtx) {
        // attribute can be ignored
    }

    fn handle_statement_attribute<'a, 'h, C: ExpressionLowering<Self>>(
        _: &mut LocalCtx<'a, 'h, C, Self>,
        _: &Attribute,
        _: StatementId,
        _: SyntaxCtx,
    ) {
        // attribute can be ignored
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Input {
    Parameter(ParameterId),
    PortConnected(PortId),
    ParamGiven(ParameterId),
    SimParam,
    SimParamStr,
    BranchAccess(DisciplineAccess, BranchId),
    PortFlow(PortId),
    Temperature,
}

impl InputKind for Input {
    fn derivative<C: CallType>(&self, unknown: Unknown, mir: &Mir<C>) -> Derivative<Self> {
        match (self, unknown) {
            (Self::Parameter(x), Unknown::Parameter(y)) if *x == y => Derivative::One,
            (Self::BranchAccess(DisciplineAccess::Flow, x), Unknown::Flow(y)) if *x == y => {
                Derivative::One
            }
            (
                Self::BranchAccess(DisciplineAccess::Potential, branch),
                Unknown::NodePotential(node),
            ) => {
                if mir[*branch].hi == node {
                    Derivative::One
                } else if mir[*branch].lo == node {
                    Derivative::Operand(OperandData::Constant(Scalar(Real(-1.0))))
                } else {
                    Derivative::Zero
                }
            }

            _ => Derivative::Zero,
        }
    }

    fn ty<C: CallType>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(param) => mir[*param].ty,
            Self::BranchAccess(_, _) | Self::PortFlow(_) | Self::Temperature | Self::SimParam => {
                Type::REAL
            }
            Self::PortConnected(_) | Self::ParamGiven(_) => Type::BOOL,
            Self::SimParamStr => Type::STRING,
        }
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param) => Debug::fmt(param, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::ParamGiven(param) => write!(f, "$param_given({:?})", param),
            Self::SimParam => f.write_str("$simparam (ignored)"),
            Self::SimParamStr => f.write_str("$simparam str (ignored)"),
            Self::BranchAccess(access, branch) => write!(f, "{}({:?})", access, branch),
            Self::PortFlow(port) => write!(f, "flow({:?})", port),
            Self::Temperature => f.write_str("$temp"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Call {
    StopTask(StopTaskKind, PrintOnFinish),
    Noise,
}

impl CallType for Call {
    type I = Input;

    fn const_fold(&self, _call: &[DiamondLattice]) -> DiamondLattice {
        DiamondLattice::NotAConstant
    }

    fn derivative<C: CallType>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        match self {
            Self::StopTask(_, _) => unreachable!(),
            Self::Noise => None,
        }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::StopTask(kind, print_mode) => write!(f, "{}({:?})", kind, print_mode),
            Self::Noise => f.write_str("noise (ignored)"),
        }
    }
}

pub const KB: f64 = 1.3806488e-23;
pub const Q: f64 = 1.602176565e-19;

impl ExpressionLowering<TestLowering> for Call {
    fn port_flow(
        _ctx: &mut LocalCtx<Self, TestLowering>,
        port: PortId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Use(Operand::new(
            OperandData::Read(Input::PortFlow(port)),
            span,
        )))
    }

    fn branch_access(
        _ctx: &mut LocalCtx<Self, TestLowering>,
        access: DisciplineAccess,
        branch: BranchId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Use(Operand::new(
            OperandData::Read(Input::BranchAccess(access, branch)),
            span,
        )))
    }

    fn parameter_ref(
        _ctx: &mut LocalCtx<Self, TestLowering>,
        param: ParameterId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Use(Operand::new(
            OperandData::Read(Input::Parameter(param)),
            span,
        )))
    }

    fn time_derivative(
        _ctx: &mut LocalCtx<Self, TestLowering>,
        _: ExpressionId,
        _: Span,
    ) -> Option<RValue<Self>> {
        debug!("Hello time derivative we are ignoring you");
        None
    }

    fn noise(
        _ctx: &mut LocalCtx<Self, TestLowering>,
        _source: NoiseSource<ExpressionId, ()>,
        _name: Option<ExpressionId>,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Call(Self::Noise, IndexVec::new(), span))
    }

    fn system_function_call(
        _ctx: &mut LocalCtx<Self, TestLowering>,
        call: &HirSystemFunctionCall,
        span: Span,
    ) -> Option<RValue<Self>> {
        let res = match *call {
            SystemFunctionCall::Temperature => {
                RValue::Use(Operand::new(OperandData::Read(Input::Temperature), span))
            }
            SystemFunctionCall::PortConnected(port) => RValue::Use(Operand::new(
                OperandData::Read(Input::PortConnected(port)),
                span,
            )),
            SystemFunctionCall::ParameterGiven(param) => RValue::Use(Operand::new(
                OperandData::Read(Input::ParamGiven(param)),
                span,
            )),
            SystemFunctionCall::Simparam(_, _) => {
                RValue::Use(Operand::new(OperandData::Read(Input::SimParam), span))
            }
            SystemFunctionCall::SimparamStr(_) => {
                RValue::Use(Operand::new(OperandData::Read(Input::SimParam), span))
            }
            SystemFunctionCall::Vt(temp) => {
                let temp = match temp {
                    Some(temp) => _ctx.fold_real(temp)?,
                    None => Operand::new(OperandData::Read(Input::Temperature), span),
                };

                RValue::BinaryOperation(
                    Spanned::new(BinOp::Multiply, span),
                    Operand::new(OperandData::Constant(ConstVal::Scalar(Real(KB / Q))), span),
                    temp,
                )
            }
            SystemFunctionCall::Lim { access, .. } => RValue::Use(Spanned {
                span,
                contents: OperandData::Read(Input::BranchAccess(access.0, access.1)),
            }),
        };
        Some(res)
    }

    fn stop_task(
        _ctx: &mut LocalCtx<Self, TestLowering>,
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

    fn collapse_hint(
        _: &mut LocalCtx<Self, TestLowering>,
        _hi: NetId,
        _lo: NetId,
        _span: Span,
    ) -> Option<StmntKind<Self>> {
        Some(StmntKind::NoOp)
    }
}

impl<'lt, 'c> CallTypeCodeGen<'lt, 'c> for Call {
    type CodeGenData = ();

    fn read_input<'a, A: CallType>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        input: &Self::I,
    ) -> BasicValueEnum<'c> {
        match input {
            Input::Parameter(id) => match cg.ctx.mir.parameters[*id].ty {
                Type::REAL => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
                Type::INT => cg.ctx.integer_ty().const_int(0, true).as_basic_value_enum(),
                Type::STRING => cg
                    .ctx
                    .str_literal(StringLiteral::DUMMY)
                    .as_basic_value_enum(),
                _ => todo!("Arrays"),
            },
            Input::PortConnected(_) => cg.ctx.bool_ty().const_int(1, false).as_basic_value_enum(),
            Input::ParamGiven(_) => cg.ctx.bool_ty().const_int(1, false).as_basic_value_enum(),
            Input::SimParam => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
            Input::SimParamStr => cg
                .ctx
                .str_literal(StringLiteral::DUMMY)
                .as_basic_value_enum(),
            Input::BranchAccess(_, _) => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
            Input::PortFlow(_) => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
            Input::Temperature => cg.ctx.real_ty().const_float(300.0).as_basic_value_enum(),
        }
    }

    fn gen_call_rvalue<'a, A: CallType>(
        &self,
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    ) -> BasicValueEnum<'c> {
        cg.ctx.real_ty().const_float(0.0).as_basic_value_enum()
    }

    fn gen_call<'a, A: CallType>(
        &self,
        _cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        _args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    ) {
        debug!("ignoring stop call")
    }

    fn gen_limexp<'a, A: CallType>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        arg: BasicValueEnum<'c>,
    ) -> BasicValueEnum<'c> {
        cg.ctx.build_intrinsic_call(Intrinsic::Exp, &[arg])
    }
}

impl From<ParameterCallType> for Call {
    fn from(_: ParameterCallType) -> Self {
        unreachable!()
    }
}
impl From<ParameterInput> for Input {
    fn from(input: ParameterInput) -> Self {
        match input {
            ParameterInput::Given(param) => Self::ParamGiven(param),
            ParameterInput::Value(param) => Self::Parameter(param),
        }
    }
}
