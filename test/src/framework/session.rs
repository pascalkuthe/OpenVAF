/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::models::Model;
use crate::framework::{Config, Test, TestCase};
use color_eyre::eyre::WrapErr;
use eyre::Result;
use indicatif::ProgressBar;
use openvaf_ast::Ast;
use openvaf_ast_lowering::{lower_ast_userfacing, AllowedReferences};
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::ExpansionPrinter;
use openvaf_hir_lowering::{
    lower_hir_userfacing, AttributeCtx, ExpressionLowering, HirFold, HirLowering,
    HirSystemFunctionCall, LocalCtx,
};
use openvaf_ir::ids::{BranchId, ExpressionId, NetId, ParameterId, PortId, StatementId, SyntaxCtx};
use openvaf_ir::{
    Attribute, NoiseSource, PrintOnFinish, SimpleConstVal, Spanned, StopTaskKind,
    SystemFunctionCall, Type, Unknown,
};
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::dfa::lattice::FlatSet;
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_middle::osdi_types::SimpleConstVal::Real;
use openvaf_middle::{
    BinOp, COperand, CallArg, CallType, ConstVal, Derivative, DisciplineAccess, InputKind, Mir,
    Operand, OperandData, RValue, StmntKind,
};
use openvaf_parser::{parse, TokenStream};
use openvaf_preprocessor::{preprocess_user_facing, std_path};
use openvaf_session::sourcemap::Span;
use openvaf_session::SourceMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::path::{Path, PathBuf};
use tracing::debug;

pub struct TestSession<'a> {
    pub config: &'a Config,
    pub models: &'a [Model],
    pub test: &'static Test,
    pub test_case: Option<TestCase>,
    pub(super) pb: ProgressBar,
}

impl<'a> TestSession<'a> {
    pub fn log_file(&self, name: &str) -> Result<PathBuf> {
        let mut res = if let Some(test_case) = self.test_case {
            self.test
                .output_dir()?
                .join(&format!("{}_{}", test_case.name, name))
        } else {
            self.test.output_dir()?.join(name)
        };

        if res.extension().is_none() {
            res.set_extension("log");
        }

        Ok(res)
    }

    pub fn run_preprocessor(&self, file: impl AsRef<Path>) -> Result<TokenStream> {
        let file = self.config.src_dirs.join(file);
        let (sm, main_file) = SourceMap::new_with_mainfile(file.clone())
            .wrap_err_with(|| format!("Failed to open VerilogA root file at {}", file.display()))?;
        let res = preprocess_user_facing(
            sm,
            main_file,
            std_path(
                self.config.src_dirs.join("std").join("constants.vams"),
                self.config.src_dirs.join("std").join("disciplines.vams"),
            ),
        )?;
        Ok(res)
    }

    pub fn run_parser(&self, file: impl AsRef<Path>) -> Result<Ast> {
        let ts = self.run_preprocessor(file)?;
        Ok(parse(ts).map_err(|error| error.user_facing::<ExpansionPrinter>())?)
    }

    pub fn compile_to_mir(&self, file: impl AsRef<Path>) -> Result<Mir<Call>> {
        let ast = self.run_parser(file)?;
        let warnings = Linter::early_user_diagnostics::<ExpansionPrinter>()?;
        if !warnings.0.is_empty() {
            self.println(warnings.to_string());
        }
        let hir = lower_ast_userfacing(ast, |_| AllowedReferences::None)?;
        Ok(lower_hir_userfacing(hir, &mut TestLowering)?)
    }

    pub fn println(&self, msg: impl AsRef<str>) {
        self.pb.println(msg)
    }
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
pub enum Input {
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
pub enum Call {
    StopTask(StopTaskKind, PrintOnFinish),
    Noise,
}

impl CallType for Call {
    type I = Input;

    fn const_fold(&self, _call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal> {
        FlatSet::Top
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
            SystemFunctionCall::ParameterGiven(_param) => RValue::Use(Operand::new(
                OperandData::Constant(Scalar(SimpleConstVal::Bool(true))),
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
