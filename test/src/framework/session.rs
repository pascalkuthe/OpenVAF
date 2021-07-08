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
use openvaf_ast_lowering::lower_ast_userfacing;
use openvaf_codegen_llvm::inkwell::values::{BasicValue, BasicValueEnum};
use openvaf_codegen_llvm::{CallTypeCodeGen, CfgCodegen, Intrinsic};
use openvaf_data_structures::index_vec::IndexSlice;
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::ExpansionPrinter;

use derive_more::{Display, From, TryInto};
use openvaf_hir::lowering::{lower_hir_userfacing, AttributeCtx, HirFold, HirLowering, LocalCtx};
use openvaf_hir::AllowedOperations;
use openvaf_ir::ids::{CallArg, StatementId, SyntaxCtx};
use openvaf_ir::{Attribute, Print, StopTask, Type};
use openvaf_macros::CfgFunctions;
use openvaf_middle::functions::{
    AcStimulus, Analysis, CfgFunction, CfgFunctionEnum, CurrentLim, DefaultFunctions,
    Discontinuity, NodeCollapse, NoiseCall, TimeDerivative, VoltageLim,
};
use openvaf_middle::inputs::{DefaultInputs, ParameterInput, SimParamKind};
use openvaf_middle::{
    derivatives::RValueAutoDiff, dfa::lattice::FlatSet, COperand, CfgFunctions, ConstVal, Mir,
    RValue,
};
use openvaf_parser::{parse, TokenStream};
use openvaf_preprocessor::{preprocess_user_facing, std_path};
use openvaf_session::sourcemap::{Span, StringLiteral};
use openvaf_session::SourceMap;
use std::fmt::Debug;
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

    pub fn compile_to_mir(&self, file: impl AsRef<Path>) -> Result<Mir> {
        let ast = self.run_parser(file)?;
        let warnings = Linter::early_user_diagnostics::<ExpansionPrinter>()?;
        if !warnings.0.is_empty() {
            self.println(warnings.to_string());
        }
        let hir = lower_ast_userfacing(ast, |_| AllowedOperations::empty())?;
        Ok(lower_hir_userfacing(hir, &mut TestLowering)?)
    }

    pub fn println(&self, msg: impl AsRef<str>) {
        self.pb.println(msg)
    }
}

struct TestLowering;

impl HirLowering for TestLowering {
    fn handle_attribute(_: &mut HirFold<Self>, _: &Attribute, _: AttributeCtx, _: SyntaxCtx) {
        // attribute can be ignored
    }

    fn handle_statement_attribute<'a, 'h>(
        _: &mut LocalCtx<'a, 'h, Self>,
        _: &Attribute,
        _: StatementId,
        _: SyntaxCtx,
    ) {
        // attribute can be ignored
    }
}

#[derive(Clone, Debug, Display, From, TryInto, CfgFunctions)]
#[cfg_inputs(DefaultInputs)]
pub enum TestFunctions {
    Noise(NoiseCall),
    TimeDerivative(TimeDerivative),
    StopTask(StopTask),
    Print(Print),
    NodeCollapse(NodeCollapse),
    VoltageLim(VoltageLim),
    CurrentLim(CurrentLim),
    AcStimulus(AcStimulus),
    Analysis(Analysis),
    Discontinuity(Discontinuity),
}

impl<'lt, 'c> CallTypeCodeGen<'lt, 'c> for TestFunctions {
    type CodeGenData = ();

    fn read_input<'a, A: CfgFunctions>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        input: &Self::I,
    ) -> BasicValueEnum<'c> {
        match input {
            DefaultInputs::Parameter(ParameterInput::Value(id)) => {
                match cg.ctx.mir.parameters[*id].ty {
                    Type::REAL => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
                    Type::INT => cg.ctx.integer_ty().const_int(0, true).as_basic_value_enum(),
                    Type::STRING => cg
                        .ctx
                        .str_literal(StringLiteral::DUMMY)
                        .as_basic_value_enum(),
                    _ => todo!("Arrays"),
                }
            }
            DefaultInputs::Parameter(ParameterInput::Given(_)) => {
                cg.ctx.bool_ty().const_int(1, false).as_basic_value_enum()
            }
            DefaultInputs::PortConnected(_) => {
                cg.ctx.bool_ty().const_int(1, false).as_basic_value_enum()
            }
            DefaultInputs::SimParam(param) => match param.kind {
                SimParamKind::Real => cg.ctx.real_ty().const_float(0.0).as_basic_value_enum(),
                SimParamKind::RealOptional => {
                    cg.ctx.real_ty().const_float(0.0).as_basic_value_enum()
                }
                SimParamKind::RealOptionalGiven => {
                    cg.ctx.bool_ty().const_zero().as_basic_value_enum()
                }
                SimParamKind::String => cg
                    .ctx
                    .str_literal(StringLiteral::DUMMY)
                    .as_basic_value_enum(),
            },

            DefaultInputs::Voltage(_)
            | DefaultInputs::CurrentProbe(_)
            | DefaultInputs::PartialTimeDerivative(_) => {
                cg.ctx.real_ty().const_float(0.0).as_basic_value_enum()
            }
            DefaultInputs::Temperature(_) => {
                cg.ctx.real_ty().const_float(300.0).as_basic_value_enum()
            }
        }
    }

    fn gen_call_rvalue<'a, A: CfgFunctions>(
        &self,
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        _args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    ) -> BasicValueEnum<'c> {
        if matches!(self, Self::Analysis(_)) {
            cg.ctx.bool_ty().const_zero().as_basic_value_enum()
        } else {
            cg.ctx.real_ty().const_float(0.0).as_basic_value_enum()
        }
    }

    fn gen_call<'a, A: CfgFunctions>(
        &self,
        _cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        _args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    ) {
        debug!("ignoring stop call")
    }

    fn gen_limexp<'a, A: CfgFunctions>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, (), A, Self>,
        arg: BasicValueEnum<'c>,
    ) -> BasicValueEnum<'c> {
        cg.ctx.build_intrinsic_call(Intrinsic::Exp, &[arg])
    }
}

impl From<DefaultFunctions> for TestFunctions {
    fn from(fun: DefaultFunctions) -> Self {
        match fun {
            DefaultFunctions::Noise(x) => x.into(),
            DefaultFunctions::TimeDerivative(x) => x.into(),
            DefaultFunctions::StopTask(x) => x.into(),
            DefaultFunctions::Print(x) => x.into(),
            DefaultFunctions::NodeCollapse(x) => x.into(),
            DefaultFunctions::VoltageLim(x) => x.into(),
            DefaultFunctions::CurrentLim(x) => x.into(),
            DefaultFunctions::AcStimulus(x) => x.into(),
            DefaultFunctions::Analysis(x) => x.into(),
            DefaultFunctions::Discontinuity(x) => x.into(),
        }
    }
}
