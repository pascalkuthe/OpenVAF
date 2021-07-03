/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::{Result, TestInitInfo, TestSession};
use eyre::WrapErr;
use openvaf_codegen_llvm::inkwell::context::Context;
use openvaf_codegen_llvm::inkwell::module::Linkage;
use openvaf_codegen_llvm::inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine,
};
use openvaf_codegen_llvm::inkwell::OptimizationLevel;
use openvaf_codegen_llvm::LlvmCodegen;
use openvaf_diagnostics::{ExpansionPrinter, MultiDiagnostic};
use openvaf_middle::cfg::START_BLOCK;
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_pass::{RemoveDeadLocals, Simplify, SimplifyBranches};

test! {
    stage: "Model Compilation",
    name: CODEGEN,
    run: run,
    collect_test_cases: TestInitInfo::model_tests_cases
}

fn run(sess: &TestSession) -> Result<()> {
    let model = sess.test_case.unwrap().model.unwrap().mainfile();

    let mir = sess.compile_to_mir(model)?;

    for (id, module) in mir.modules.iter_enumerated() {
        let mut cfg = module.analog_cfg.borrow_mut();

        let mut errors = MultiDiagnostic(Vec::new());
        cfg.generate_derivatives(&mir, &mut errors);

        // if sess.config.print_verbose_info {
        //     mir.print_to_file_with_shared(sess.log_file("before_optimization.mir")?, id, &cfg)
        //         .wrap_err("Failed to print MIR")?;
        // }

        if !errors.is_empty() {
            return Err(errors.user_facing::<ExpansionPrinter>().into());
        }

        cfg.modify(SimplifyBranches);
        cfg.modify(Simplify);
        cfg.modify(RemoveDeadLocals);
        cfg.modify(ConstantPropagation::default());
        cfg.modify(SimplifyBranches);
        cfg.modify(Simplify);
        cfg.modify(RemoveDeadLocals);

        if sess.config.print_mir {
            mir.print_to_file_with_shared(sess.log_file("after_optimizations.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
        }

        let llvm_context = Context::create();

        Target::initialize_all(&InitializationConfig::default());

        let default_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&default_triple)
            .unwrap()
            .create_target_machine(
                &default_triple,
                "x86-64",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        let target_data = target.get_target_data();
        let mut codegen_ctx = LlvmCodegen::new(&mir, &llvm_context, false, &target_data, "test");
        let function = codegen_ctx.module.add_function(
            &module.ident.name.as_str(),
            llvm_context.void_type().fn_type(&[], false),
            Some(Linkage::External),
        );
        let mut codegen = codegen_ctx.cfg_codegen(&cfg, function);
        codegen
            .ctx
            .builder
            .position_at_end(codegen.blocks[START_BLOCK]);
        codegen.alloc_vars_and_branches(|cg, access, branch| {
            cg.ctx.builder.build_alloca(
                cg.ctx.context.f64_type(),
                &format!("{}({})", access, mir[branch].ident),
            )
        });

        codegen.build_blocks();
        codegen_ctx.builder.build_return(None);

        codegen_ctx.module.verify().unwrap();

        codegen_ctx
            .module
            .print_to_file(sess.log_file("final.ll")?)
            .unwrap();
    }

    Ok(())
}
