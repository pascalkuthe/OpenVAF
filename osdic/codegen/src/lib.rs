/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

mod cfg;

use codegen_llvm::inkwell::context::Context;
use codegen_llvm::inkwell::targets::{InitializationConfig, Target};

pub(super) struct CodeGenCfg {
    msvc: bool,
    opt_lvl: OptimizationLevel,
    features: String,
    triple: String,
    cpu: String,
}

impl CodeGenCfg {
    pub fn new(args: &CodeGenArgs) -> Result<Self> {
        Target::initialize_all(&InitializationConfig::default());
        let opt_lvl = if args.debug {
            OptimizationLevel::None
        } else {
            OptimizationLevel::Aggressive
        };

        let triple = args.triple();
        let features = args.features();

        let triple_str = triple.as_str().to_bytes();
        let msvc = &triple_str[triple_str.len() - 4..] == b"msvc";

        let target = Target::from_triple(&triple).unwrap();
        if target
            .create_target_machine(
                &triple,
                &args.cpu,
                &features,
                opt_lvl,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .is_none()
        {
            let triple = triple.as_str().to_string_lossy();
            bail!(
                "Target {} with cpu {} and features [{}] is not supported!",
                triple,
                &args.cpu,
                features
            )
        }

        Ok(Self {
            msvc,
            opt_lvl,
            features,
            cpu: args.cpu.clone(),
            triple: triple.as_str().to_str().unwrap().to_owned(),
        })
    }

    pub fn pass_manager_builder(&self) -> PassManagerBuilder {
        let builder = PassManagerBuilder::create();
        builder.set_optimization_level(self.opt_lvl);
        builder
    }

    pub fn target_machine(&self) -> TargetMachine {
        // TODO find out if this not save to just throw into a sync/send wrapper
        let triple = TargetTriple::create(&self.triple);
        let target = Target::from_triple(&triple).unwrap();
        target
            .create_target_machine(
                &TargetTriple::create(&self.triple),
                &self.cpu,
                &self.features,
                self.opt_lvl,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap()
    }
}

fn codegen_test() {
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
    for module in &mir.modules {
        let cfg = module.analog_cfg.borrow();
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
    }

    codegen_ctx.module.verify().unwrap();

    Ok(())
}
