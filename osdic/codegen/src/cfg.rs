/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use openvaf_codegen_llvm::inkwell::passes::PassManagerBuilder;
use openvaf_codegen_llvm::inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use openvaf_codegen_llvm::inkwell::OptimizationLevel;
use anyhow::Result;
use target_lexicon::Triple;

pub(super) struct CodeGenCfg {
    msvc: bool,
    opt_lvl: OptimizationLevel,
    features: String,
    triple: String,
    cpu: String,
}

impl CodeGenCfg {
    pub fn new(cpu: String, triple: Option<Triple>, debug: bool, target_features: Option<Vec<String>>) -> Result<Self> {
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
