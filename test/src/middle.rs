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
use eyre::bail;
use eyre::WrapErr;
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::{ExpansionPrinter, MultiDiagnostic};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_pass::{RemoveDeadLocals, Simplify, SimplifyBranches, Verify};

test! {
    stage: "Model Compilation",
    name: MIDDLE,
    run: run,
    collect_test_cases: TestInitInfo::model_tests_cases
}

fn run(sess: &TestSession) -> Result<()> {
    let model = sess.test_case.unwrap().model.unwrap().mainfile();

    let mir = sess.compile_to_mir(model)?;

    for (id, module) in mir.modules.iter_enumerated() {
        let mut cfg = module.analog_cfg.borrow_mut();

        let malformations = cfg.analyse(Verify(&mir));

        if !malformations.is_empty() {
            bail!("Invalid CFG produced by HIR lowering:\n{}", malformations)
        }

        let mut errors = MultiDiagnostic(Vec::new());
        cfg.generate_derivatives(&mir, &mut errors);

        if sess.config.print_verbose_info {
            mir.print_to_file_with_shared(sess.log_file("before_optimization.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
        }

        if !errors.is_empty() {
            return Err(errors.user_facing::<ExpansionPrinter>().into());
        }

        let malformations = cfg.analyse(Verify(&mir));

        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!("Invalid CFG produced by derivatives:\n{}", malformations)
        }

        cfg.modify(SimplifyBranches);
        let malformations = cfg.analyse(Verify(&mir));

        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!(
                "Invalid CFG produced by SimplifyBranches:\n{}",
                malformations
            )
        }

        cfg.modify(Simplify);
        let malformations = cfg.analyse(Verify(&mir));

        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!("Invalid CFG produced by Simplify:\n{}", malformations)
        }

        cfg.modify(RemoveDeadLocals);

        let malformations = cfg.analyse(Verify(&mir));
        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!(
                "Invalid CFG produced by RemoveDeadLocals:\n{}",
                malformations
            )
        }

        if sess.config.print_verbose_info {
            mir.print_to_file_with_shared(sess.log_file("before_const_prop.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
        }

        cfg.modify(ConstantPropagation::default());

        let malformations = cfg.analyse(Verify(&mir));
        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!(
                "Invalid CFG produced by RemoveDeadLocals:\n{}",
                malformations
            )
        }

        cfg.modify(SimplifyBranches);

        let malformations = cfg.analyse(Verify(&mir));
        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!(
                "Invalid CFG produced by SimplifyBranches:\n{}",
                malformations
            )
        }
        cfg.modify(Simplify);

        let malformations = cfg.analyse(Verify(&mir));
        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!("Invalid CFG produced by Simplify:\n{}", malformations)
        }
        cfg.modify(RemoveDeadLocals);

        let malformations = cfg.analyse(Verify(&mir));
        if !malformations.is_empty() {
            mir.print_to_file_with_shared(sess.log_file("invalid.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
            bail!(
                "Invalid CFG produced by RemoveDeadLocals:\n{}",
                malformations
            )
        }

        let warnings = Linter::late_user_diagnostics::<ExpansionPrinter>()?;
        if !warnings.0.is_empty() {
            sess.println(warnings.to_string());
        }
    }

    Ok(())
}
