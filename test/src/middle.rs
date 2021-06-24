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
use openvaf_data_structures::iter;
use openvaf_data_structures::iter::Itertools;
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::{ExpansionPrinter, MultiDiagnostic};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::{LocalKind, VariableLocalKind};
use openvaf_pass::program_dependence::control_dependence::BuildControlDependenceGraph;
use openvaf_pass::program_dependence::data_dependence::BuildUseDefGraph;
use openvaf_pass::program_dependence::BuildPDG;
use openvaf_pass::{
    BackwardSlice, FindAssignments, ReachingDefinitionsAnalysis, RemoveDeadLocals, Simplify,
    SimplifyBranches, Strip, Verify,
};
use std::ops::Deref;

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

        if sess.config.print_verbose_info {
            mir.print_to_file_with_shared(sess.log_file("after_optimizations.mir")?, id, &cfg)
                .wrap_err("Failed to print MIR")?;
        }

        let locations = cfg.intern_locations();
        let assignments = cfg.analyse(FindAssignments(&locations));
        let reaching_definitions = cfg.analyse(ReachingDefinitionsAnalysis {
            assignments: &assignments,
            locations: &locations,
        });
        let mut cursor = reaching_definitions.as_results_cursor(&cfg);
        cursor.seek_to_exit_block_end(&cfg);
        let exit_block_definitions = cursor.finish();
        assert!(!exit_block_definitions.is_empty());

        let use_def_graph = cfg.analyse(BuildUseDefGraph {
            locations: &locations,
            assignments: &assignments,
            reaching_definitions,
        });

        let pdg = cfg.analyse(BuildPDG {
            locations: &locations,
            use_def_graph,
            assignments,
            control_dependence: BuildControlDependenceGraph::default(),
        });

        let warnings = Linter::late_user_diagnostics::<ExpansionPrinter>()?;
        if !warnings.0.is_empty() {
            sess.println(warnings.to_string());
        }

        let vars_to_check = match sess.test_case.unwrap().model.unwrap().0 {
            "DIODE" => vec!["Id", "Qd", "VT", "vcrit"],
            "HICUML2" => vec!["itf", "ibhrec", "ick", "cjei0_t", "Cjep"],
            "BSIM6" => vec!["Ibd", "ids", "Qs", "Qd"],
            _ => return Ok(()), // Only check seleted variable to ensure that we dont accidently use an unused variable
        };

        let locals = cfg
            .locals
            .iter_enumerated()
            .filter_map(|(local, decl)| {
                if let LocalKind::Variable(var, VariableLocalKind::User) = decl.kind {
                    if vars_to_check.contains(&mir[var].ident.as_str().deref()) {
                        return Some((local, mir[var].ident));
                    }
                }
                None
            })
            .collect_vec();

        for (local, name) in locals {
            let assignments = pdg.assignments.row(local);

            let out_set = if let Some(assignments) = assignments {
                if assignments.is_empty() {
                    bail!("No assignments were found for '{}'", name)
                }
                let mut out_set = assignments.clone().to_dense(locations.len());
                out_set.intersect(&exit_block_definitions);
                out_set
            } else {
                bail!("No assignments were found for '{}'", name)
            };

            if out_set.is_empty() {
                bail!(
                    "No assignments to '{}' are valid at the end of the analog block!",
                    name
                )
            }

            let retain = cfg.analyse(
                BackwardSlice::new(&pdg, &locations)
                    .requiring_locals_in(iter::once(local), &exit_block_definitions),
            );
            cfg.modify(Strip {
                retain: &retain,
                locations: &locations,
            });
            cfg.modify(Simplify);
            cfg.modify(ConstantPropagation::default());
            cfg.modify(SimplifyBranches);
            cfg.modify(Simplify);
            let new_locals = cfg.modify(RemoveDeadLocals);
            if new_locals[local] < cfg.locals.len_idx() {
                bail!("'{}' was removed by optimizations/backwarsd slice despite being present in the out set!", name)
            }
        }
    }

    Ok(())
}
