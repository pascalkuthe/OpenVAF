//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::{test_session, PrettyError};
use openvaf_analysis::lints::{lint_unreadable_assignments, lint_unused_items};
use openvaf_analysis::ReachingDefinitionsAnalysis;
use openvaf_analysis::{
    backward_variable_slice, post_dominators, ControlDependenceGraph, ProgramDependenceGraph,
};
use openvaf_analysis::{conditional_constant_propagation, GlobalConstants};
use openvaf_ast_lowering::lower_ast_userfacing;
use openvaf_derivatives::calculate_all_registered_derivatives;
use openvaf_diagnostics::lints::{LintLevel, Linter};
use openvaf_diagnostics::{lints, StandardPrinter};
use openvaf_hir_lowering::lower_hir_userfacing;
use openvaf_parser::parse_user_facing;
use std::fs::File;
use std::path::PathBuf;

fn extraction_integration_test(
    model: &'static str,
    extracted_var: &'static str,
) -> Result<(), PrettyError> {
    let mut main_file = PathBuf::new();

    main_file.push("integration");
    main_file.push(model);

    let mut file_name = model.to_lowercase();
    file_name.push_str(".va");

    main_file.push(file_name);

    test_session(main_file, |ts| {
        // Bugs/oversights/deliberate in models
        // this is not helpful during tests so we just allows this
        lints::builtin::ignored_display_task.overwrite_lvl_global(LintLevel::Allow);
        lints::builtin::dead_code.overwrite_lvl_global(LintLevel::Allow);
        lints::builtin::unused_variables.overwrite_lvl_global(LintLevel::Allow);
        lints::builtin::unused_parameters.overwrite_lvl_global(LintLevel::Allow);

        let ast = parse_user_facing(ts)?;
        let hir = lower_ast_userfacing(ast)?;

        let warnings = Linter::early_user_diagnostics::<StandardPrinter>()?;

        print!("{}", warnings);

        let mut mir = lower_hir_userfacing(hir)?;

        calculate_all_registered_derivatives(&mut mir)
            .map_err(|err| err.user_facing::<StandardPrinter>())?;

        // Late lints
        lint_unused_items(&mir);

        for module in mir.modules.indices() {
            let cfg = mir[module].contents.analog_cfg.clone();
            let mut cfg_borrow = cfg.borrow_mut();

            conditional_constant_propagation(
                &mut cfg_borrow,
                &mut mir,
                &mut GlobalConstants::default(),
            );

            let reaching_analysis = ReachingDefinitionsAnalysis::new(&mir, &cfg_borrow);
            let udg = reaching_analysis.run(&mut cfg_borrow);
            #[cfg(feature = "graph_debug")]
            {
                let mut file = File::create("cfg.dot").unwrap();
                cfg.render_to(&mut file);
                let mut file = File::create("data_dependencies.dot").unwrap();
                cfg.render_to(&mut file);
            }

            lint_unreadable_assignments(&mir, &cfg_borrow, &udg);
            let ipdom = post_dominators(&cfg_borrow);

            #[cfg(feature = "graph_debug")]
            {
                let mut file = File::create("ipdom.dot").unwrap();
                cfg.render_post_dominators(&mut file, &ipdom);
            }

            let control_dependencies = ControlDependenceGraph::from_ipdom(&cfg_borrow, &ipdom);

            #[cfg(feature = "graph_debug")]
            {
                let mut file = File::create("control_dependence.dot").unwrap();
                cfg.render_control_dependence_to(&mut file, &control_dependencies);
            }

            #[cfg(feature = "graph_debug")]
            {
                let mut file = File::create("cfg_constant_fold.dot").unwrap();
                cfg.render_to(&mut file);
            }

            let pdg = ProgramDependenceGraph {
                data_dependencies: udg,
                control_dependencies,
            };

            let mut extracted_var_id = None;
            for (id, var) in mir.variables.iter_enumerated() {
                if var.contents.ident.name.as_str() == extracted_var {
                    extracted_var_id = Some(id)
                }
            }

            backward_variable_slice(&mut cfg_borrow, extracted_var_id.unwrap(), &pdg);

            cfg_borrow.simplify();

            #[cfg(feature = "graph_debug")]
            {
                let mut file = File::create("cfg_sliced.dot").unwrap();
                cfg.render_to(&mut file);
            }
        }

        let warnings = Linter::late_user_diagnostics::<StandardPrinter>()?;
        print!("{}", warnings);

        Ok(())
    })
}

macro_rules! extraction_test {
    (extract $var:literal from $model:ident) => {
        #[test]
        pub fn $model() -> Result<(), PrettyError> {
            extraction_integration_test(stringify!($model), $var)
        }
    };
}

extraction_test!(extract "itf" from HICUML2);
extraction_test!(extract "IC" from BSIMSOI);
extraction_test!(extract "ids" from BSIMBULK);
extraction_test!(extract "IGTOT" from BSIMCMG);
extraction_test!(extract "ID" from BSIMIMG);
extraction_test!(extract "Ibe" from VBIC_4T_IT_XF_HO);
extraction_test!(extract "I_d" from DIODE);
