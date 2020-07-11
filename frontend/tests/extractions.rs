//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use common::{preprocess_test, PrettyError, TEST_EXPANSION_HINT};
use open_vaf::analysis::constant_fold::PropagatedConstants;
use open_vaf::analysis::data_flow::reaching_definitions::ReachingDefinitionsAnalysis;
use open_vaf::analysis::ProgramDependenceGraph;
use open_vaf::diagnostic::{ExpansionPrinter, StandardPrinter};
use open_vaf::lints;
use open_vaf::lints::{LintLevel, Linter};
use open_vaf::{Ast, ControlFlowGraph, SourceMap};
use std::fs::File;
use std::path::PathBuf;
use std::str::FromStr;

mod common;

fn extraction_integration_test(
    model: &'static str,
    extracted_var: &'static str,
) -> Result<(), PrettyError> {
    fern::Dispatch::new()
        .format(|out, message, _record| out.finish(*message))
        .level(log::LevelFilter::Warn)
        .chain(std::io::stderr())
        .apply();

    lints::builtin::ignored_display_task.overwrite_lvl_global(LintLevel::Allow);

    let mut test_dir = std::env::current_dir()?.canonicalize()?;
    test_dir.push("tests");

    let mut main_file = PathBuf::new();

    main_file.push("tests");
    main_file.push("integration");
    main_file.push(model);

    let mut file_name = model.to_lowercase();
    file_name.push_str(".va");

    main_file.push(file_name);

    let (sm, main_file) = SourceMap::new_with_mainfile(main_file)?;
    let (ts, sm) = preprocess_test(test_dir, sm, main_file)?;
    let hir = Ast::parse_from_token_stream_user_facing_with_printer::<ExpansionPrinter>(
        ts,
        &sm,
        TEST_EXPANSION_HINT,
    )?
    .lower_user_facing(&sm, TEST_EXPANSION_HINT)?;

    let warnings = Linter::early_user_diagnostics::<StandardPrinter>(&sm, TEST_EXPANSION_HINT)?;

    print!("{}", warnings);

    let mut mir = hir.lower_user_facing(&sm, TEST_EXPANSION_HINT)?;

    for module in mir.modules.indices() {
        let mut cfg: ControlFlowGraph = mir[module].contents.analog_cfg.clone();

        cfg.calculate_all_registered_derivatives(&mut mir);

        let reaching_analysis = ReachingDefinitionsAnalysis::new(&mir, &cfg);

        let mut udg = reaching_analysis.run(&mut cfg);
        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg.dot").unwrap();
            cfg.render_to(&mut file);
            let mut file = File::create("data_dependencies.dot").unwrap();
            cfg.render_to(&mut file);
        }

        let ipdom = cfg.post_dominators();

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("ipdom.dot").unwrap();
            cfg.render_post_dominators(&mut file, &ipdom);
        }

        let control_dependencies = cfg.control_dependence_graph_from_ipdom(&ipdom);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("control_dependence.dot").unwrap();
            cfg.render_control_dependence_to(&mut file, &control_dependencies);
        }

        cfg.constant_propagation(&mut mir, &mut udg, &mut PropagatedConstants::default());

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

        cfg.backward_variable_slice(extracted_var_id.unwrap(), &pdg);

        cfg.simplify();

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_sliced.dot").unwrap();
            cfg.render_to(&mut file);
        }
    }

    let warnings = Linter::late_user_diagnostics::<StandardPrinter>(&sm, TEST_EXPANSION_HINT)?;
    print!("{}", warnings);

    Ok(())
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
