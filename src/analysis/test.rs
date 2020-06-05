//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::constant_folding::{ConstantFoldState, IntermediateWritingConstantFold};
use crate::analysis::data_flow::reaching_variables::ReachableDefinitionsAnalysis;
use crate::analysis::ProgramDependenceGraph;
use crate::ir::{ModuleId, VariableId};
use crate::{Ast, ControlFlowGraph};
use bumpalo::Bump;
use std::fs::File;
use std::path::Path;

#[test]
pub fn cfg() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Debug)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    let mut ast = Ast::new();
    let source_map = ast
        .parse_from_and_print_errors(Path::new("tests/hl2.va"), &source_map_allocator, true)
        .ok_or(())?;

    let mut mir = ast
        .lower_and_print_errors(source_map, true)
        .ok_or(())?
        .lower_and_print_errors(source_map, true)
        .ok_or(())?;

    for module in mir.modules.indices() {
        let mut cfg: ControlFlowGraph = mir[module].contents.analog_cfg.clone();

        let reaching_analysis = ReachableDefinitionsAnalysis::new(&mir, &cfg);

        let (mut udg, _) = reaching_analysis.run(&mut cfg);
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

        let mut control_dependencies = cfg.control_dependence_graph_from_ipdom(&ipdom);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("control_dependence.dot").unwrap();
            cfg.render_control_dependence_to(&mut file, &control_dependencies);
        }

        cfg.constant_fold(
            &mut IntermediateWritingConstantFold(&mut mir),
            &mut udg,
            &mut ConstantFoldState::default(),
            true,
        );

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_constant_fold.dot").unwrap();
            cfg.render_to(&mut file);
        }

        let pdg = ProgramDependenceGraph {
            data_dependencies: udg,
            control_dependencies: control_dependencies,
        };

        // forward transfer current as a pretty complex slice
        let mut itf_id = None;
        for (id, var) in mir.variables.iter_enumerated() {
            if var.contents.name.name.as_str() == "CjCx_i" {
                itf_id = Some(id)
            }
        }

        cfg.backward_variable_slice(itf_id.unwrap(), &pdg);

        cfg.simplify();

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_sliced.dot").unwrap();
            cfg.render_to(&mut file);
        }
    }

    Ok(())
}
