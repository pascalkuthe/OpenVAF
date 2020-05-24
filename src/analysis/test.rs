//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::constant_folding::{ConstantFoldState, IntermediateWritingConstantFold};
use crate::analysis::data_flow::reaching_variables::ReachableDefinitionsAnalysis;
use crate::analysis::dominator_tree::DominatorTree;
use crate::ir::{ModuleId, SafeRangeCreation, VariableId};
use crate::simplify;
use bumpalo::Bump;
use std::path::Path;

#[test]
pub fn cfg() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Debug)
        .chain(std::io::stderr())
        .apply();
    let source_map_allocator = Bump::new();
    mk_ast!(ast);
    let source_map = ast
        .parse_from_and_print_errors(Path::new("tests/hl2.va"), &source_map_allocator, true)
        .ok_or(())?;

    let mut mir = ast
        .lower_and_print_errors(source_map, true)
        .ok_or(())?
        .lower_and_print_errors(source_map, true)
        .ok_or(())?;

    for module in SafeRangeCreation::<ModuleId>::full_range(&*mir) {
        mk_tiny_heap_arena!(cfg_allocator);
        let mut cfg = mir[module].contents.analog_cfg.clone_into(cfg_allocator);

        let reaching_analysis = ReachableDefinitionsAnalysis::new(&mir, &cfg);

        let (mut udg, mut dfg) = reaching_analysis.run(&mut cfg);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg.dot").unwrap();
            cfg.render_to(&mut file);
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

        let dtree = DominatorTree::from_cfg(&cfg);
        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("dominators.dot").unwrap();
            dtree.render_to(&mut file);
        }

        cfg.extract_relevant_statements_for_variable(
            unsafe { VariableId::from_raw_index(64) },
            &udg,
            &dfg,
            &dtree,
        );

        simplify!(cfg);

        #[cfg(feature = "graph_debug")]
        {
            let mut file = File::create("cfg_simplified.dot").unwrap();
            cfg.render_to(&mut file);
        }
    }

    Ok(())
}
