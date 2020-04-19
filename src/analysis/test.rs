//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::analysis::dominator_tree::DominatorTree;
use crate::compact_arena::SafeRange;
use crate::ir::mir::ControlFlowGraph;
use crate::ir::{ModuleId, SafeRangeCreation};
use bumpalo::Bump;
use log::debug;
use rustc_hash::{FxHashSet, FxHasher};
use std::fs::File;
use std::path::Path;

#[test]
pub fn cfg() -> Result<(), ()> {
    fern::Dispatch::new()
        .format(|out, message, record| out.finish(*message))
        .level(log::LevelFilter::Info)
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

        // let mut file = File::create("cfg.dot").unwrap();
        // cfg.render_to(&mut file);
        // let mut file = File::create("cfg_as_dominators.dot").unwrap();
        // cfg.render_dominators_to(&mut file);

        mk_tiny_heap_arena!(dominator_allocator);
        let dominator_tree = DominatorTree::from_cfg(&cfg, dominator_allocator);
        // let mut file = File::create("dominators.dot").unwrap();
        // dominator_tree.render_to(&mut file);

        dominator_tree.variable_extraction(
            &mut cfg,
            &mir,
            FxHashSet::default(),
            |_| (),
            |_| (),
            |_, _| (),
            |_| (),
        );

        mk_tiny_heap_arena!(cfg_allocator);
        let res = cfg.simplify(cfg_allocator);

        // let mut file = File::create("simplified_cfg.dot").unwrap();
        // res.render_to(&mut file);
        assert_eq!(res.block_count(), 1)
    }

    Ok(())
}
