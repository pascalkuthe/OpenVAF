/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

mod parser;
mod preprocessor;
use std::path::{Path, PathBuf};

pub const FRONTEND_TEST: &str = "Parsing";

pub fn parsing_tests_src(name: &'static str) -> PathBuf {
    let mut res = Path::new("parsing").join(name);
    assert!(res.set_extension("va"));
    res
}

test! {
    stage: FRONTEND_TEST,
    name: Parser_Units,
    run: parser::run,
    test_cases: ["module_header","branch","net_declaration","variable_declaration","statements"]
}

test! {
    stage: FRONTEND_TEST,
    name: Macro_Resolution,
    run: preprocessor::macros,
    test_cases: []
}
