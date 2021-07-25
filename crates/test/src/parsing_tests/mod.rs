use crate::framework::{TestInitInfo, model_root_file};

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

pub const FRONTEND_TEST: &str = "Parsing";

test! {
    stage: FRONTEND_TEST,
    name: Parser_Units,
    run: parser::run,
    test_cases: [
        ("module_header", parser::src::MODULE_HEADER),
        ("branch", parser::src::BRANCH),
        ("net_declaration", parser::src::NET_DECLARATION),
        ("variable_declaration", parser::src::VARIABLE_DECLARATION),
        ("statements", parser::src::STATEMENTS),
        ("contribute", parser::src::CONTRIUBTE)
    ],
}

test! {
    stage: FRONTEND_TEST,
    name: Parser_Integration,
    run: parser::run,
    collect_test_cases: TestInitInfo::model_tests_cases,
    root_file: model_root_file,
}

test! {
    stage: FRONTEND_TEST,
    name: Macro_Resolution,
    run: preprocessor::macros,
    test_cases: [],
    root_file: const preprocessor::SRC,
}

#[doc(hidden)]
pub(crate) fn __keep_alive() {}
