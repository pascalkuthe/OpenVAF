//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


use std::path::Path;

use crate::ast::RawAst;

#[macro_use]
mod util;
#[macro_use]
mod preprocessor;
mod syntax;

//TODO better error information?
pub fn parse_to_unverified_ast(file_path: &Path) -> Result<RawAst, String> {
    let preprocessed_source = preprocessor::process_file(file_path.to_str().unwrap())?;
    syntax::raw_ast_from_preprocessed_source(&preprocessed_source)
}

