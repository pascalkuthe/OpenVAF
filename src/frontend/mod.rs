/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
 *  No part of rust_adms, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *******************************************************************************************
 */

use std::fs;
use std::path::Path;

use indextree::{Arena, NodeId};
use log::{debug, error, info, warn};
use pest;
use pest::error::ErrorVariant;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;

use crate::frontend::parser::ParseTreeToAstFolder;
use crate::frontend::preprocessor::Preprocessor;

pub mod ast;
#[macro_use]
mod parser;
mod preprocessor;


pub fn run_frontend(file_path: &Path) -> Result<(Arena<ast::Node>, NodeId), ()> {
    let file_contents = fs::read_to_string(file_path).expect("File not found!");
    let mut preprocessor = Preprocessor::new();
    preprocessor.run_preprocessor(&file_contents)?;
    let preprocessed_source = preprocessor.finalize();
    let parse_tree = parser::create_parse_tree(&preprocessed_source)?;
    let parse_tree_ast_fold = ParseTreeToAstFolder::fold(parse_tree)?;
    Ok(parse_tree_ast_fold.finish())
}


