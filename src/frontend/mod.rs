/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
 *  No part of rust_adms, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *******************************************************************************************
 */




use std::collections::HashMap;
use std::fmt::Error;
use std::fs;
use std::path::Path;

use pest;
use pest::error::ErrorVariant;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;

use ast::*;
use ast::AST;

pub mod ast;
mod parser;

pub fn parse_file(file: &Path) -> parser::Result<AST> {
    ast::create_ast_from_parse_tree(parse_tree_top_node);
}


