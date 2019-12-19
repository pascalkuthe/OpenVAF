/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
 *  No part of rust_adms, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */


use log::*;
use pest::Parser;
use std::{fs, mem};
use std::collections::HashMap;
use typed_arena::Arena;

use crate::parsing::preprocessor::util::*;
use crate::parsing::util::*;

#[cfg(test)]
mod test;
mod util;

#[derive(Parser)]
#[grammar = "parsing/preprocessor/grammar.pest"]
struct PestParser;


//make
pub struct PreprocessedSource(pub(super) String);

impl PreprocessedSource {
    pub fn skip_preprocessor(source: String) -> Self {
        Self(source)
    }
}

pub fn process_file(file: &str) -> std::result::Result<PreprocessedSource, String> {
    let file_contents =
        match fs::read_to_string(file) {
            Ok(res) => res,
            Err(e) => return Err(format!("{}", e))
        };
    let source_container = Arena::new();
    let mut preprocessor = Preprocessor::new(&source_container);
    if let Err(e) = preprocessor.run(&file_contents) {
        return Err(format!("{}", e))
    }
    Ok(preprocessor.collapse())
}

pub struct Preprocessor<'lifetime> {
    source_container: &'lifetime Arena<u8>,

    macros: HashMap<String, MACRO<'lifetime>>,
    calling_macros: Vec<String>,
    calling_macro_arguments: HashMap<String, String>,

    preprocessed_source: String,
}

#[derive(Debug, Clone)]
pub struct MACRO<'lt> {
    args: Vec<String>,
    text: Option<ParseTreeNode<'lt>>,
}

impl<'lt> Preprocessor<'lt> {
    pub fn new(source_container: &'lt Arena<u8>) -> Self {//Arena is passed insntead of created internally because this would be a self referential strucht which would be a pain
        Self {
            source_container,
            macros: HashMap::new(),
            calling_macros: Vec::new(),
            calling_macro_arguments: HashMap::new(),
            preprocessed_source: String::from(""),
        }
    }


    fn process_parsed_source(&mut self, node: ParseTreeNode<'lt>) -> PreprocessorResult {
        for compiler_directive_or_code in node.into_inner() {
            if let Some(current) = compiler_directive_or_code.clone().into_inner().next() {
                match current.as_rule() {
                    Rule::CODE => self.preprocessed_source.push_str(current.as_str()),
                    Rule::COMPILER_DIRECTIVE => self.process_compiler_directive(current)?,
                    _ => unexpected_rule!(current),
                }
            }
        };
        Ok(())
    }
    fn process_compiler_directive(&mut self, compiler_directive_or_code: ParseTreeNode<'lt>) -> PreprocessorResult {
        trace!("Processing compiler declaration from: {:?}", compiler_directive_or_code);
        let compiler_directive = compiler_directive_or_code.into_inner().next().unwrap();
        match compiler_directive.as_rule() {
            Rule::MACRO_DEFINITION => self.process_declaration(compiler_directive)?,
            Rule::MACRO_REFERENCE => self.process_reference(compiler_directive)?,
            Rule::MACRO_CONDITION => self.process_macro_condition(compiler_directive)?,
            Rule::INCLUDE => self.process_include(compiler_directive)?,
            _ => { unexpected_rule!(compiler_directive); },
        };
        Ok(())
    }

    pub fn mutate_macros(&mut self) -> &mut HashMap<String, MACRO<'lt>> {
        &mut self.macros
    }
    pub fn macros(&self) -> &HashMap<String, MACRO<'lt>> {
        &self.macros
    }
    pub fn result(&self) -> &str {
        &self.preprocessed_source
    }
    //not strictly necessary but prettier than making someone call let res = preprocessor.result().to_string(); ....; Preprocess_Source::skip_preprocessor(res);
    pub fn mut_result(&mut self) -> &mut str {
        &mut self.preprocessed_source
    }

    pub fn collapse(self) -> PreprocessedSource {
        PreprocessedSource(self.preprocessed_source)
    }
}

mod macros;
pub mod sources;

