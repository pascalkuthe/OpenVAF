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
use std::collections::HashMap;
use std::mem;
use typed_arena::Arena;

use crate::error::PreprocessorResult;
use crate::parsing::util::*;
use crate::parsing::Source;
use pest::iterators::Pair;
use std::convert::TryFrom;

#[cfg(test)]
mod test;

pub(crate) type ParseTreeNode<'lifetime> = Pair<'lifetime, Rule>;

#[derive(Parser)]
#[grammar = "parsing/preprocessor/grammar.pest"]
pub struct PestParser;

impl Source {
    /// Creates a Source object directly from a source String without invoking the preprocessor
    /// *Note* Parsing a source created using this method may fail even tough the syntax is correct
    /// as compiler directives are exclusively handled by the preprocessor
    /// #Exampels
    /// TODO demonstrate failure
    pub fn skip_preprocessing(source: String) -> Self {
        Self { raw: source }
    }
}

impl TryFrom<&str> for Source {
    type Error = pest::error::Error<Rule>;

    /// Invokes the preprocessor and returns the resulting Source Object
    /// #Arguments
    /// * 'unprocessed_source' the source file contends as a String
    fn try_from(unprocessed_source: &str) -> PreprocessorResult<Self> {
        let source_allocator = Arena::new();
        let mut preprocessor = Preprocessor::new(&source_allocator);
        preprocessor.run(unprocessed_source)?;
        Ok(preprocessor.collapse())
    }
}
impl TryFrom<String> for Source {
    type Error = pest::error::Error<Rule>;
    /// Invokes the preprocessor and returns the resulting Source Object
    /// #Arguments
    ///* 'unprocessed_source' the source file contends as a String
    fn try_from(unprocessed_source: String) -> PreprocessorResult<Self> {
        Self::try_from(&unprocessed_source[..])
    }
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
    pub fn new(source_container: &'lt Arena<u8>) -> Self {
        //Arena is passed insntead of created internally because this would be a self referential strucht which would be a pain
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
        }
        Ok(())
    }
    fn process_compiler_directive(
        &mut self,
        compiler_directive_or_code: ParseTreeNode<'lt>,
    ) -> PreprocessorResult {
        trace!(
            "Processing compiler declaration from: {:?}",
            compiler_directive_or_code
        );
        let compiler_directive = compiler_directive_or_code.into_inner().next().unwrap();
        match compiler_directive.as_rule() {
            Rule::MACRO_DEFINITION => self.process_declaration(compiler_directive)?,
            Rule::MACRO_REFERENCE => self.process_reference(compiler_directive)?,
            Rule::MACRO_CONDITION => self.process_macro_condition(compiler_directive)?,
            Rule::INCLUDE => self.process_include(compiler_directive)?,
            _ => {
                unexpected_rule!(compiler_directive);
            }
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

    pub fn collapse(self) -> Source {
        Source {
            raw: self.preprocessed_source,
        }
    }
}

mod macros;
pub mod sources;
