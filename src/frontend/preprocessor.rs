/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
 *  No part of rust_adms, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */


use std::{fs, mem};
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::{Error, format};
use std::path::Path;
use std::process::id;

use indextree::{Arena, NodeId};
use log::{debug, error, info, trace, warn};
use pest::error::ErrorVariant;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

use crate::frontend::{ast, parser};
use crate::frontend::ast::{Node, Node_Types};
use crate::frontend::ast::Node_Types::VERILOG_TYPE::UNDECLARED;
use crate::frontend::parser::{};
use crate::pest::Parser;

#[derive(Parser)]
#[grammar = "frontend/preprocessor.pest"]
struct PestParser;

fn identifier_string(matched_pair: Pair<Rule>) -> String {
    if matched_pair.as_str().starts_with("\\") {
        let raw = matched_pair.as_str();
        raw[1..raw.len() - 1].to_string()
    } else {
        matched_pair.as_str().to_string()
    }
}

fn error<T>(error_message: &str, containing_rule: pest::Span)
            -> Result<T> {
    Err(pest::error::Error::new_from_span(
        ErrorVariant::CustomError { message: error_message.to_string() }, containing_rule))
}

pub type Result<T = ()> = std::result::Result<T, pest::error::Error<Rule>>;
type ParseTreeNode<'lifetime> = Pair<'lifetime, Rule>;

//TODO include
pub struct Preprocessor<'lifetime> {
    macros: HashMap<String, MACRO<'lifetime>>,
    preprocessed_source: String,
    calling_macros: Vec<String>,
    calling_macro_arguments: HashMap<String, String>,
}

#[derive(Debug, Clone)]
struct MACRO<'lt> {
    args: Vec<String>,
    text: Option<ParseTreeNode<'lt>>,

}

impl<'lt> Preprocessor<'lt> {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            preprocessed_source: String::from(""),
            calling_macros: Vec::new(),
            calling_macro_arguments: HashMap::new(),
        }
    }

    pub fn run_preprocessor(&mut self, source_code: &'lt str) -> std::result::Result<(), ()> {
        let preprocessor_parse_result
            = PestParser::parse(Rule::PREPROCESSOR, source_code);
        match preprocessor_parse_result {
            Err(message) => {
                error!("{}", message);
                Err(())
            }
            Ok(mut preprocessor_parse_tree_top_nodes) => {
                if let Err(message) = self.process_compiler_directives(preprocessor_parse_tree_top_nodes.next().unwrap().into_inner().next().unwrap()) {
                    error!("{}", message);
                    Err(())
                } else {
                    Ok(())
                }
            }
        }
    }
    fn process_compiler_directives(&mut self, node: ParseTreeNode<'lt>) -> Result {
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
    fn process_compiler_directive(&mut self, compiler_directive_or_code: ParseTreeNode<'lt>) -> Result {
        trace!("Processing compiler declaration from: {:?}", compiler_directive_or_code);
        let compiler_directive = compiler_directive_or_code.into_inner().next().unwrap();
        match compiler_directive.as_rule() {
            Rule::MACRO_DEFINITION => self.process_macro_declaration(compiler_directive)?,
            Rule::MACRO_REFERENCE => self.process_macro_reference(compiler_directive)?,
            Rule::MACRO_CONDITION => self.process_macro_condition(compiler_directive)?,
            _ => { unexpected_rule!(compiler_directive); },
        };
        Ok(())
    }

    fn process_macro_declaration(&mut self, node: ParseTreeNode<'lt>) -> Result {
        trace!("Processing macro declaration from: {:?}", node);
        let mut description: Pairs<'lt, Rule> = node.into_inner();
        description.next();
        let name = identifier_string(description.next().unwrap());
        let mut args: Vec<String> = Vec::new();
        if_rule!(let Some(identifier_list) = description.next() where Rule::IDENTIFIER_LIST => {
            for identifier in identifier_list.into_inner() {
                args.push(identifier_string(identifier));
            }
        });
        let text = description.next();
        debug!("Macro {} with args {:?} declared with the following body", name, args);
        trace! {"{:?}", text}
        //TODO warn when overriden or no effekt for compiler directives
        self.macros.insert(name, MACRO { args, text });
        Ok(())
    }

    fn process_macro_reference(&mut self, node: ParseTreeNode<'lt>) -> Result {
        trace!("Processing macro reference from: {:?}", node);
        let mut description: Pairs<Rule> = node.clone().into_inner();
        let name = identifier_string(description.next().unwrap());
        if self.calling_macros.contains(&name) {
            return error(&format!("MACRO {} WAS CALLED FROM INSIDE ITSELF; MACRO RECURSION IS FORBIDDEN!", name), node.as_span())
        }
        let macro_definition;

        self.calling_macros.push(name.clone());
        if let Some(found) = self.macros.get(&name) {
            macro_definition = found.clone()
        } else {
            return error(&format!("Macro {} not defined here!", name), node.as_span())
        }

        debug!("Macro {} with args {:?} referenced", name, macro_definition.args);
        trace!("Body: {:?}", macro_definition.text);
        if let Some(args_list) = description.next() {
            let mut arg_bindings: HashMap<String, String> = HashMap::new();
            let mut arg_definitions = macro_definition.args.iter();

            {
                let arguments_found = args_list.clone().into_inner().count();
                let arguments_expected = macro_definition.args.len();
                if arguments_expected != arguments_found {
                    return error(&format!("Expected {} arguments for the Macro {} found: {}", arguments_expected, name, arguments_found), args_list.as_span());
                }
            }
            for arg in args_list.into_inner() {
                let arg_name = as_string!(arg_definitions.next().unwrap());
                arg_bindings.insert(arg_name, self.process_macro_argument(arg)?);
            }
            debug!("Arg Bindings {:?}", arg_bindings);
            if macro_definition.text.is_none() {
                debug!("Empty macro {}", name);
                return Ok(())
            }

            mem::swap(&mut self.calling_macro_arguments, &mut arg_bindings);

            let macro_body = macro_definition.text.clone().unwrap();
            for line in macro_body.into_inner() {
                for node in line.into_inner() {
                    match node.as_rule() {
                        Rule::SIMPLE_IDENTIFIER =>
                            if self.calling_macro_arguments.contains_key(node.as_str()) {
                                self.preprocessed_source.push_str(self.calling_macro_arguments.get(node.as_str()).unwrap());
                            } else {
                                self.preprocessed_source.push_str(node.as_str());
                            },
                        Rule::MACRO_CODE => self.preprocessed_source.push_str(node.as_str()),
                        Rule::COMPILER_DIRECTIVE => self.process_compiler_directive(node)?,
                        _ => unexpected_rule!(node)
                    }
                }
                self.preprocessed_source.push('\n');
            }
            self.preprocessed_source.pop();
            mem::swap(&mut self.calling_macro_arguments, &mut arg_bindings);
        } else {
            if macro_definition.args.len() != 0 {
                return error(&format!("Found no arguments for the Macro {} expected: {} ", name, macro_definition.args.len()), node.as_span());
            } else if let Some(ref macro_body) = macro_definition.text {
                for line in macro_body.clone().into_inner() {
//                    println!("{:?}:{}",line.as_rule(),line.as_str());
                    for node in line.into_inner() {
//                        println!("{}",node);
                        match node.as_rule() {
                            Rule::MACRO_CODE | Rule::SIMPLE_IDENTIFIER => self.preprocessed_source.push_str(node.as_str()),
                            Rule::COMPILER_DIRECTIVE => self.process_compiler_directive(node)?,
                            Rule::COMMENT => (),
                            _ => unexpected_rule!(node)
                        }
                    }
                    self.preprocessed_source.push('\n')
                }
                self.preprocessed_source.pop();
            }
        }
        self.calling_macros.pop();
        Ok(())
    }

    fn process_macro_argument(&mut self, arg: ParseTreeNode<'lt>) -> Result<String> {
        let mut arg_str = String::from("");
        mem::swap(&mut self.preprocessed_source, &mut arg_str);
        for arg_token in arg.into_inner() {
            match arg_token.as_rule() {
                Rule::SIMPLE_IDENTIFIER => if self.calling_macro_arguments.contains_key(arg_token.as_str()) {
                    self.preprocessed_source.push_str(self.calling_macro_arguments.get(arg_token.as_str()).unwrap());
                } else {
                    self.preprocessed_source.push_str(arg_token.as_str());
                },
                Rule::COMPILER_DIRECTIVE => self.process_compiler_directive(arg_token)?,
                Rule::MACRO_ARGUMENT_CODE => self.preprocessed_source.push_str(arg_token.as_str()),
                Rule::MACRO_ARGUMENT => {
                    let text = self.process_macro_argument(arg_token)?;
                    self.preprocessed_source.push_str(&text);
                },
                _ => unexpected_rule!(arg_token)
            }
        }
        mem::swap(&mut self.preprocessed_source, &mut arg_str);
        Ok(arg_str)
    }
    fn process_macro_condition(&mut self, node: ParseTreeNode<'lt>) -> Result {
        trace!("Processing macro_condition from: {:?}", node);
        let mut description = node.clone().into_inner();
        let condition_type = description.next().unwrap();
        let if_ndef =
            match condition_type.as_rule() {
                Rule::TOK_IFDEF => false,
                Rule::TOK_IFNDEF => true,
                _ => unexpected_rule!(condition_type)
            };
        {
            let identifier = identifier_string(description.next().unwrap());
            debug!("Macro condition {} {}", if if_ndef { "ifndef" } else { "ifdef" }, identifier);
            if self.macros.contains_key(&identifier) && !if_ndef || !self.macros.contains_key(&identifier) && if_ndef {
                debug!("Its true!");
                return self.process_compiler_directives(description.next().unwrap());
            } else {
                description.next();
            }
        }
        while let Some(current) = description.next() {
            match current.as_rule() {
                Rule::TOK_ELSIF => {
                    let identifier = identifier_string(description.next().unwrap());
                    debug!("Processing ELSE IFDEF for {}", identifier);
                    if self.macros.contains_key(&identifier) {
                        debug!("Its true");
                        return self.process_compiler_directives(description.next().unwrap());
                    } else {
                        description.next();
                    }
                }
                Rule::TOK_ELSEDEF => {
                    debug!("PROCESSING ELSE");
                    return self.process_compiler_directives(description.next().unwrap())
                },
                Rule::TOK_ENDIF => {
                    debug!("no condition was satisfied and no else block was defined");
                    return Ok(())
                },
                _ => unexpected_rule!(current)
            }
        };
        unreachable!()
    }

    pub fn finalize<'res>(self) -> String {
        self.preprocessed_source
    }
}

#[cfg(test)]
mod test{
    use std::fs;
    use crate::setup_logger;
    use crate::frontend::parser::create_parse_tree;
    use super::*;
    use pest::error::{Error, ErrorVariant};
    use pest::RuleType;

    #[test]
    pub fn macro_test(){
        setup_logger();
        let file_contents = fs::read_to_string("tests/macros.va").expect("File not found!");
        let mut preprocessor = Preprocessor::new();
        if let Err(e) = preprocessor.run_preprocessor(&file_contents){
            panic!();
        }
        let preprocessed_source = preprocessor.finalize();
        //Trimming is done here since amount of newlines may change and mostly doesnt make any statement about correctness (as long as necessary ones arent removed)
        //TODO seperate test for trimming?
        assert_eq!(preprocessed_source.replace("\n","").replace("\r",""),"OK1, OK2,__SMS__OK3OK3L,OK4")
    }

    pub fn code_invariance(){
        setup_logger();
        let file_contents = fs::read_to_string("tests/invariance.va").expect("File not found!");
        let mut preprocessor = Preprocessor::new();
        if let Err(e) = preprocessor.run_preprocessor(&file_contents){
            panic!();
        }
        let preprocessed_source = preprocessor.finalize();
        let parse_tree = match create_parse_tree(&preprocessed_source){
            Ok(result) => result,
            Err(e)=>panic!()
        };
        let control_source = fs::read_to_string("tests/invariance_control.va").expect("File not found!");
        let control_parse_tree = match create_parse_tree(&control_source){
            Ok(result) => result,
            Err(e)=>panic!()
        };
        if let Err(e) = assert_eq_parse_tree(parse_tree,control_parse_tree){
            panic!(e)
        }
    }
    fn assert_eq_parse_tree<T:RuleType>(top_node1: Pair<T>,top_node2: Pair<T>) -> std::result::Result<(),String>{
        if top_node1.as_rule()!=top_node2.as_rule() {
            return Err(format!("Rules mismatch for {:?},{:?}",top_node1,top_node2));
        }
        let mut children_tree1 = top_node1.clone().into_inner();
        let mut children_tree2 = top_node2.clone().into_inner();
        while let (Some(child1),Some(child2)) = (children_tree1.next(),children_tree2.next()){
            assert_eq_parse_tree(child1,child2)?
        }
        if children_tree1.next().is_none()&&children_tree2.next().is_none(){
            Ok(())
        }else{
            return Err(format!("Children mismatch for {:?},{:?}",top_node1,top_node2));
        }
    }
}

