/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
 *  No part of rust_adms, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use indextree::{Arena, NodeId};
use log::{debug, error, info, trace, warn};
use pest::iterators::{Pair, Pairs};
use pest::Parser;

use crate::ast::{node_types, RawAst};
use crate::ast;
use crate::parsing::preprocessor::PreprocessedSource;
use crate::parsing::syntax::util::*;
use crate::parsing::util::*;

#[macro_use]
pub(super) mod util;

//Create Parser from grammar
#[derive(Parser)]
#[grammar = "parsing/syntax/grammar.pest"]
struct PestParser;


#[derive(Debug, Clone)]
pub struct ParseTree<'lt>(pub(super)ParseTreeNode<'lt>);

pub fn create_parse_tree(preprocessed_source: &PreprocessedSource) -> std::result::Result<ParseTree, String> {
    let res = PestParser::parse(Rule::VERILOG_AMS, &preprocessed_source.0);
    if let Err(message) = res {
        Err(format!("{}", message))
    } else {
        Ok(ParseTree(res.unwrap().next().unwrap()))
    }
}

pub fn raw_ast_from_preprocessed_source(source: &PreprocessedSource) -> std::result::Result<RawAst, String> {
    let parse_tree = create_parse_tree(source)?;
    let mut parse_tree_ast_folder = ParseTreeToRawAstFolder::new();
    if let Err(e) = parse_tree_ast_folder.fold(parse_tree) {
        return Err(format!("{}", e))
    }
    Ok(parse_tree_ast_folder.collapse())
}

fn hierarchical_identifier_string(matched_pair: Pair<Rule>) -> Vec<String> {
    let mut description = matched_pair.into_inner();
    let mut res = vec!(identifier_string(description.next().unwrap()));
    for identifier in description {
        res.push(identifier_string(identifier));
    };
    res
}


//TODO multithreading
//TODO finish refactor
//TODO state stack as field
pub struct ParseTreeToRawAstFolder {
    ast: RawAst,
    state_stack: Vec<State>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum State {
    AnalogBehavior,
    DigitalBehavior,
    INITIAL,
    BLOCK,
}

impl<'lt> ParseTreeToRawAstFolder {
    pub fn new() -> Self {
        let ast = {
            let mut arena = Arena::new();
            let top_node = arena.new_node(ast::Node::Top);
            RawAst { arena, top_node }
        };
        Self { state_stack: Vec::new(), ast }
    }

    //TODO Documentation
    pub fn fold(&mut self, parse_tree: ParseTree) -> SyntaxResult {
        for parse_tree_node in parse_tree.0.into_inner() {
            self.fold_item_with_attributes(parse_tree_node, self.ast.top_node)?
        }
        Ok(())
    }

    pub fn mut_state(&mut self) -> &mut Vec<State> {
        &mut self.state_stack
    }

    pub fn collapse(self) -> RawAst {
        self.ast
    }
    //TODO function to folds but doesnt return error and prints as many errors possible instead


    //Called when a large amount of notes are possible (like behavioral statement or from the file top)
    fn fold_item(&mut self, parse_tree_node: Pair<Rule>, parent_ast_node: NodeId, attributes: Vec<NodeId>) -> SyntaxResult {
        match parse_tree_node.as_rule() {
            Rule::MODULE => self.fold_module(parse_tree_node, parent_ast_node, attributes),
            Rule::PORT_DECLARATION => self.fold_port_declaration(parse_tree_node, parent_ast_node, attributes),
            Rule::ANALOG => self.fold_analog(parse_tree_node, parent_ast_node, attributes),
            Rule::SEQ_BLOCK => self.fold_block(parse_tree_node, parent_ast_node, attributes),
            Rule::CONDITONAL_STATEMENT => self.fold_condition(parse_tree_node, parent_ast_node, attributes),
            Rule::CONTRIBUTE_STMT => self.fold_contribute(parse_tree_node, parent_ast_node, attributes),
            Rule::BRANCH_DECELERATION => self.fold_branch_declaration(parse_tree_node, parent_ast_node, attributes),
            Rule::VARIABLE_ASSIGNMENT => self.fold_variable_assignment(parse_tree_node, parent_ast_node, attributes),
            Rule::VARIABLE_DECELERATION => self.fold_variable_declaration(parse_tree_node, parent_ast_node, attributes),
            Rule::PARAMETER_DECELERATION => self.fold_parameter_declaration(parse_tree_node, parent_ast_node, attributes),
            Rule::EOI => Ok(()),
            _ => unexpected_rule!(parse_tree_node),
        }
    }
    //Many General Nodes (like behavioral statement, module item or just file level items) allow attributes to be specified
    //this method parses all the attributes and then calls the process_parse_tree_node method to handel the actual contents
    //just here to save me from rewriteting the module parsing code over and over
    fn fold_item_with_attributes(&mut self, mut parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> SyntaxResult {
        match parse_tree_node.as_rule() {
            //Module items may either be an module_or_generate_item for which attributes cant be specified or an port declaration for which attributes may be specified
            Rule::MODULE_ITEM => {
                let next = parse_tree_node.clone().into_inner().next().unwrap();
                if next.as_rule() == Rule::MODULE_OR_GENERATE_ITEM {
                    // A module_or_generate item is itself a general node which allow attributes to be specified
                    // Calling the function again would be costly as the match would have to be reprocessed plus function calling overhead
                    // This is less pretty but fast
                    parse_tree_node = next;
                }
            },
            Rule::MODULE_OR_GENERATE_ITEM | Rule::TOP_ITEM | Rule::BEHAVIORAL_STMT => (),
            //items that do not allow attributes are just ignored
            _ => return self.fold_item(parse_tree_node, parent_ast_node, Vec::new()),
        }

        let mut description = parse_tree_node.into_inner();
        let attributes = self.process_attributes(&mut description)?;
        if let Some(node) = description.next() {
            self.fold_item(node, parent_ast_node, attributes)?;
        }
        Ok(())
    }
    //Processes lists which allow attributes to be declared for each node
    fn fold_list_items_with_attributes(&mut self, parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> SyntaxResult {
        //TODO change to macro (function pointers not necessary as possible functions are known at compiler time (also will be easier to read)
        let process_function =
            match parse_tree_node.as_rule() {
                Rule::PORT_DECLARATION_LIST => {
                    Self::fold_port_declaration
                },
                Rule::PORT_LIST => {
                    //TODO change this to fold function
                    Self::process_port
                }
                _ => unexpected_rule!(parse_tree_node)
            };
        let mut list = parse_tree_node.into_inner();
        while list.peek().is_some() {
            let attributes = self.process_attributes(&mut list)?;
            process_function(self, list.next().unwrap(), parent_ast_node, attributes)?;
        }
        Ok(())
    }
}

//The impl block for the folder is split across the following
//Each module contains methods to process a parse tree nodes relating to a certain categor
mod module;
mod behavior;
mod expression;
mod attributes;
mod variable_declarations;
mod type_info;

//TODO testing

