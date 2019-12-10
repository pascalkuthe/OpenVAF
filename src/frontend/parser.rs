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
use std::collections::HashMap;
use std::fmt::{Error, format};
use std::path::Path;

use indextree::{Arena, NodeId};
use pest::error::ErrorVariant;
use pest::iterators::{Pair, Pairs};
use regex::{RegexSet, RegexSetBuilder};

use crate::frontend::{ast, parser};
use crate::frontend::ast::Node_Types;
use crate::pest::Parser;

//Create Pest Parser from grammar
#[derive(Parser)]
#[grammar = "frontend/verilog_ams.pest"]
pub struct PestParser;


//Some convenient stuff

pub type ParseResult<T> = std::result::Result<T, pest::error::Error<Rule>>;
type Result = ParseResult<()>;
type ParseTreeNode<'lifetime> = Pair<'lifetime, Rule>;


fn identifier_string(matched_pair: Pair<Rule>) -> String {
    if matched_pair.as_str().starts_with("\\") {
        let raw = matched_pair.as_str();
        raw[1..raw.len() - 1].to_string()
    } else {
        matched_pair.as_str().to_string()
    }
}

fn error(error_message: &str, containing_rule: pest::Span)
         -> Result {
    Err(pest::error::Error::new_from_span(
        ErrorVariant::CustomError { message: error_message.to_string() }, containing_rule))
}


macro_rules! unexpected_rule {
    ( $ unexpected_pair: expr) => { panic! ("Unexpected Rule {:?} from string {}", $ unexpected_pair.as_rule(), $ unexpected_pair.as_str()) }
}
macro_rules! as_string {
($pair:expr) => {$pair.as_str().to_string()}
}
//fn gpair: Pair<Rule>) {
//    panic!("Unexpected Rule {:?} from string {}", pair.as_rule(), pair.as_str());
//}


//Preprocessor and Parse tree generation
//TODO include
pub struct Preprocessor<'lifetime> {
    macros: HashMap<String, MACRO<'lifetime>>,
    preprocessed_source: String,
    calling_macros: Vec<String>,
    calling_macro_arguments: HashMap<String, String>,
}
#[derive(Debug)]
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

    pub fn run_preprocessor(&mut self, source_code: &'lt str) -> Result {
        let mut preprocessor_parse_tree_top_nodes
            = PestParser::parse(Rule::PREPROCESSOR, &source_code)?;
        self.process_compiler_directives(preprocessor_parse_tree_top_nodes.next().unwrap().into_inner().next().unwrap())?;
        Ok(())
    }
    fn process_compiler_directives(&mut self, node: ParseTreeNode<'lt>) -> Result {
        for compiler_directive_or_code in node.into_inner() {
            let current = compiler_directive_or_code.clone().into_inner().next().unwrap();
            match current.as_rule() {
                Rule::CODE => self.preprocessed_source.push_str(current.as_str()),
                Rule::COMPILER_DIRECTIVE => self.process_compiler_directive(current)?,
                _ => unexpected_rule!(current),
            }
        };
        Ok(())
    }
    fn process_compiler_directive(&mut self, compiler_directive_or_code: ParseTreeNode<'lt>) -> Result {
        match compiler_directive_or_code.as_rule() {
            Rule::CODE => {
                self.preprocessed_source.push_str(compiler_directive_or_code.as_str());
                return Ok(());
            },
            Rule::COMPILER_DIRECTIVE => (),
            _ => unexpected_rule!(compiler_directive_or_code),
        }
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
        let mut description: Pairs<'lt, Rule> = node.into_inner();
        description.next();
        let name = identifier_string(description.next().unwrap());
        let mut args: Vec<String> = Vec::new();
        if !description.peek().is_none() && description.peek().unwrap().as_rule() == Rule::IDENTIFIER_LIST {
            for identifier in description.next().unwrap().into_inner() {
                args.push(identifier_string(identifier));
            }
        }
        let text = description.next();
        //TODO warn when overriden or no effekt for compiler directives
        self.macros.insert(name, MACRO { args, text });
        Ok(())
    }

    fn process_macro_reference(&mut self, node: ParseTreeNode<'lt>) -> Result {
        let mut description: Pairs<Rule> = node.clone().into_inner();
        let name = identifier_string(description.next().unwrap());
        if self.calling_macros.contains(&name) {
            return error(&format!("MACRO {} WAS CALLED FROM INSIDE ITSELF; MACRO RECURSION IS FORBIDDEN!", name), node.as_span())
        }
        let macro_definition;

        self.calling_macros.push(name.clone());
        if let Some(found) = self.macros.get(&name) {
            macro_definition = found
        } else {
            return error(&format!("Macro {} not defined here!", name), node.as_span())
        }

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
                let mut arg_value = as_string!(arg);
                if self.calling_macro_arguments.contains_key(&arg_value) {
                    arg_value = self.calling_macro_arguments.get(&arg_value).unwrap().to_string();
                }
                arg_bindings.insert(arg_name, arg_value);
            }

            if macro_definition.text.is_none() {
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
                self.preprocessed_source.push('\n')
            }
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
                            _ => unexpected_rule!(node)
                        }
                    }
                    self.preprocessed_source.push('\n')
                }
            }
        }
        self.calling_macros.pop();
        Ok(())
    }


    fn process_macro_condition(&mut self, node: ParseTreeNode<'lt>) -> Result {
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
            if self.macros.contains_key(&identifier) && !if_ndef || !self.macros.contains_key(&identifier) && if_ndef {
                return self.process_compiler_directives(description.next().unwrap());
            } else {
                description.next();
            }
        }
        while description.peek().is_some() {
            let current = description.next().unwrap();
            match current.as_rule() {
                Rule::TOK_ELSIF => {
                    let identifier = identifier_string(description.next().unwrap());
                    if self.macros.contains_key(&identifier) {
                        return self.process_compiler_directives(description.next().unwrap());
                    } else {
                        description.next();
                    }
                }
                Rule::TOK_ELSEDEF => return self.process_compiler_directives(description.next().unwrap()),
                Rule::TOK_ENDIF => return Ok(()),
                _ => unexpected_rule!(current)
            }
        };
        unreachable!()
    }

    pub fn finalize<'res>(self) -> String {
        self.preprocessed_source
    }
}

pub(super) fn create_parse_tree(preprocessed_source: &str) -> ParseResult<ParseTreeNode> {
    Ok(PestParser::parse(Rule::VERILOG_AMS, preprocessed_source)?.next().unwrap())
}


//TODO multithreading
//TODO finish refactor
//TODO state stack as field
pub struct ParseTreeToAstFolder {
    ast: Arena<ast::Node>,
    ast_top_node: NodeId,
    state_stack: Vec<State>,
}

enum State {
    ANALOG_BEHAVIOR,
    DIGITAL_BEHAVIOR,
    INITAL,
    BLOCK,
}

impl<'lt> ParseTreeToAstFolder {
    pub fn fold(parse_tree: ParseTreeNode) -> ParseResult<Self> {
        let mut ast = Arena::new();
        let mut res = ParseTreeToAstFolder { ast_top_node: ast.new_node(ast::Node::TOP), state_stack: Vec::new(), ast };
        res.process_inner_parse_tree_nodes(parse_tree.into_inner(), res.ast_top_node)?;
        Ok(res)
    }
    fn process_parse_tree_node(&mut self, parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> Result {
        match parse_tree_node.as_rule() {
            Rule::MODULE => self.process_module(parse_tree_node, parent_ast_node),
            Rule::MODULE_ITEM =>
                self.process_parse_tree_node(parse_tree_node.into_inner().next().unwrap(), parent_ast_node),
            Rule::MODULE_OR_GENERATE_ITEM =>
                self.process_parse_tree_node(parse_tree_node.into_inner().next().unwrap(), parent_ast_node),
            Rule::PORT_DECLARATION_LIST => {
                for current_port_declaration_parse_tree in parse_tree_node.into_inner() {
                    self.process_port_declaration(current_port_declaration_parse_tree, parent_ast_node)?
                }
                Ok(())
            },
            Rule::PORT_DECLARATION => self.process_port_declaration(parse_tree_node, parent_ast_node),
            Rule::ANALOG => self.process_analog(parse_tree_node, parent_ast_node),
            Rule::BEHAVIORAL_STMT => self.process_behavioral_stmt(parse_tree_node, parent_ast_node),
            Rule::SEQ_BLOCK => self.process_block(parse_tree_node, parent_ast_node),
            Rule::CONDITONAL_STATEMENT => self.process_if(parse_tree_node, parent_ast_node),
            Rule::CONSTANT_EXPRESSION => self.process_constant_expression(parse_tree_node, parent_ast_node),
            Rule::EOI => Ok(()),
            _ => unexpected_rule!(parse_tree_node),
        }
    }
    fn process_module(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> Result {
        let mut description = parse_tree_node.into_inner();
        description.next();
        let ast_node = self.ast.new_node(ast::Node::MODULE(String::from("")));
        self.process_attributes(&mut description, ast_node)?;
        let name = identifier_string(description.next().unwrap());
        *self.ast.get_mut(ast_node).unwrap().get_mut() = ast::Node::MODULE(name);
        while description.peek().unwrap().as_rule() != Rule::TOK_ENDMODULE {
            self.process_parse_tree_node(description.next().unwrap(), ast_node)?
        }
        parent_ast_node.append(ast_node, &mut self.ast);
        Ok(())
    }
    fn process_analog(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> Result {
        self.state_stack.push(State::ANALOG_BEHAVIOR);
        let mut description = parse_tree_node.into_inner();
        description.next();
        let inital_or_behavior = description.next().unwrap();
        let node;
        match inital_or_behavior.as_rule() {
            Rule::TOK_INITIAL => {
                node = self.ast.new_node(ast::Node::ANALOG(true));
                self.state_stack.push(State::INITAL);
                self.process_behavioral_stmt(description.next().unwrap(), node)?;
                self.state_stack.pop();
            },
            Rule::BEHAVIORAL_STMT => {
                node = self.ast.new_node(ast::Node::ANALOG(false));
                self.process_behavioral_stmt(inital_or_behavior, node)?;
            },
            _ => unexpected_rule!(inital_or_behavior),
        };
        parent_ast_node.append(node, &mut self.ast);
        self.state_stack.pop();
        Ok(())
    }
    fn process_inner_parse_tree_nodes(&mut self, inner_parse_tree_nodes: Pairs<Rule>, parent_node: NodeId) -> Result {
        for parse_tree_node in inner_parse_tree_nodes {
            self.process_parse_tree_node(parse_tree_node, parent_node)?;
        }
        Ok(())
    }

    fn process_port_declaration(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> Result {
        let mut description = parse_tree_node.clone().into_inner();
        let ast_node = self.ast.new_node(ast::Node::MODPORT(None));
        let mut port_info = ast::Node_Types::PORT::new();
        self.process_attributes(&mut description, ast_node)?;
        let inout_declaration = description.next().unwrap();
        match inout_declaration.as_rule() {
            Rule::TOK_IN => {
                port_info.is_input = true;
            },
            Rule::TOK_OUT => {
                port_info.is_output = true;
            }
            Rule::TOK_INOUT => {
                port_info.is_output = true;
                port_info.is_input = true;
            }
            _ => unexpected_rule!(inout_declaration),
        }

        while description.peek().is_some() && description.peek().unwrap().as_rule() != Rule::IDENTIFIER_LIST && description.peek().unwrap().as_rule() != Rule::VARIABEL_IDENTIFIER_LIST {
            let port_type_property = description.next().unwrap();
            match port_type_property.as_rule() {
                Rule::IDENTIFIER => port_info.discipline = identifier_string(port_type_property),
                Rule::TOK_SIGNED => port_info.is_signed = true,
                Rule::RANGE_DECL => self.process_range(port_type_property, ast_node)?,
                _ => port_info.verilog_type = Self::process_type(port_type_property),
            }
        }

        let mut identifier_list = description.next().unwrap().into_inner();
        while identifier_list.peek().is_some() {
            let current_port = port_info.clone();
            port_info.identifier = identifier_string(identifier_list.next().unwrap());
            let current_node = self.ast.new_node(ast::Node::MODPORT(Some(current_port)));
            let children: Vec<NodeId> = ast_node.children(&self.ast).collect();
            for child in children {
                current_node.append(child, &mut self.ast);
            }
            parent_ast_node.append(current_node, &mut self.ast);
            if identifier_list.peek().is_some() && identifier_list.peek().unwrap().as_rule() == Rule::CONSTANT_EXPRESSION {
                self.process_constant_expression(identifier_list.next().unwrap(), current_node)?;
            }
        }
        //remove temporary node
        ast_node.remove(&mut self.ast);
        Ok(())
    }


    fn process_behavioral_stmt(&mut self, matched_parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> Result {
        self.process_inner_parse_tree_nodes(matched_parse_tree_node.into_inner(), parent_ast_node)
    }


    fn process_if(&mut self, matched_parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> Result {
        let mut description = matched_parse_tree_node.clone().into_inner();
        description.next();
        let ast_node = self.ast.new_node(ast::Node::COND);
        parent_ast_node.append(ast_node, &mut self.ast);
        self.process_expression(description.next().unwrap(), ast_node)?;
        self.process_behavioral_stmt(description.next().unwrap(), ast_node)?;
        while description.peek().is_some() {
            let current_parse_tree_node = description.next().unwrap();
            if current_parse_tree_node.as_rule() == Rule::TOK_ELSE {
                match description.next().unwrap().as_rule() {
                    Rule::TOK_IF => {
                        self.process_expression(description.next().unwrap(), ast_node)?;
                        self.process_behavioral_stmt(description.next().unwrap(), ast_node)?;
                    },
                    Rule::BEHAVIORAL_STMT => { self.process_behavioral_stmt(description.next().unwrap(), parent_ast_node)?; },

                    _ => unexpected_rule!(matched_parse_tree_node),
                }
            } else {
                unexpected_rule!(matched_parse_tree_node);
            }
        };
        Ok(())
    }


    fn process_range(&mut self, matched_parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> Result {
        let range_node = self.ast.new_node(ast::Node::RANGE);
        parent_ast_node.append(range_node, &mut self.ast);
        let mut description = matched_parse_tree_node.into_inner();
        self.process_constant_expression(description.next().unwrap(), parent_ast_node)?;
        self.process_constant_expression(description.next().unwrap(), parent_ast_node)?;
        Ok(())
    }

    fn process_type(matched_pair: Pair<Rule>) -> Node_Types::VERILOG_TYPE {
        let actual_pair =
            if let Rule::NET_TYPE = matched_pair.as_rule() {
                matched_pair.into_inner().next().unwrap()
            } else {
                matched_pair
            };
        match actual_pair.as_rule() {
            Rule::TOK_WREAL => Node_Types::VERILOG_TYPE::WREAL,
            Rule::TOK_SUPPLY0 => Node_Types::VERILOG_TYPE::SUPPLY0,
            Rule::TOK_SUPPLY1 => Node_Types::VERILOG_TYPE::SUPPLY1,
            Rule::TOK_TRI => Node_Types::VERILOG_TYPE::TRI,
            Rule::TOK_TRIAND => Node_Types::VERILOG_TYPE::TRIAND,
            Rule::TOK_TRIOR => Node_Types::VERILOG_TYPE::TRIOR,
            Rule::TOK_TRI0 => Node_Types::VERILOG_TYPE::TRI0,
            Rule::TOK_TRI1 => Node_Types::VERILOG_TYPE::TRI1,
            Rule::TOK_WIRE => Node_Types::VERILOG_TYPE::WIRE,
            Rule::TOK_UWIRE => Node_Types::VERILOG_TYPE::UWIRE,
            Rule::TOK_WAND => Node_Types::VERILOG_TYPE::WAND,
            Rule::TOK_WOR => Node_Types::VERILOG_TYPE::WOR,
            Rule::TOK_REG => Node_Types::VERILOG_TYPE::REG,
            Rule::TOK_INTEGER => Node_Types::VERILOG_TYPE::INTEGER,
            Rule::TOK_TIME => Node_Types::VERILOG_TYPE::TIME,
            _ => unexpected_rule!(actual_pair),
        }
    }


    fn process_block(&mut self, matched_parse_tree_node: Pair<Rule>, parent_node: NodeId) -> Result {
        self.state_stack.push(State::BLOCK);
        let mut parse_tree_nodes_in_block = matched_parse_tree_node.into_inner();
        parse_tree_nodes_in_block.next();
        let ident_or_behavior = parse_tree_nodes_in_block.next().unwrap();
        match ident_or_behavior.as_rule() {
            Rule::IDENTIFIER => {
                let block_name = identifier_string(ident_or_behavior);
                let block_node = self.ast.new_node(ast::Node::BLOCK(Some(block_name)));
                parent_node.append(block_node, &mut self.ast);
                self.process_inner_parse_tree_nodes(parse_tree_nodes_in_block, block_node)?
            },
            Rule::BEHAVIORAL_STMT => {
                let block_node = self.ast.new_node(ast::Node::BLOCK(None));
                parent_node.append(block_node, &mut self.ast);
                self.process_parse_tree_node(ident_or_behavior, block_node)?
            },
            _ => unexpected_rule!(ident_or_behavior),
        };
        self.state_stack.pop();
        Ok(())
    }


    fn process_attributes(&mut self, parse_tree_nodes: &mut Pairs<Rule>
                          , parent_ast_node: NodeId) -> Result {
        let node_iterator = parse_tree_nodes.into_iter();
        let mut attributes: HashMap<String, NodeId> = HashMap::new();
        let mut attribute_list;
        while node_iterator.peek().is_some() && node_iterator.peek().unwrap().as_rule() == Rule::ATTRIBUTE {
            attribute_list = node_iterator.next().unwrap();
            for attribute in attribute_list.into_inner() {
                let mut description = attribute.into_inner();
//                    let identifier = as_string!(description.next().unwrap());
                let identifier = as_string!(description.next().unwrap());
                let node = self.ast.new_node(ast::Node::ATTRIBUTE(identifier.clone()));
                if let Some(value) = description.next() {
                    self.process_constant_expression(value, node)?;
                }
                //Overwrite if attribute already declared
                if attributes.contains_key(&identifier) {
                    attributes.get(&identifier).unwrap().remove(&mut self.ast);
                }
                attributes.insert(identifier, node);
                parent_ast_node.append(node, &mut self.ast)
            }
        }
        Ok(())
    }


    //TODO implement constant expression
    //TODO make return type result
    fn process_constant_expression(&mut self, parse_tree_node: Pair<Rule>
                                   , parent_ast_node: NodeId) -> Result {
        self.process_expression(parse_tree_node, parent_ast_node)?;
        //TODO check if actually constant and resolve
        Ok(())
    }

    //TODO implement expression
    fn process_expression(&mut self, parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> Result {
//        unimplemented!()
        Ok(())
    }

    pub fn finish(self) -> (Arena<ast::Node>, NodeId) {
        (self.ast, self.ast_top_node)
    }
}
