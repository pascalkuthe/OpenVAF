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

use pest;
use pest::error::ErrorVariant;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;

use ast::*;

pub mod ast;

#[derive(Parser)]
#[grammar = "frontend/verilog_ams.pest"]
struct VerilogAmsParser;

enum State {
    ANALOG_BEHAVIOR,
    DIGITAL_BEHAVIOR,
}

pub type Result<T> = std::result::Result<T, pest::error::Error<Rule>>;

pub fn parse_file<'res>(filename: &str) -> Result<Node> {
    let file_contents = fs::read_to_string(filename).expect("File not found!");
    parse(&file_contents)
}

pub fn parse(input: &str) -> Result<Node> {
    let parse_tree_top_node = VerilogAmsParser::parse(Rule::FILE, input)?.next().unwrap().into_inner().next().unwrap();
    let mut state_stack = Vec::new();
    let ast = process_children(parse_tree_top_node.into_inner(), &mut state_stack)?;
    Ok(Node {
        Type: Node_Type::DESIGN,
        children: ast,
        attributes: HashMap::new(),
    })
}

fn process(pair: Pair<Rule>, state_stack: &mut Vec<State>) -> Result<Vec<Node>> {
    match pair.as_rule() {
        Rule::MODULE => {
            let mut description = pair.into_inner();
            let attributes = process_attributes(&mut description).1;
            let name = description.next().unwrap().as_str();
            let mut Node = Node {
                Type: Node_Type::MODULE(name.to_string())
                ,
                attributes
                ,
                children: Vec::new(),
            };
            Node.children.extend(process_children(description, state_stack)?);
            Ok(vec!(Node))
        },
        Rule::PORT_DECLARATION_LIST => {
            let mut res = Vec::new();
            for current_pair in pair.into_inner() {
                if let Rule::PORT_DECLARATION = current_pair.as_rule() {
                    res.extend(process_port_declaration(current_pair.into_inner())?);
                } else {
                    unexpected_rule(current_pair);
                }
            }
            Ok(res)
        }
        Rule::CONSTANT_EXPRESSION => Ok(vec![process_constant_expression(pair)]),
        Rule::TOK_ENDMODULE => Ok(Vec::new()),
        Rule::MODULE_ITEM => process(pair.into_inner().next().unwrap(), state_stack),
        _ => {
            unexpected_rule(pair);
            unreachable!()
        },
    }
}

fn expect<'pair>(expected_value: Option<Pair<'pair, Rule>>, error_message: &str, containing_rule: pest::Span)
                 -> Result<Pair<'pair, Rule>> {
    match expected_value {
        Some(pair) => Ok(pair),
        None => error(error_message, containing_rule)
    }
}

fn error<T>(error_message: &str, containing_rule: pest::Span)
            -> Result<T> {
    Err(pest::error::Error::new_from_span(
        ErrorVariant::CustomError { message: error_message.to_string() }, containing_rule))
}

fn unexpected_rule(pair: Pair<Rule>) {
    panic!("Unexpected Rule form string {}", pair.as_str());
}

fn process_children(mut pairs: Pairs<Rule>, state_stack: &mut Vec<State>) -> Result<Vec<Node>> {
    let mut res = Vec::new();
    for pair in pairs {
        res.extend(process(pair, state_stack)?);
    }
    Ok(res)
}

fn process_port_declaration(mut pairs: Pairs<Rule>) -> Result<Vec<Node>> {
    let mut PORT_TYPE;
    let attributes;
    let mut children = Vec::new();

    {
        let processres = process_attributes(&mut pairs);
        let inout_decl_pair = processres.0.unwrap();
        let is_input;
        let is_output;
        if let inout_decl = inout_decl_pair.as_rule() {
            match inout_decl {
                Rule::TOK_IN => {
                    is_input = true;
                    is_output = false;
                },
                Rule::TOK_OUT => {
                    is_output = true;
                    is_input = false;
                }
                Rule::TOK_INOUT => {
                    is_output = true;
                    is_input = true;
                }
                _ => {
                    unexpected_rule(inout_decl_pair);
                    unreachable!()
                }
            }
        } else {
            unexpected_rule(inout_decl_pair);
            unreachable!();
        }
        PORT_TYPE = Node_Types::PORT {
            identifier: String::from(""),
            verilog_type: Node_Types::VERILOG_TYPE::UNDECLARED,
            is_input,
            is_output,
            is_signed: false,
            discipline: String::from(""),
        };
        attributes = processres.1;
    }
    for pair in pairs {
        match pair.as_rule() {
            Rule::IDENTIFIER => PORT_TYPE.discipline = process_identifier(pair),
            Rule::TOK_SIGNED => PORT_TYPE.is_signed = true,
            Rule::RANGE_DECL => children.push(process_range(pair.into_inner(), &mut Vec::new())?),
            Rule::IDENTIFIER_LIST | Rule::VARIABEL_IDENTIFIER_LIST => {
                let mut identifier_list = pair.into_inner();
                let mut res: Vec<Node> = Vec::new();
                let mut ident = identifier_list.next().unwrap().to_owned();
                loop {
                    let mut current_node = Node {
                        Type: Node_Type::MODPORT(Node_Types::PORT {
                            identifier: process_identifier(ident),
                            ..PORT_TYPE.clone()
                        }),
                        children: children.clone(),
                        attributes: attributes.clone(),
                    };

                    if let Some(next) = identifier_list.next() {
                        if let Rule::CONSTANT_EXPRESSION = next.as_rule() {
                            current_node.children.push(process_constant_expression(next));
                            if let Some(next_loop) = identifier_list.next() {
                                ident = next_loop;
                            } else {
                                break;
                            }
                        } else {
                            ident = next;
                        }
                        res.push(current_node);
                    } else {
                        res.push(current_node);

                        break;
                    }
                }
                return Ok(res);
            },
            _ => PORT_TYPE.verilog_type = process_type(pair),
        }
    }
    unreachable!()
}

fn process_identifier(matched_pair: Pair<Rule>) -> String {
    if matched_pair.as_str().starts_with("\\") {
        let raw = matched_pair.as_str();
        raw[1..raw.len() - 1].to_string()
    } else {
        matched_pair.as_str().to_string()
    }
}

fn process_range(children: Pairs<Rule>, state_stack: &mut Vec<State>) -> Result<Node> {
    let children = process_children(children, state_stack)?;
    Ok(Node { Type: Node_Type::RANGE, children, attributes: HashMap::new() })
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
        TOK_SUPPLY0 => Node_Types::VERILOG_TYPE::SUPPLY0,
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
        _ => {
            unexpected_rule(actual_pair);
            unreachable!()
        }
    }
}

fn process_attributes<'res>(pairs: &'res mut Pairs<Rule>)
                            -> (Option<Pair<'res, Rule>>, HashMap<String, Node>) {
    let mut res: HashMap<String, Node> = HashMap::new();
    for pair in pairs {
        if let Rule::ATTRIBUTE = pair.as_rule() {
            for attribute in pair.into_inner() {
                let mut description = attribute.into_inner();
                let identifier = description.next().unwrap().as_str();
                if let Some(value) = description.next() {
                    res.insert(identifier.to_string(), process_constant_expression(value));
                } else {
                    res.insert(identifier.to_string(), Node {
                        Type: Node_Type::CONSTANT_EXPRESSION
                        ,
                        attributes: HashMap::new()
                        ,
                        children: vec![Node::new(Node_Type::INTEGERVALUE(1))],
                    });
                }
            }
        } else {
            return (Some(pair), res);
        }
    }
    (None, res)
}

//TODO implement constant expression
fn process_constant_expression(pair: Pair<Rule>) -> Node {
    let node = Node::new(Node_Type::CONSTANT_EXPRESSION);
    node
}

