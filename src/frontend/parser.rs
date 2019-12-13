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
use crate::frontend::ast::Node_Types::VERILOG_TYPE;
use crate::frontend::ast::Node_Types::VERILOG_TYPE::UNDECLARED;
use crate::pest::Parser;

//Create Pest Parser from grammar
#[derive(Parser)]
#[grammar = "frontend/verilog_ams.pest"]
struct PestParser;


pub(super) fn create_parse_tree(preprocessed_source: &str) -> std::result::Result<ParseTreeNode, ()> {
    let res = PestParser::parse(Rule::VERILOG_AMS, preprocessed_source);
    if let Err(message) = res {
        error!("{}", message);
        Err(())
    } else {
        Ok(res.unwrap().next().unwrap())
    }
}
//Some convenient stuff

pub type Result<T = ()> = std::result::Result<T, pest::error::Error<Rule>>;
type ParseTreeNode<'lifetime> = Pair<'lifetime, Rule>;

fn hierarchical_identifier_string(matched_pair: Pair<Rule>) -> Vec<String> {
    let mut description = matched_pair.into_inner();
    let mut res = vec!(identifier_string(description.next().unwrap()));
    for identifier in description {
        res.push(identifier_string(identifier));
    };
    res
}

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


macro_rules! unexpected_rule {
    ( $ unexpected_pair: expr) => { panic! ("Unexpected Rule {:?} from string {}", $ unexpected_pair.as_rule(), $ unexpected_pair.as_str()) }
}
macro_rules! as_string {
($pair:expr) => {$pair.as_str().to_string()}
}

//TODO multithreading
//TODO finish refactor
//TODO state stack as field
pub struct ParseTreeToAstFolder {
    ast: Arena<ast::Node>,
    ast_top_node: NodeId,
    state_stack: Vec<State>,
}

#[derive(Clone, Debug, PartialEq)]
enum State {
    ANALOG_BEHAVIOR,
    DIGITAL_BEHAVIOR,
    INITAL,
    BLOCK,
}

impl<'lt> ParseTreeToAstFolder {
    pub fn fold(parse_tree: ParseTreeNode) -> std::result::Result<Self, ()> {
        let mut ast = Arena::new();

        let mut res = ParseTreeToAstFolder { ast_top_node: ast.new_node(ast::Node::TOP), state_stack: Vec::new(), ast };
        if let Err(e) = res.process_children(parse_tree.into_inner(), res.ast_top_node) {
            error!("{}", e);
            Err(())
        } else {
            Ok(res)
        }
    }

    fn process_parse_tree_node(&mut self, parse_tree_node: Pair<Rule>, parent_ast_node: NodeId, attributes: Vec<NodeId>) -> Result {
        match parse_tree_node.as_rule() {
            Rule::MODULE => self.process_module(parse_tree_node, parent_ast_node, attributes),

            Rule::PORT_DECLARATION => self.process_port_declaration(parse_tree_node, parent_ast_node, attributes),
            Rule::ANALOG => self.process_analog(parse_tree_node, parent_ast_node),
            Rule::SEQ_BLOCK => self.process_block(parse_tree_node, parent_ast_node),
            Rule::CONDITONAL_STATEMENT => self.process_if(parse_tree_node, parent_ast_node),
            Rule::CONTRIBUTE_STMT => self.process_contribute(parse_tree_node, parent_ast_node),
            Rule::BRANCH_DECELERATION => self.process_branch_declaration(parse_tree_node, parent_ast_node),
            Rule::VARIABLE_ASSIGNMENT => self.process_variable_assignment(parse_tree_node, parent_ast_node, attributes),
            Rule::VARIABLE_DECELERATION => self.process_variable_declaration(parse_tree_node, parent_ast_node, attributes),
            Rule::PARAMETER_DECLERATION => self.process_parameter_declaration(parse_tree_node, parent_ast_node, attributes),
            Rule::EOI => Ok(()),
            _ => unexpected_rule!(parse_tree_node),
        }
    }
    fn process_parse_tree_node_with_attributes(&mut self, mut parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> Result {
        //LISTS WITH ATTRIBUTES FOR EACH ITEM
        match parse_tree_node.as_rule() {
            Rule::PORT_DECLARATION_LIST | Rule::PORT_LIST => return self.process_list_with_attributes(parse_tree_node, parent_ast_node),
            Rule::MODULE_ITEM => {
                let next = parse_tree_node.clone().into_inner().next().unwrap();
                if next.as_rule() == Rule::MODULE_OR_GENERATE_ITEM {
                    parse_tree_node = next;
                }
            },
            Rule::MODULE_OR_GENERATE_ITEM | Rule::TOP_ITEM | Rule::BEHAVIORAL_STMT => (),
            _ => return self.process_parse_tree_node(parse_tree_node, parent_ast_node, Vec::new()),
        }

        let mut description = parse_tree_node.into_inner();
        let attributes = self.process_attributes(&mut description)?;
        if description.peek().is_some() {
            self.process_parse_tree_node(description.next().unwrap(), parent_ast_node, attributes)?;
        }
        Ok(())
    }
    //TODO switch to macro
    fn process_list_with_attributes(&mut self, parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> Result {
        let process_function =
            match parse_tree_node.as_rule() {
                Rule::PORT_DECLARATION_LIST => {
                    Self::process_port_declaration
                },
                Rule::PORT_LIST => {
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

    fn process_children(&mut self, inner_parse_tree_nodes: Pairs<Rule>, parent_node: NodeId) -> Result {
        for parse_tree_node in inner_parse_tree_nodes {
            self.process_parse_tree_node_with_attributes(parse_tree_node, parent_node)?;
        }
        Ok(())
    }

    //MODULE

    fn process_module(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId, attributes: Vec<NodeId>) -> Result {
        let mut description = parse_tree_node.into_inner();
        description.next();
        let name = identifier_string(description.next().unwrap());
        let ast_node = self.ast.new_node(ast::Node::MODULE(name));
        self.append_attributes(ast_node, &attributes);
        while description.peek().unwrap().as_rule() != Rule::TOK_ENDMODULE {
            self.process_parse_tree_node_with_attributes(description.next().unwrap(), ast_node)?
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
                self.process_parse_tree_node_with_attributes(description.next().unwrap(), node)?;
                self.state_stack.pop();
            },
            Rule::BEHAVIORAL_STMT => {
                node = self.ast.new_node(ast::Node::ANALOG(false));
                self.process_parse_tree_node_with_attributes(inital_or_behavior, node)?;
            },
            _ => unexpected_rule!(inital_or_behavior),
        };
        parent_ast_node.append(node, &mut self.ast);
        self.state_stack.pop();
        Ok(())
    }

    fn process_port_declaration(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId, attributes: Vec<NodeId>) -> Result {
        trace!("Processing port declaration from {:?}", parse_tree_node);
        let mut description = parse_tree_node.clone().into_inner();
        let mut port_info = ast::Node_Types::PORT::new();
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

        let mut range = None;
        while description.peek().is_some() && description.peek().unwrap().as_rule() != Rule::IDENTIFIER_LIST && description.peek().unwrap().as_rule() != Rule::VARIABLE_IDENTIFIER_LIST {
            let port_type_property = description.next().unwrap();
            match port_type_property.as_rule() {
                Rule::IDENTIFIER => port_info.discipline = identifier_string(port_type_property),
                Rule::TOK_SIGNED => port_info.is_signed = true,
                Rule::RANGE_DECL => {
                    if range.is_some() {
                        return error("Range cant be declared twice", parse_tree_node.as_span());
                    }
                    range = Some(self.process_range(port_type_property)?)
                },
                _ => port_info.verilog_type = Self::process_type(port_type_property),
            }
        }

        let mut identifier_list = description.next().unwrap().into_inner();
        while identifier_list.peek().is_some() {
            let identifier = identifier_string(identifier_list.next().unwrap());
            let current_node = self.ast.new_node(ast::Node::MODPORT(identifier, port_info.clone()));
            self.append_attributes(current_node, &attributes);
            if range.is_some() {
                current_node.append(range.unwrap(), &mut self.ast);
            }
            if identifier_list.peek().is_some() && identifier_list.peek().unwrap().as_rule() == Rule::CONSTANT_EXPRESSION {
                current_node.append(
                    self.process_constant_expression(identifier_list.next().unwrap())?,
                    &mut self.ast);
            }
            parent_ast_node.append(current_node, &mut self.ast);
        }
        Ok(())
    }

    fn process_branch_declaration(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> Result {
        let mut description = parse_tree_node.into_inner();
        description.next();
        let mut wire_reference_description = description.next().unwrap().into_inner();
        //TODO twice if macro instead if this nonsense
        let mut references = [None, None];
        let mut i = 0;
        while wire_reference_description.peek().is_some() {
            let reference_node = self.process_hierarchical_id(wire_reference_description.next().unwrap())?;
            if wire_reference_description.peek().is_some() && wire_reference_description.peek().unwrap().as_rule() == Rule::SINGEL_RANGE {
                self.process_single_range(wire_reference_description.next().unwrap(), reference_node)?;
            }
            references[i] = Some(reference_node);
            i = i + 1;
        }
        let is_port_branch = description.peek().unwrap().as_rule() == Rule::PORT_BRANCH_IDENTIFIER;
        let mut identifier_list = description.next().unwrap().into_inner();
        for ident in identifier_list {
            let name = identifier_string(ident);
            let ast_node = self.ast.new_node(ast::Node::BRANCH_DECL(name, is_port_branch));
            for i in references.iter() {
                if i.is_some() {
                    ast_node.append(i.unwrap(), &mut self.ast);
                }
            }
            parent_ast_node.append(ast_node, &mut self.ast);
        }
        Ok(())
    }

    fn process_parameter_declaration(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId, attributes: Vec<NodeId>) -> Result {
        let mut description = parse_tree_node.clone().into_inner();
        let local = description.next().unwrap().as_rule() == Rule::TOK_LOCALPARAM;
        let mut parameter_type = ast::Node_Types::PARAMETER { verilog_type: UNDECLARED, signed: false };
        let mut type_info_node = description.next().unwrap();
        let mut range = None;
        while type_info_node.as_rule() != Rule::PARAMETER_ASSIGNMET {
            match type_info_node.as_rule() {
                Rule::TOK_SIGNED => parameter_type.signed = true,
                Rule::RANGE => {
                    if range.is_some() {
                        return error("Range may only be declared once", parse_tree_node.as_span())
                    }
                    range = Some(self.process_range(type_info_node)?)
                },
                _ => parameter_type.verilog_type = Self::process_type(type_info_node)
            }
            type_info_node = description.next().unwrap();
        }
        for assignment in description {
            let mut assignment_description = assignment.into_inner();
            let name = identifier_string(assignment_description.next().unwrap());
            let node = self.ast.new_node(ast::Node::PARAMETER(name, local, parameter_type.clone()));
            self.append_attributes(node, &attributes);
            parent_ast_node.append(node, &mut self.ast);
            if range.is_some() {
                node.append(range.unwrap(), &mut self.ast);
            }
            let value = assignment_description.peek();
            //TODO weird expression, value range
            if value.is_some() {
                node.append(self.process_expression(value.unwrap())?, &mut self.ast)
            }
        }
        Ok(())
    }

    //TODO understand standard
    //TODO complety reimplement this
    fn process_port(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId, attributes: Vec<NodeId>) -> Result {
        let mut description = parse_tree_node.into_inner();
        if description.peek().is_none() {
            //EMPTY ports are allowed according to standard (they represent ports not connected to the interior)
            return Ok(())
        }
        if description.peek().unwrap().as_rule() == Rule::IDENTIFIER {
            unimplemented!("Weird port listing not yet supported");
        }

        description = description.next().unwrap().into_inner();
        let mut expression = description.next().unwrap().into_inner();
        if description.peek().is_some() {
            unimplemented!("Multiple references for single port not yet supported")
        }
        let identifer = identifier_string(expression.next().unwrap());
        if expression.peek().is_some() {
            unimplemented!("Port listing with range not yet supported")
        }
        let node = self.ast.new_node(ast::Node::MODPORTMEMBER(identifer));
        self.append_attributes(node, &attributes);
        parent_ast_node.append(node, &mut self.ast);
        Ok(())
    }


    fn process_variable_declaration(&mut self, parse_tree_node: ParseTreeNode, parent_ast_node: NodeId, attributes: Vec<NodeId>) -> Result {
        trace!("Processing port declaration from {:?}", parse_tree_node);
        let mut description = parse_tree_node.clone().into_inner();
        let mut variable_info = ast::Node_Types::VARIABLE {
            verilog_type: VERILOG_TYPE::UNDECLARED,
            discipline: String::from(""),
            is_signed: false,
        };
        let mut range = None;
        if description.peek().unwrap().as_rule() != Rule::IDENTIFIER {
            variable_info.verilog_type = Self::process_type(description.next().unwrap())
        };
        while description.peek().is_some() && description.peek().unwrap().as_rule() != Rule::VARIABLE_IDENTIFIER_LIST && description.peek().unwrap().as_rule() != Rule::IDENTIFIER_LIST {
            let variable_type_property = description.next().unwrap();
            match variable_type_property.as_rule() {
                Rule::IDENTIFIER => variable_info.discipline = identifier_string(variable_type_property),
                Rule::TOK_SIGNED => {
                    if variable_info.is_signed {
                        //TODO disallow for some type
                        return error("UNEXPECTED TOKEN: ", variable_type_property.as_span());
                    }
                    variable_info.is_signed = true
                },
                Rule::RANGE_DECL => {
                    if range.is_some() {
                        //TODO disallow for some type
                        return error("Range cant be declared twice", variable_type_property.as_span());
                    }
                    range = Some(self.process_range(variable_type_property)?)
                },
                Rule::DRIVE_STRENGTH => unimplemented!("Drive Strength"),
                Rule::CHARGE_STRENGTH => unimplemented!("CHARGE Strength"),
                _ => unexpected_rule!(variable_type_property),
            }
        }

        let mut identifier_list = description.next().unwrap().into_inner();
        while identifier_list.peek().is_some() {
            let identifier = identifier_string(identifier_list.next().unwrap());
            let current_node = self.ast.new_node(ast::Node::WIRE(identifier, variable_info.clone()));
            self.append_attributes(current_node, &attributes);
            if range.is_some() {
                current_node.append(range.unwrap(), &mut self.ast);
            }
            if identifier_list.peek().is_some() && identifier_list.peek().unwrap().as_rule() == Rule::CONSTANT_EXPRESSION {
                current_node.append(
                    self.process_constant_expression(identifier_list.next().unwrap())?,
                    &mut self.ast);
            }
            parent_ast_node.append(current_node, &mut self.ast);
        }
        Ok(())
    }

    //BEHAVIOR


    fn process_if(&mut self, matched_parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> Result {
        let mut description = matched_parse_tree_node.clone().into_inner();
        description.next();
        let ast_node = self.ast.new_node(ast::Node::COND);
        parent_ast_node.append(ast_node, &mut self.ast);
        let expr = self.process_expression(description.next().unwrap())?;
        ast_node.append(expr, &mut self.ast);
        self.process_parse_tree_node_with_attributes(description.next().unwrap(), ast_node)?;
        while description.peek().is_some() {
            let current_parse_tree_node = description.next().unwrap();
            if current_parse_tree_node.as_rule() == Rule::TOK_ELSE {
                match description.peek().unwrap().as_rule() {
                    Rule::TOK_IF => {
                        let expr = self.process_expression(description.next().unwrap())?;
                        ast_node.append(expr, &mut self.ast);
                        self.process_parse_tree_node_with_attributes(description.next().unwrap(), ast_node)?;
                    },
                    Rule::BEHAVIORAL_STMT => {
                        self.process_parse_tree_node_with_attributes(description.next().unwrap(), parent_ast_node)?;
                    },

                    _ => unexpected_rule!(matched_parse_tree_node),
                }
            } else {
                unexpected_rule!(matched_parse_tree_node);
            }
        };
        Ok(())
    }

    fn process_block(&mut self, matched_parse_tree_node: Pair<Rule>, parent_node: NodeId) -> Result {
        self.state_stack.push(State::BLOCK);
        let mut parse_tree_nodes_in_block = matched_parse_tree_node.into_inner();
        parse_tree_nodes_in_block.next();
        let ast_node;
        if parse_tree_nodes_in_block.peek().unwrap().as_rule() == Rule::IDENTIFIER {
            let block_name = identifier_string(parse_tree_nodes_in_block.next().unwrap());
            ast_node = self.ast.new_node(ast::Node::BLOCK(Some(block_name)));
            while parse_tree_nodes_in_block.peek().unwrap().as_rule() != Rule::BEHAVIORAL_STMT {
                self.process_parse_tree_node_with_attributes(parse_tree_nodes_in_block.next().unwrap(), ast_node)?
            }
        } else {
            ast_node = self.ast.new_node(ast::Node::BLOCK(None))
        }
        parent_node.append(ast_node, &mut self.ast);
        while parse_tree_nodes_in_block.peek().unwrap().as_rule() != Rule::TOK_END {
            self.process_parse_tree_node_with_attributes(parse_tree_nodes_in_block.next().unwrap(), ast_node)?;
        }
        self.state_stack.pop();
        Ok(())
    }

    fn process_contribute(&mut self, parse_tree_node: ParseTreeNode, parent: NodeId) -> Result {
        let mut description = parse_tree_node.into_inner();
        let ast_node = self.ast.new_node(ast::Node::CONTRIBUTE);
        parent.append(ast_node, &mut self.ast);
        let lvalue = self.process_branch_reference(description.next().unwrap())?;
        let rvalue = self.process_expression(description.next().unwrap())?;
        ast_node.append(lvalue, &mut self.ast);
        ast_node.append(rvalue, &mut self.ast);
        Ok(())
    }

    fn process_variable_assignment(&mut self, parse_tree_node: ParseTreeNode, parent: NodeId, attributes: Vec<NodeId>) -> Result {
        let mut description = parse_tree_node.into_inner();
        let mut lvalues = description.next().unwrap().into_inner();
        let value = self.process_expression(description.next().unwrap())?;
        for ident in lvalues {
            //TODO Range
            let node = self.ast.new_node(ast::Node::ASSIGN);
            let ident_node = self.ast.new_node(ast::Node::REFERENCE(hierarchical_identifier_string(ident)));
            self.append_attributes(node, &attributes);
            node.append(ident_node, &mut self.ast);
            node.append(value, &mut self.ast);
            parent.append(node, &mut self.ast);
        }
        Ok(())
    }

    //TYPE DESCRIPTIONS

    fn process_range(&mut self, matched_parse_tree_node: ParseTreeNode) -> Result<NodeId> {
        let range_node = self.ast.new_node(ast::Node::RANGE);
        let mut description = matched_parse_tree_node.into_inner();
        range_node.append(
            self.process_constant_expression(description.next().unwrap())?,
            &mut self.ast);
        range_node.append(
            self.process_constant_expression(description.next().unwrap())?,
            &mut self.ast);
        Ok(range_node)
    }

    fn process_single_range(&mut self, matched_parse_tree_node: ParseTreeNode, parent_ast_node: NodeId) -> Result {
        let range_node = self.ast.new_node(ast::Node::RANGE);
        parent_ast_node.append(range_node, &mut self.ast);
        let mut description = matched_parse_tree_node.into_inner();
        range_node.append(
            self.process_constant_expression(description.next().unwrap())?
            , &mut self.ast);
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
            Rule::TOK_REAL => Node_Types::VERILOG_TYPE::REAL,
            _ => unexpected_rule!(actual_pair),
        }
    }


    //EXPRESSIONS

    fn process_constant_expression(&mut self, parse_tree_node: Pair<Rule>) -> Result<NodeId> {
        let constant_expr = self.ast.new_node(ast::Node::CONSTANT_EXPRESSION);
        let id = self.process_expression(parse_tree_node.into_inner().next().unwrap())?;
        constant_expr.append(id, &mut self.ast);
        Ok(constant_expr)
    }

    fn process_expression(&mut self, parse_tree_node: Pair<Rule>) -> Result<NodeId> {
        let shared_self = RefCell::new(self);
        let operand_evaluation = |node: Pair<Rule>| -> Result<NodeId>{
            match node.as_rule() {
                Rule::UNARY_OPERATOR => {
                    let mut description = node.into_inner();
                    shared_self.borrow_mut().process_unary_operator(description.next().unwrap().as_rule(), description.next().unwrap())
                },
                Rule::EXPRESSION => shared_self.borrow_mut().process_expression(node),
                Rule::PRIMARY => shared_self.borrow_mut().process_primary(node.into_inner().next().unwrap()),
                _ => unexpected_rule!(node)
            }
        };
        let operator_evaluation = |lh: Result<NodeId>, op: Pair<Rule>, rh: Result<NodeId>| -> Result<NodeId>{
            shared_self.borrow_mut().process_operator(lh, op, rh)
        };
        let mut operator_precedence: PrecClimber<Rule> = PrecClimber::new(vec![
            //OTHER
            Operator::new(Rule::OP_CONCAT, Assoc::Left)
                | Operator::new(Rule::OP_REPLICATION, Assoc::Left),
            //CONDITIONAL
            Operator::new(Rule::OP_COND, Assoc::Right),
            //LOGICAL OR
            Operator::new(Rule::OP_LOGIC_OR, Assoc::Left),
            //LOGICAL AND
            Operator::new(Rule::OP_LOGIC_AND, Assoc::Left),
            //BITWISE OR
            Operator::new(Rule::OP_OR, Assoc::Left),
            //BITWISE XOR NXOR
            Operator::new(Rule::OP_XOR, Assoc::Left)
                | Operator::new(Rule::OP_NXOR, Assoc::Left),
            //BITWISE AND
            Operator::new(Rule::OP_AND, Assoc::Left),
            //EQUAL COMPARISON
            Operator::new(Rule::OP_EQ, Assoc::Left)
                | Operator::new(Rule::OP_NE, Assoc::Left)
                | Operator::new(Rule::OP_CASE_EQ, Assoc::Left)
                | Operator::new(Rule::OP_CASE_NE, Assoc::Left),
            //GREATER/LESS COMPARISON
            Operator::new(Rule::OP_GE, Assoc::Left)
                | Operator::new(Rule::OP_LE, Assoc::Left)
                | Operator::new(Rule::OP_LT, Assoc::Left)
                | Operator::new(Rule::OP_GT, Assoc::Left),
            //SHIFT
            Operator::new(Rule::OP_LOGIC_LEFT, Assoc::Left)
                | Operator::new(Rule::OP_LOGIC_RIGHT, Assoc::Left)
                | Operator::new(Rule::OP_ARITHMETIC_LEFT, Assoc::Left)
                | Operator::new(Rule::OP_ARITHMETIC_RIGHT, Assoc::Left),
            //DASH ARITHMETIC
            Operator::new(Rule::OP_PLUS, Assoc::Left)
                | Operator::new(Rule::OP_MINUS, Assoc::Left),
            //DOT ARITHMETIC
            Operator::new(Rule::OP_DIV, Assoc::Left)
                | Operator::new(Rule::OP_MUL, Assoc::Left)
                | Operator::new(Rule::OP_MOD, Assoc::Left)
                | Operator::new(Rule::OP_DIV, Assoc::Left),

            //BINARY
            Operator::new(Rule::OP_POT, Assoc::Left)
        ]);
        let res = operator_precedence.climb(parse_tree_node.into_inner(), operand_evaluation, operator_evaluation);
        res
    }

    fn process_operator(&mut self, lh: Result<NodeId>, op: Pair<Rule>, rh: Result<NodeId>) -> Result<NodeId> {
        let node =
            match op.as_rule() {
                Rule::OP_PLUS => self.ast.new_node(ast::Node::ADD),
                Rule::OP_MINUS => self.ast.new_node(ast::Node::SUB),
                Rule::OP_MUL => self.ast.new_node(ast::Node::MUL),
                Rule::OP_DIV => self.ast.new_node(ast::Node::DIV),
                Rule::OP_MOD => self.ast.new_node(ast::Node::MOD),

                Rule::OP_XOR => self.ast.new_node(ast::Node::BIT_XOR),
                Rule::OP_NXOR => self.ast.new_node(ast::Node::BIT_EQ),
                Rule::OP_OR => self.ast.new_node(ast::Node::BIT_OR),
                Rule::OP_AND => self.ast.new_node(ast::Node::BIT_AND),

                Rule::OP_GE => self.ast.new_node(ast::Node::GE),
                Rule::OP_GT => self.ast.new_node(ast::Node::GT),
                Rule::OP_LE => self.ast.new_node(ast::Node::LE),
                Rule::OP_LT => self.ast.new_node(ast::Node::LT),

                Rule::OP_NE => self.ast.new_node(ast::Node::NE),
                Rule::OP_EQ => self.ast.new_node(ast::Node::EQ),

                Rule::OP_LOGIC_AND => self.ast.new_node(ast::Node::LOGIC_AND),
                Rule::OP_LOGIC_OR => self.ast.new_node(ast::Node::LOGIC_OR),

                Rule::OP_LOGIC_LEFT => self.ast.new_node(ast::Node::SHIFT_LEFT),
                Rule::OP_LOGIC_RIGHT => self.ast.new_node(ast::Node::SHIFT_RIGHT),
                Rule::OP_ARITHMETIC_LEFT => self.ast.new_node(ast::Node::SHIFT_SLEFT),
                Rule::OP_ARITHMETIC_RIGHT => self.ast.new_node(ast::Node::SHIFT_SRIGHT),

                Rule::OP_COND => {
                    let node = self.ast.new_node(ast::Node::COND);
                    node.append(lh?, &mut self.ast);
                    let inner_expression = op.into_inner().next();
                    if inner_expression.is_some() {
                        node.append(self.process_expression(inner_expression.unwrap())?, &mut self.ast);
                    }
                    node.append(rh?, &mut self.ast);
                    return Ok(node);
                },
                _ => unexpected_rule!(op)
            };
        node.append(lh?, &mut self.ast);
        node.append(rh?, &mut self.ast);
        Ok(node)
    }

    fn process_unary_operator(&mut self, operator: Rule, value: Pair<Rule>) -> Result<NodeId> {
        let child = match value.as_rule() {
            Rule::PRIMARY => self.process_primary(value.into_inner().next().unwrap())?,
            Rule::EXPRESSION => self.process_expression(value)?,
            _ => unexpected_rule!(value)
        };
        let op_node =
            match operator {
                Rule::OP_MINUS => self.ast.new_node(ast::Node::NEG),
                Rule::OP_PLUS => return Ok(child), //PLUS OPERATOR CAN BE IGNORED BUT IS PART OF THE SPEC SMH
                Rule::OP_BIT_NOT => self.ast.new_node(ast::Node::BIT_NOT),
                Rule::OP_NOT => self.ast.new_node(ast::Node::LOGIC_NOT),
                Rule::OP_XOR => self.ast.new_node(ast::Node::REDUCE_XOR),
                Rule::OP_NXOR => self.ast.new_node(ast::Node::REDUCE_XNOR),
                Rule::OP_OR => self.ast.new_node(ast::Node::REDUCE_OR),
                Rule::OP_AND => self.ast.new_node(ast::Node::REDUCE_AND),
                _ => unimplemented!(),
            };
        op_node.append(child, &mut self.ast);
        Ok(op_node)
    }

    fn process_primary(&mut self, value: ParseTreeNode) -> Result<NodeId> {
        match value.as_rule() {
            Rule::HIERARCHICAL_ID => self.process_hierarchical_id(value),
            Rule::UNSIGNED_NUMBER => Ok(self.ast.new_node(
                ast::Node::INTEGER_VALUE(value.as_str().parse::<i64>().unwrap()))),
            Rule::REAL_NUMBER => self.process_real_value(value),
            Rule::BRANCH_RVALUE => self.process_branch_reference(value),
            Rule::SYSTEM_CALL => self.process_function_call(value, true),
            Rule::FUNCTION_CALL => self.process_function_call(value, false),
            Rule::STRING => self.process_string(value),
            _ => unexpected_rule!(value)
        }
    }

    fn process_hierarchical_id(&mut self, value: ParseTreeNode) -> Result<NodeId> {
        let ident = hierarchical_identifier_string(value);
        Ok(self.ast.new_node(ast::Node::REFERENCE(ident)))
    }

    fn process_branch_reference(&mut self, value: ParseTreeNode) -> Result<NodeId> {
        if !self.state_stack.contains(&State::ANALOG_BEHAVIOR) {
            return error("Branch acess is only allowed in analog behavior!", value.as_span());
        }
        let mut description = value.into_inner();
        let nature_node = description.next().unwrap().into_inner().next().unwrap();
        let nature =
            match nature_node.as_rule() {
                Rule::TOK_POTENTIAL => ast::Node_Types::NATURE_ACCESS::POTENTIAL,
                Rule::TOK_FLOW => ast::Node_Types::NATURE_ACCESS::FLOW,
                Rule::IDENTIFIER => ast::Node_Types::NATURE_ACCESS::UNRESOLVED(identifier_string(nature_node)),
                _ => unexpected_rule!(nature_node)
            };
        let port_branch = description.peek().unwrap().as_rule() == Rule::PORT_BRANCH_IDENTIFIER;
        let ast_node = self.ast.new_node(ast::Node::BRANCH_REF(nature, port_branch));
        description = description.next().unwrap().into_inner();
        while description.peek().is_some() {
            let reference_node = self.process_hierarchical_id(description.next().unwrap())?;
            ast_node.append(reference_node, &mut self.ast);
            if description.peek().is_some() && description.peek().unwrap().as_rule() == Rule::SINGEL_RANGE {
                self.process_single_range(description.next().unwrap(), reference_node)?
            }
        }
        Ok(ast_node)
    }

    fn process_function_call(&mut self, value: ParseTreeNode, system_function: bool) -> Result<NodeId> {
        let mut description = value.into_inner();
        let name = if system_function {
            vec!(identifier_string(description.next().unwrap()))
        } else {
            if description.peek().unwrap().as_rule() == Rule::STANDARD_FUNCTIONS {
                vec!(as_string!(description.next().unwrap()))
            } else {
                hierarchical_identifier_string(description.next().unwrap())
            }
        };
        let node = self.ast.new_node(ast::Node::FCALL(name, system_function));
        for arg in description {
            let arg_node = self.process_expression(arg)?;
            node.append(arg_node, &mut self.ast);
        }
        Ok(node)
    }

    fn process_real_value(&mut self, value: ParseTreeNode) -> Result<NodeId> {
        let mut description = value.into_inner();
        let mut number_as_string = as_string!(description.next().unwrap());
        if description.peek().unwrap().as_rule() == Rule::UNSIGNED_NUMBER {
            number_as_string = format!("{}.{}", number_as_string, description.next().unwrap().as_str());
        }
        let mut real_value: f64 = number_as_string.parse::<f64>().unwrap();
        if description.peek().is_some() {
            let scientific_factor =
                if description.peek().unwrap().as_rule() == Rule::EXP {
                    description.next();
                    let mut scientific_factor_str = as_string!(description.next().unwrap());
                    if description.peek().is_some() {
                        scientific_factor_str.push_str(&as_string!(description.next().unwrap()));
                    }
                    scientific_factor_str.parse::<i32>().unwrap()
                } else {
                    let scale_factor = description.next().unwrap();
                    match scale_factor.as_str() {
                        "T" => 12,
                        "G" => 9,
                        "M" => 6,
                        "K" | "k" => 3,
                        "m" => -3,
                        "u" => -6,
                        "p" => -9,
                        "f" => -12,
                        "a" => -15,
                        _ => unexpected_rule!(scale_factor)
                    }
                };
            real_value = real_value * (10f64).powi(scientific_factor);
        }
        Ok(self.ast.new_node(ast::Node::REAL_VALUE(real_value)))
    }

    fn process_string(&mut self, value: ParseTreeNode) -> Result<NodeId> {
        //TODO weird octal numbers
        let mut string = as_string!(value.into_inner().next().unwrap());
        string = string.replace("\\n", "\n").replace("\\t", "\t").replace("\\\\", "\\").replace("\\\"", "\"");
        Ok(self.ast.new_node(ast::Node::STRINGVALUE(string)))
    }

    fn process_attributes(&mut self, parse_tree_nodes: &mut Pairs<Rule>) -> Result<Vec<NodeId>> {
        let node_iterator = parse_tree_nodes.into_iter();
        let mut attributes: HashMap<String, Option<ParseTreeNode>> = HashMap::new();
        let mut attribute_list;
        while node_iterator.peek().is_some() && node_iterator.peek().unwrap().as_rule() == Rule::ATTRIBUTE {
            attribute_list = node_iterator.next().unwrap();
            for attribute in attribute_list.into_inner() {
                let mut description = attribute.into_inner();
                let identifier = as_string!(description.next().unwrap());
                //Overwrite if attribute already declared
                if attributes.insert(identifier, description.next()).is_some() {
                    //TODO warn when attribute is overwritten
                }
            }
        }
        let res = Vec::new();
        for attribute in attributes {
            let node = self.ast.new_node(ast::Node::ATTRIBUTE(attribute.0));
            let value =
                if attribute.1.is_some() {
                    self.process_constant_expression(attribute.1.unwrap())?
                } else {
                    self.ast.new_node(ast::Node::INTEGER_VALUE(1))
                };
            node.append(value, &mut self.ast)
        }
        Ok(res)
    }

    fn append_attributes(&mut self, parent: NodeId, attributes: &Vec<NodeId>) {
        for attribute in attributes {
            parent.append(*attribute, &mut self.ast);
        }
    }

    pub fn finish(self) -> (Arena<ast::Node>, NodeId) {
        (self.ast, self.ast_top_node)
    }
}
