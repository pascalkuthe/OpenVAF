/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
 *  No part of rust_adms, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */



use std::collections::HashMap;
use std::fmt::Error;
use std::fs;
use std::path::Path;

use indextree::{Arena, NodeId};
use pest::error::ErrorVariant;
use pest::iterators::{Pair, Pairs};
use pest::ParseResult;

use crate::frontend::{ast, parser};
use crate::frontend::ast::{AST, Node, Node_Types};

//Create Pest Parser from grammar
#[derive(Parser)]
#[grammar = "frontend/verilog_ams.pest"]
pub struct PestParser;


//Some convenient stuff

type ParseResult<T> = std::result::Result<T, pest::error::Error<Rule>>;
type Result = ParseResult<()>;
type ParseTreeNode<'lifetime> = Pair<'lifetime, Rule>;

//define some convention methods pair is lacking
impl ParseTreeNode {
    fn as_string(&self) -> String {
        self.as_str().to_string()
    }
}
//macro_rule! as_string{
//($parser_node) => $parser_node.as_str().to_string()
//}

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

fn unexpected_rule(pair: Pair<Rule>) {
    panic!("Unexpected Rule {:?} from string {}", pair.as_rule(), pair.as_str());
}


//Preprocessor and Parse tree generation
//TODO include
//TODO conditions
pub struct ParseTreeBuilder<'lifetime> {
    preprocessor_parse_tree_top: ParseTreeNode<'lifetime>,
    macros: HashMap<String, MACRO<'lifetime>>,
    preprocessed_source: String,
    forbidden_macros: Vec<String>,
}

struct MACRO<'lt> {
    args: Vec<&'lt ParseTreeNode<'lt>>,
    text: Option<ParseTreeNode<'lt>>,
}

impl<'lt> ParseTreeBuilder<'lt> {
    pub fn new(source_code: String) -> ParseResult<Self> {
        let preprocessor_parse_tree_top: ParseTreeNode<'lt>
            = PestParser::parse(Rule::PREPROCESSOR, source_code)?;
        Ok(ParseTreeBuilder {
            preprocessor_parse_tree_top
            ,
            macros: HashMap::new(),
            preprocessed_source: String::from("")
            ,
            forbidden_macros: Vec::new(),
        })
    }
    pub fn new_from_file(file_path: Path) -> std::result::Result<Self, pest::error::Error<Rule>> {
        let file_contents = fs::read_to_string(file).expect("File not found!");
        Self::new(file_contents)
    }
    pub fn run_preprocessor(&mut self) -> Result {
        self.process_compiler_directives(&self.preprocessor_parse_tree_top)
    }
    fn process_compiler_directives(&mut self, node: &mut ParseTreeNode<'lt>) -> Result {
        for compiler_directive_or_code in self.preprocessor_parse_tree.into_iter() {
            match compiler_directive_or_code.as_rule() {
                Rule::CODE => self.preprocessed_source.push_str(compiler_directive_or_code.as_str()),
                Rule::COMPILER_DIRECTIVE => self.process_compiler_directive(compiler_directive_or_code),
                _ => unexpected_rule(compiler_directive_or_code)
            }
        };
        OK(())
    }
    fn process_compiler_directive(&mut self, compiler_directive_or_code: ParseTreeNode<'lt>) -> Result {
        match compiler_directive_or_code.as_rule() {
            Rule::CODE => {
                self.preprocessed_source.push_str(compiler_directive_or_code.as_str());
                return Ok(());
            },
            Rule::COMPILER_DIRECTIVE => (),
            _ => {
                unexpected_rule(compiler_directive_or_code);
                unreachable!()
            }
        }
        let compiler_directive = compiler_directive_or_code.into_inner().next().unwrap();
        match compiler_directive.as_rule() {
            Rule::MACRO_DEFINITION => self.process_macro_declaration(compiler_directive),
            Rule::MACRO_REFERENCE => self.process_macro_reference(compiler_directive),
            MACRO_CONDITION => { unimplemented!() }
            _ => unexpected_rule(node)
        }
        OK(())
    }
    fn process_macro_declaration(&mut self, node: ParseTreeNode<'lt>) -> Result {
        let mut description = node.into_inner();
        description.skip(1);
        let name = identifier_string(description.next().unwrap());
        let mut args = Vec::new();
        while !description.peek().is_none() && description.peek().unwrap().as_rule() == MACRO_ARGUMENT_LIST_DEFINITION {
            args.push(&description.next().unwrap())
        }
        let text = description.next();
        //TODO warn when overriden or no effekt for compiler directives
        self.macros.insert(name, MACRO { args, text });
        Ok(())
    }
    fn process_macro_reference(&mut self, node: ParseTreeNode<'lt>) -> Result {
        let mut description: Pairs<Rule> = node.into_inner();
        description.skip(1);
        let name = identifier_string(description.next().unwrap());
        if self.forbidden_macros.contains(&name) {
            return error("MACRO " + name + " WAS CALLED FROM INSIDE ITSELF; MACRO RECURSION IS FORBIDDEN!", node.as_span())
        }
        self.forbidden_macros.push(name.clone());
        let macro_definition;
        if let Some(found) = self.macros.get(&name) {
            macro_definition = found
        } else {
            return error("Macro " + name + " not defined here!", node.as_span())
        }
        if let Some(args_list) = description.next() {
            let mut arg_bindings = HashMap::new();
            let arg_definitions = macro_definition.args.iter();

            {
                let arguments_found = args_list.clone().into_inner().count();
                let arguments_expected = macro_definition.args.len();
                if arguments_expected != arguments_found {
                    return error("Expected " + arguments_expected + " arguments for the Macro " + name + " found: " + arguments_expected, args_list.as_span());
                }
            }

            for arg in args_list.into_inner() {
                let arg_name = arg_definition.next().unwrap().as_str();
                arg_bindings.insert(arg_name, arg);
            }

            if macro_definition.text.is_none() {
                return Ok(())
            }
            for line in macro_definition.text.unwrap().into_inner() {
                for token in line.into_inner() {
                    if token.as_rule() == Rule::SIMPLE_IDENTIFIER {
                        let argument_content = arg_bindings.get(token.as_str()).unwrap();
                        self.preprocessed_source.push_str(argument_content)
                    } else {
                        self.process_compiler_directive(token)
                    }
                }
            }
        } else {
            if macro_definition.args.len() != 0 {
                return error("Expected no arguments for the Macro " + name + " found: " + macro_definition.args.len(), args_list.as_span());
            } else if let Some(macro_text) = macro_definition.text {
                self.process_compiler_directives(&macro_text)
            }
        }
        self.forbidden_macros.pop();
        OK(())
    }
    pub fn finalize(&self) -> ParseResult<ParseTreeNode> {
        PestParser::parse(Rule::VERILOG_AMS, &self.preprocessed_source)
    }
}


//TODO multithreading
//TODO finish refactor
//TODO state stack as field
pub struct ParseTreeToAstFolder<'lifetime> {
    ast: Arena<ast::Node>,
    parse_tree: Pair<'lifetime, Rule>,
}

enum State {
    ANALOG_BEHAVIOR,
    DIGITAL_BEHAVIOR,
    INITAL,
    BLOCK,
}

impl ParseTreeToAstFolder {
    pub fn new() -> Self {}

    pub fn create_from_parse_tree(parse_tree: Pair<Rule>) -> Result {
        let ast = process_children(parse_tree_top_node.into_inner(), &mut state_stack)?;
        Ok(());
    }
    fn process_parse_tree_node(parse_tree_node: Pair<Rule>, parent: NodeId) -> Result {
        match parse_tree_node.as_rule() {
            Rule::MODULE => {
                let mut description = parse_tree_node.into_inner();
                let attributes = process_attributes(&mut description).1;
                let name = description.next().unwrap().as_str();
                let mut Node = UnfinishedNode {
                    Type: Node_Type::MODULE(name.to_string())
                    ,
                    attributes
                    ,
                    children: Vec::new(),
                };
                Node.children.extend(process_children(description, state_stack)?);
                Ok(vec!(Node))
            },
            Rule::MODULE_ITEM => process(parse_tree_node.into_inner().next().unwrap(), state_stack),
            Rule::MODULE_OR_GENERATE_ITEM => process(parse_tree_node.into_inner().next().unwrap(), state_stack),
            Rule::TOK_ENDMODULE => Ok(Vec::new()),
            Rule::PORT_DECLARATION_LIST => {
                let mut res = Vec::new();
                for current_pair in parse_tree_node.into_inner() {
                    if let Rule::PORT_DECLARATION = current_pair.as_rule() {
                        res.extend(process_port_declaration(current_pair)?);
                    } else {
                        unexpected_rule(current_pair);
                    }
                }
                Ok(res)
            },
            Rule::PORT_DECLARATION => process_port_declaration(parse_tree_node),
            Rule::ANALOG => {
                state_stack.push(State::ANALOG_BEHAVIOR);
                let mut children = parse_tree_node.into_inner();
                children.next().unwrap();
                let next = children.next().unwrap();
                let mut inital = false;
                let children =
                    match next.as_rule() {
                        Rule::TOK_INITIAL => {
                            state_stack.push(State::INITAL);
                            let res = process(children.next().unwrap().into_inner().next().unwrap(), state_stack)?;
                            state_stack.pop();
                            inital = true;
                            res
                        },
                        Rule::BEHAVIORAL_STMT => process(next, state_stack)?,
                        _ => {
                            unexpected_rule(next);
                            unreachable!();
                        }
                    };
                state_stack.pop();
                Ok(vec!(UnfinishedNode {
                    Type: Node_Type::ANALOG(inital),
                    children,
                    attributes: HashMap::new(),
                }))
            },
            Rule::BEHAVIORAL_STMT => {
                let children = process_children(parse_tree_node.into_inner(), state_stack)?;
                Ok(vec!(UnfinishedNode {
                    Type: Node_Type::BLOCK,
                    children,
                    attributes: HashMap::new(),
                }))
            },
            Rule::SEQ_BLOCK => process_block(parse_tree_node, state_stack),
            Rule::CONDITONAL_STATEMENT => Ok(vec![process_if(parse_tree_node, state_stack)?]),
            Rule::CONSTANT_EXPRESSION => Ok(vec![process_constant_expression(parse_tree_node)]),
            _ => {
                unexpected_rule(parse_tree_node);
                unreachable!()
            },
        }
    }

    fn process_inner_parse_tree_nodes(&mut self, mut inner_node: Pairs<Rule>, parent: NodeId) -> Result {
        for node in inner_node {
            self.process_parse_tree_node(self, node)
        }
        Ok(())
    }

    fn process_port_declaration(mut pair: Pair<Rule>) -> Result {
        let mut pairs = pair.into_inner();
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
                    let mut res: Vec<UnfinishedNode> = Vec::new();
                    let mut ident = identifier_list.next().unwrap().to_owned();
                    loop {
                        let mut current_node = UnfinishedNode {
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


    fn process_if(matched_pair: Pair<Rule>, state_stack: &mut Vec<State>) -> Result {
        let mut pairs = matched_pair.clone().into_inner();
        pairs.next().unwrap();
        let mut children = vec![process_expression(pairs.next().unwrap())?];
        children.extend(process(pairs.next().unwrap(), state_stack)?);
        loop {
            if let Some(pair) = pairs.next() {
                if let Rule::TOK_ELSE = pair.as_rule() {
                    match pairs.next().unwrap().as_rule() {
                        TOK_IF => {
                            let mut child_children = vec![process_expression(pairs.next().unwrap())?];
                            child_children.extend(process(pairs.next().unwrap(), state_stack)?);
                            children.push(UnfinishedNode {
                                Type: Node_Type::COND
                                ,
                                children: child_children,
                                attributes: HashMap::new(),
                            });
                        },
                        Rule::BEHAVIORAL_STMT => children.extend(process(pairs.next().unwrap(), state_stack)?),
                        _ => {
                            unexpected_rule(matched_pair);
                            unreachable!()
                        }
                    }
                } else {
                    unexpected_rule(matched_pair);
                    unreachable!()
                }
            } else {
                break;
            }
        };
        Ok(UnfinishedNode {
            Type: Node_Type::COND
            ,
            children
            ,
            attributes: HashMap::new(),
        })
    }
    fn process_range(children: Pairs<Rule>, state_stack: &mut Vec<State>) -> Result {
        let children = process_children(children, state_stack)?;
        Ok(UnfinishedNode { Type: Node_Type::RANGE, children, attributes: HashMap::new() })
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
    fn process_block(matched_pair: Pair<Rule>, state_stack: &mut Vec<State>) -> Result {
        let mut pairs = matched_pair.clone().into_inner();
        state_stack.push(State::BLOCK);
        pairs.next().unwrap();
        let ident_or_behavior = pairs.next().unwrap();
        let res =
            match ident_or_behavior.as_rule() {
                Rule::BEHAVIORAL_STMT => process(ident_or_behavior, state_stack),
                Rule::IDENTIFIER => {
                    //TODO create and aquire scope from Symbol tabel

                    process_children(pairs, state_stack)
                    //TODO drop Symbol tabel scope
                }
                _ => {
                    unexpected_rule(matched_pair);
                    unreachable!();
                }
            };
        state_stack.pop();
        res
    }
    fn process_attributes_from_parse_tree(&mut self, parse_tree_nodes: &mut Pairs<Rule>, parent_ast_node: NodeId) -> (Option<Pair<Rule>>, Result) {
        for attribute_list in parse_tree_nodes {
            if let Rule::ATTRIBUTE = attribute_list.as_rule() {
                for attribute in attribute_list.into_inner() {
                    let mut description = attribute.into_inner();
                    let identifier = as_string!(description.next().unwrap());
                    let node = self.ast.new_node(Node::ATTRIBUTE(identifier));
                    if let Some(value) = description.next() {
                        self.process_constant_expression_from_parse_tree(value, node)?;
                    }
                    parent_ast_node.append(node, &mut self.ast)
                }
            } else {
                return (Some(pair), res);
            }
        }
        (None, OK(()))
    }

    //TODO implement constant expression
    //TODO make return type result
    fn process_constant_expression_from_parse_tree(&mut self, parse_tree_node: Pair<Rule>, parent_ast_node: NodeId) -> UnfinishedNode {
        unimplemented!()
    }

    //TODO implement expression
    fn process_expression(&mut self, pair: Pair<Rule>) -> Result {
        unimplemented!()
    }

    pub fn finish(self) -> AST {
        self.res
    }
}
