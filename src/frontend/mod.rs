use std::collections::HashMap;
use std::fs;

use pest;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;

use ast::{Node, Node_Type};

mod ast;

#[derive(Parser)]
#[grammar = "frontend/verilog_ams.pest"]
struct VerilogAmsParser;

pub fn parse_file<'res>(filename: &str) -> Node {
    //TODO parse from file directly
    //TODO proper error handling
    let parse_tree_top_node = VerilogAmsParser::parse::<pest::inputs::FileInput>(Rule::DESIGN, input)
        .expect("Parsing failed!").next().unwrap();
    let mut top_node = Node::new(Node_Type::DESIGN);
    process_children(parse_tree_top_node.into_inner(), &mut top_node);
    top_node
}

pub fn parse(input: &str) -> Node {
    let parse_tree_top_node = VerilogAmsParser::parse(Rule::DESIGN, input)
        .expect("Parsing failed!").next().unwrap();
    let mut top_node = Node::new(Node_Type::DESIGN);
    process_children(parse_tree_top_node.into_inner(), &mut top_node);
    top_node
}

fn process(pair: Pair<Rule>) -> Result<Node, String> {
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
            process_children(description, &mut Node);
            Ok(Node)
        },
        Rule::PORT_DECLARATION_LIST => {
            for pair in pair.into_inner() {
                if let Rule::PORT_DECLARATION = pair {} else {
                    panic!()
                }
            }
        }
        _ => Err()
    }
}

fn process_children(mut pairs: Pairs<Rule>, parent: &mut Node) {
    for pair in pairs {
        parent.children.push(process(pair));
    }
}

fn process_port_decleration(mut pairs: Pairs<Rule>) {
    for pair in pairs {
        parent.children.push(process(pair));
    }
}

fn process_attributes<'res>(pairs: &'res mut Pairs<Rule>) -> (Option<Pair<'res, Rule>>, Result<HashMap<String, Node>, pest::error::Error<Rule, pest::inputs::FileInput>>) {
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

