//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::cell::RefCell;

use crate::parsing::syntax::ParseTreeToRawAstFolder;

use super::*;
use crate::ast::Node;

impl<'lt> ParseTreeToRawAstFolder<'lt> {
    pub(super) fn process_constant_expression(
        &mut self,
        parse_tree_node: ParseTreeNode<'lt>,
    ) -> SyntaxResult<NodeId> {
        let constant_expr = self.ast.arena.new_node(ast::RawNode {
            node_info: Node::ConstantExpression,
            src: parse_tree_node.as_span(),
        });
        let id = self.process_expression(parse_tree_node.into_inner().next().unwrap())?;
        constant_expr.append(id, &mut self.ast.arena);
        Ok(constant_expr)
    }

    pub(super) fn process_expression(
        &mut self,
        parse_tree_node: ParseTreeNode<'lt>,
    ) -> SyntaxResult<NodeId> {
        trace!("Processing expression from {:?}", parse_tree_node);
        let shared_self = RefCell::new(self);
        let operand_evaluation = |node: ParseTreeNode<'lt>| -> SyntaxResult<NodeId> {
            match node.as_rule() {
                Rule::UNARY_OPERATOR => {
                    let mut description = node.into_inner();
                    shared_self.borrow_mut().process_unary_operator(
                        description.next().unwrap().as_rule(),
                        description.next().unwrap(),
                    )
                }
                Rule::EXPRESSION => shared_self.borrow_mut().process_expression(node),
                Rule::PRIMARY => shared_self
                    .borrow_mut()
                    .process_primary(node.into_inner().next().unwrap()),
                _ => unexpected_rule!(node),
            }
        };
        let operator_evaluation = |lh: SyntaxResult<NodeId>,
                                   op: ParseTreeNode<'lt>,
                                   rh: SyntaxResult<NodeId>|
         -> SyntaxResult<NodeId> {
            shared_self.borrow_mut().process_operator(lh?, op, rh?)
        };
        let operator_precedence: PrecClimber<Rule> = PrecClimber::new(vec![
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
            Operator::new(Rule::OP_XOR, Assoc::Left) | Operator::new(Rule::OP_NXOR, Assoc::Left),
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
            Operator::new(Rule::OP_PLUS, Assoc::Left) | Operator::new(Rule::OP_MINUS, Assoc::Left),
            //DOT ARITHMETIC
            Operator::new(Rule::OP_DIV, Assoc::Left)
                | Operator::new(Rule::OP_MUL, Assoc::Left)
                | Operator::new(Rule::OP_MOD, Assoc::Left)
                | Operator::new(Rule::OP_DIV, Assoc::Left),
            //BINARY
            Operator::new(Rule::OP_POT, Assoc::Left),
        ]);
        operator_precedence.climb(
            parse_tree_node.into_inner(),
            operand_evaluation,
            operator_evaluation,
        )
    }

    fn process_operator(
        &mut self,
        lh: NodeId,
        op: ParseTreeNode<'lt>,
        rh: NodeId,
    ) -> SyntaxResult<NodeId> {
        let node = if op.as_rule() == Rule::OP_COND {
            let node = self.ast.arena.new_node(ast::RawNode {
                node_info: Node::Cond,
                src: op.as_span(),
            });
            node.append(lh, &mut self.ast.arena);
            if let Some(inner_expression) = op.into_inner().next() {
                node.append(
                    self.process_expression(inner_expression)?,
                    &mut self.ast.arena,
                );
            }
            node.append(rh, &mut self.ast.arena);
            return Ok(node);
        } else {
            let node_info = match op.as_rule() {
                Rule::OP_PLUS => Node::ADD,
                Rule::OP_MINUS => Node::SUB,
                Rule::OP_MUL => Node::MUL,
                Rule::OP_DIV => Node::DIV,
                Rule::OP_MOD => Node::MOD,

                Rule::OP_XOR => Node::BitXor,
                Rule::OP_NXOR => Node::BitEq,
                Rule::OP_OR => Node::BitOr,
                Rule::OP_AND => Node::BitAnd,

                Rule::OP_GE => Node::GE,
                Rule::OP_GT => Node::GT,
                Rule::OP_LE => Node::LE,
                Rule::OP_LT => Node::LT,

                Rule::OP_NE => Node::NE,
                Rule::OP_EQ => Node::EQ,

                Rule::OP_LOGIC_AND => Node::LogicAnd,
                Rule::OP_LOGIC_OR => Node::LogicOr,

                Rule::OP_LOGIC_LEFT => Node::ShiftLeft,
                Rule::OP_LOGIC_RIGHT => Node::ShiftRight,
                Rule::OP_ARITHMETIC_LEFT => Node::ShiftSleft,
                Rule::OP_ARITHMETIC_RIGHT => Node::ShiftSright,
                _ => unexpected_rule!(op),
            };
            self.ast.arena.new_node(ast::RawNode {
                src: op.as_span(),
                node_info,
            })
        };
        node.append(lh, &mut self.ast.arena);
        node.append(rh, &mut self.ast.arena);
        Ok(node)
    }

    fn process_unary_operator(
        &mut self,
        operator: Rule,
        value: ParseTreeNode<'lt>,
    ) -> SyntaxResult<NodeId> {
        let span = value.as_span();
        let child = match value.as_rule() {
            Rule::PRIMARY => self.process_primary(value.into_inner().next().unwrap())?,
            Rule::EXPRESSION => self.process_expression(value)?,
            _ => unexpected_rule!(value),
        };
        let op_node_type = match operator {
            Rule::OP_MINUS => Node::NEG,
            Rule::OP_PLUS => return Ok(child), //PLUS OPERATOR CAN BE IGNORED BUT IS PART OF THE SPEC SMH
            Rule::OP_BIT_NOT => Node::BitNot,
            Rule::OP_NOT => Node::LogicNot,
            Rule::OP_XOR => Node::ReduceXor,
            Rule::OP_NXOR => Node::ReduceXnor,
            Rule::OP_OR => Node::ReduceOr,
            Rule::OP_AND => Node::ReduceAnd,
            _ => unimplemented!(),
        };
        let op_node = self.ast.arena.new_node(ast::RawNode {
            src: span,
            node_info: op_node_type,
        });
        op_node.append(child, &mut self.ast.arena);
        Ok(op_node)
    }

    fn process_primary(&mut self, value: ParseTreeNode<'lt>) -> SyntaxResult<NodeId> {
        match value.as_rule() {
            Rule::HIERARCHICAL_ID => self.process_hierarchical_id(value),
            Rule::UNSIGNED_NUMBER => Ok(self.ast.arena.new_node(ast::RawNode {
                src: value.as_span(),
                node_info: Node::IntegerValue(value.as_str().parse::<i64>().unwrap()),
            })),
            Rule::REAL_NUMBER => self.process_real_value(value),
            Rule::SYSTEM_CALL => self.process_function_call(value, true),
            Rule::FUNCTION_CALL => self.process_function_call(value, false),
            Rule::STRING => self.process_string(value),
            _ => unexpected_rule!(value),
        }
    }

    pub(super) fn process_hierarchical_id(
        &mut self,
        value: ParseTreeNode<'lt>,
    ) -> SyntaxResult<NodeId> {
        let span = value.as_span();
        let ident = hierarchical_identifier_string(value);
        Ok(self.ast.arena.new_node(ast::RawNode {
            src: span,
            node_info: Node::Reference(ident),
        }))
    }

    fn process_real_value(&mut self, value: ParseTreeNode<'lt>) -> SyntaxResult<NodeId> {
        let span = value.as_span();
        let mut description = value.into_inner();
        let mut number_as_string = as_string!(description.next().unwrap());
        if description.peek().unwrap().as_rule() == Rule::UNSIGNED_NUMBER {
            number_as_string = format!(
                "{}.{}",
                number_as_string,
                description.next().unwrap().as_str()
            );
        }
        let mut real_value: f64 = number_as_string.parse::<f64>().unwrap();
        if let Some(first_factor_node) = description.next() {
            let scientific_factor = if first_factor_node.as_rule() == Rule::EXP {
                let mut scientific_factor_str = as_string!(description.next().unwrap());
                if let Some(number) = description.next() {
                    scientific_factor_str.push_str(&as_string!(number));
                }
                scientific_factor_str.parse::<i32>().unwrap()
            } else {
                match first_factor_node.as_str() {
                    "T" => 12,
                    "G" => 9,
                    "M" => 6,
                    "K" | "k" => 3,
                    "m" => -3,
                    "u" => -6,
                    "p" => -9,
                    "f" => -12,
                    "a" => -15,
                    _ => unexpected_rule!(first_factor_node),
                }
            };
            real_value *= (10_f64).powi(scientific_factor);
        }
        Ok(self.ast.arena.new_node(ast::RawNode {
            src: span,
            node_info: Node::RealValue(real_value),
        }))
    }

    pub(super) fn process_optional_constant_expression(
        &mut self,
        description: &mut Pairs<'lt, Rule>,
    ) -> SyntaxResult<Option<NodeId>> {
        if_rule!(let Some(expr) = description.next() where Rule::CONSTANT_EXPRESSION => {
                return Ok(Some(self.process_constant_expression(expr)?));
        });
        Ok(None)
    }

    fn process_string(&mut self, value: ParseTreeNode<'lt>) -> SyntaxResult<NodeId> {
        let span = value.as_span();
        //TODO weird octal numbers
        let mut string = as_string!(value.into_inner().next().unwrap());
        string = string
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\\\", "\\")
            .replace("\\\"", "\"");
        Ok(self.ast.arena.new_node(ast::RawNode {
            src: span,
            node_info: Node::StringValue(string),
        }))
    }
}
