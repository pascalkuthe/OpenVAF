/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use sr_alloc::{NodeId, SliceId};

use crate::ast::Primary::BranchAcess;
use crate::ast::{
    AstNodeId, BinaryOperator, BranchAccess, Expression, NatureAccess, Node, Primary, Reference,
    UnaryOperator,
};
use crate::parser::error::Type::UnexpectedTokens;
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::primaries::{parse_real_value, parse_unsigned_int_value, RealLiteralType};
use crate::parser::Parser;

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Node<Expression>> {
        let lhs = self.parse_atom()?;
        self.precedence_climb_expression(0, lhs)
    }
    /// Parses Expressions using a precedance clinbing parser (see
    /// This enforces very little correctness (this is done at Schemantic Analysis) so a==x**2?y is legal here
    ///
    fn precedence_climb_expression(
        &mut self,
        min_prec: u8,
        mut lhs: Node<Expression>,
    ) -> Result<Node<Expression>> {
        loop {
            match self.parse_binary_operator() {
                Ok((op, precedence)) if precedence >= min_prec => {
                    self.lookahead.take();
                    let op_span = self.preprocessor.current_span();
                    let mut rhs = self.parse_atom()?;
                    loop {
                        match self.parse_binary_operator() {
                            Ok((op, right_prec))
                                if right_prec > precedence
                                    || ((op == BinaryOperator::Condition
                                        || op == BinaryOperator::Either)
                                        && right_prec == precedence) =>
                            {
                                rhs = self.precedence_climb_expression(precedence, rhs)?
                            }
                            _ => break,
                        }
                    }
                    let op = Node::new(op, op_span);
                    let span = lhs.source.extend(rhs.source);
                    let parsed_lhs = self.ast_allocator.alloc_node(|| lhs);
                    let parsed_rhs = self.ast_allocator.alloc_node(|| rhs);
                    lhs = Node::new(Expression::BinaryOperator(parsed_lhs, op, parsed_rhs), span)
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_binary_operator(&mut self) -> Result<(BinaryOperator, u8)> {
        let (token, span) = self.look_ahead()?;
        let res = match token {
            Token::OpCondition => (BinaryOperator::Condition, 1),
            Token::Colon => (BinaryOperator::Either, 1),
            Token::OpLogicalOr => (BinaryOperator::LogicOr, 2),
            Token::OpLogicAnd => (BinaryOperator::LogicAnd, 3),
            Token::OpBitOr => (BinaryOperator::Or, 4),
            Token::OpBitXor => (BinaryOperator::Xor, 5),
            Token::OpBitNXor => (BinaryOperator::NXor, 5),
            Token::OpBitAnd => (BinaryOperator::And, 6),
            Token::OpEqual => (BinaryOperator::LogicEqual, 7),
            Token::OpNotEqual => (BinaryOperator::LogicalNotEqual, 7),
            Token::OpLess => (BinaryOperator::LessThen, 8),
            Token::OpLessEqual => (BinaryOperator::LessEqual, 8),
            Token::OpGreater => (BinaryOperator::GreaterThen, 8),
            Token::OpGreaterEqual => (BinaryOperator::GreaterEqual, 8),
            Token::OpArithmeticShiftLeft => (BinaryOperator::ShiftLeft, 9),
            Token::OpArithmeticShiftRight => (BinaryOperator::ShiftRight, 9),
            Token::Plus => (BinaryOperator::Sum, 9),
            Token::Minus => (BinaryOperator::Subtract, 9),
            Token::OpMul => (BinaryOperator::Multiply, 10),
            Token::OpDiv => (BinaryOperator::Divide, 10),
            Token::OpModulus => (BinaryOperator::Modulus, 10),
            Token::Exp => (BinaryOperator::Exponent, 11),
            _ => {
                return Err(Error {
                    error_type: UnexpectedTokens {
                        expected: vec![Expected::BinaryOperator],
                    },
                    source: span,
                })
            }
        };
        Ok(res)
    }
    /*
    parse_expression_1(lhs, min_precedence)
    lookahead := peek next token
    while lookahead is a binary operator whose precedence is >= min_precedence
        op := lookahead
        advance to next token
        rhs := parse_primary ()
        lookahead := peek next token
        while lookahead is a binary operator whose precedence is greater
                 than op's, or a right-associative operator
                 whose precedence is equal to op's
            rhs := parse_expression_1 (rhs, lookahead's precedence)
            lookahead := peek next token
        lhs := the result of applying op with operands lhs and rhs
    return lhs
    */
    fn parse_atom(&mut self) -> Result<Node<Expression>> {
        let (token, span) = self.next()?;
        let res = match token {
            Token::Minus => self.parse_unary_operator(UnaryOperator::ArithmeticNegate)?,
            Token::Plus => self.parse_unary_operator(UnaryOperator::ExplicitPositive)?,
            Token::OpLogicNot => self.parse_unary_operator(UnaryOperator::LogicNegate)?,
            Token::OpBitNot => self.parse_unary_operator(UnaryOperator::BitNegate)?,
            Token::ParenOpen => {
                let res = self.parse_expression()?;
                self.expect(Token::ParenClose)?;
                res
            }
            Token::LiteralRealNumberDot => {
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::Dot);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberDotExp => {
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::DotExp);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberDotScaleChar => {
                let value =
                    parse_real_value(self.preprocessor.slice(), RealLiteralType::DotScaleChar);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberExp => {
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::Exp);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberScaleChar => {
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::ScaleChar);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralUnsignedNumber => {
                let value = parse_unsigned_int_value(self.preprocessor.slice());
                Node::new(Expression::Primary(Primary::UnsignedInteger(value)), span)
            }
            Token::SimpleIdentifier | Token::EscapedIdentifier => {
                let ident = self.parse_hieraichal_identifier(false)?; //we allow hieraichal identifers here because they are required for functions (but illegal for natures) this will just produce an error at name resolution
                let start = self.preprocessor.current_start();
                let primary = if self.look_ahead()?.0 == Token::ParenOpen {
                    self.lookahead.take();
                    if self.look_ahead()?.0 == Token::OpLess {
                        Primary::BranchAcess(
                            NatureAccess::Unresolved(ident),
                            BranchAccess::Implicit(self.parse_branch()?),
                        )
                    } else if self.look_ahead()?.0 == Token::ParenClose {
                        self.lookahead.take();
                        Primary::FunctionCall(Reference::new(ident), SliceId::dangling())
                    } else {
                        let mut arg = vec![self.parse_expression()?];
                        self.parse_list(
                            |sel| {
                                arg.push(sel.parse_expression()?);
                                Ok(())
                            },
                            Token::ParenClose,
                            true,
                        )?;
                        Primary::FunctionCall(
                            Reference::new(ident),
                            self.ast_allocator.alloc_slice_copy(arg.as_slice()),
                        )
                    }
                } else {
                    Primary::VariableReference(Reference::new(ident)) //this is a variable because thats whats most common in expressions but it could also be a net. Schemantic anlasys will figure that out and chnage this accordingly
                };
                Node::new(
                    Expression::Primary(primary),
                    self.span_to_current_end(start),
                )
            }
            Token::Potential => {
                Primary::BranchAccess(NatureAccess::Potential, self.parse_branch_access()?)
            }
            Token::Flow => Primary::BranchAccess(NatureAccess::Flow, self.parse_branch_access()?),
            _ => {
                return Err(Error {
                    source: span,
                    error_type: UnexpectedTokens {
                        expected: vec![Expected::Primary, Expected::UnaryOperator],
                    },
                })
            }
        };
        Ok(res)
    }
    fn parse_unary_operator(&mut self, unary_op: UnaryOperator) -> Result<Node<Expression>> {
        let span = self.look_ahead()?.1;
        self.lookahead.take();
        let unary_op = Node {
            source: span,
            contents: unary_op,
        };
        let expr = self.parse_expression()?;
        let span = unary_op.source.extend(unary_op.source);
        let expr = self.ast_allocator.alloc_node(|| expr);
        Ok(Node::new(Expression::UnaryOperator(unary_op, expr), span))
    }
}
