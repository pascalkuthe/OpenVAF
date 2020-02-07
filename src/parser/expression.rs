/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::rc::Rc;

use crate::ast::{BinaryOperator, BranchAccess, Expression, Node, Primary, UnaryOperator};
use crate::ir::ExpressionId;
use crate::parser::error::Type::UnexpectedTokens;
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::primaries::{parse_real_value, parse_unsigned_int_value, RealLiteralType};
use crate::parser::Parser;
use crate::symbol::{keywords, Ident};
use crate::util::{Push, SafeRangeCreation};

impl<'lt, 'ast, 'astref, 'source_map> Parser<'lt, 'ast, 'astref, 'source_map> {
    pub fn parse_expression(&mut self) -> Result<Node<Expression<'ast>>> {
        let mut lhs = self.parse_atom()?;
        loop {
            match self.parse_binary_operator() {
                Ok((op, precedence)) => {
                    self.lookahead.take();
                    let op_span = self.preprocessor.current_span();
                    let rhs = self.parse_atom()?;
                    let mut rhs = self.ast.push(rhs);
                    loop {
                        match self.parse_binary_operator() {
                            Ok((op, right_prec))
                                if right_prec > precedence
                                    || ((op == BinaryOperator::Condition
                                        || op == BinaryOperator::Either)
                                        && right_prec == precedence) =>
                            {
                                rhs = self.precedence_climb_expression_id(precedence, rhs)?
                            }
                            _ => break,
                        }
                    }
                    let op = Node::new(op, op_span);
                    let span = lhs.source.extend(self.ast[rhs].source);
                    lhs = Node::new(
                        Expression::BinaryOperator(self.ast.push(lhs), op, rhs),
                        span,
                    )
                }
                _ => return Ok(lhs),
            }
        }
    }
    pub fn parse_expression_id(&mut self) -> Result<ExpressionId<'ast>> {
        let lhs = self.parse_atom()?;
        let lhs = self.ast.push(lhs);
        self.precedence_climb_expression_id(0, lhs)
    }
    /// Parses Expressions using a precedance clinbing parser (see
    /// This enforces very little correctness (this is done at Schemantic Analysis) so a==x**2?y is legal here
    ///
    pub(super) fn precedence_climb_expression_id(
        &mut self,
        min_prec: u8,
        mut lhs: ExpressionId<'ast>,
    ) -> Result<ExpressionId<'ast>> {
        loop {
            match self.parse_binary_operator() {
                Ok((op, precedence)) if precedence >= min_prec => {
                    self.lookahead.take();
                    let op_span = self.preprocessor.current_span();
                    let rhs = self.parse_atom()?;
                    let mut rhs = self.ast.push(rhs);
                    loop {
                        match self.parse_binary_operator() {
                            Ok((op, right_prec))
                                if right_prec > precedence
                                    || ((op == BinaryOperator::Condition
                                        || op == BinaryOperator::Either)
                                        && right_prec == precedence) =>
                            {
                                rhs = self.precedence_climb_expression_id(precedence, rhs)?
                            }
                            _ => break,
                        }
                    }
                    let op = Node::new(op, op_span);
                    let span = self.ast[lhs].source.extend(self.ast[rhs].source);
                    lhs = self
                        .ast
                        .push(Node::new(Expression::BinaryOperator(lhs, op, rhs), span))
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
            Token::OpExp => (BinaryOperator::Exponent, 11),
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

    fn parse_atom(&mut self) -> Result<Node<Expression<'ast>>> {
        let (token, span) = self.look_ahead()?;
        let res = match token {
            Token::Minus => {
                self.lookahead.take();
                self.parse_unary_operator(UnaryOperator::ArithmeticNegate)?
            }
            Token::Plus => {
                self.lookahead.take();
                self.parse_unary_operator(UnaryOperator::ExplicitPositive)?
            }
            Token::OpLogicNot => {
                self.lookahead.take();
                self.parse_unary_operator(UnaryOperator::LogicNegate)?
            }
            Token::OpBitNot => {
                self.lookahead.take();
                self.parse_unary_operator(UnaryOperator::BitNegate)?
            }
            Token::ParenOpen => {
                self.lookahead.take();
                let res = self.parse_expression()?;
                self.expect(Token::ParenClose)?;
                res
            }
            Token::LiteralRealNumberDot => {
                self.lookahead.take();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::Dot);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberDotExp => {
                self.lookahead.take();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::DotExp);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberDotScaleChar => {
                self.lookahead.take();
                let value =
                    parse_real_value(self.preprocessor.slice(), RealLiteralType::DotScaleChar);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberExp => {
                self.lookahead.take();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::Exp);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberScaleChar => {
                self.lookahead.take();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::ScaleChar);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralUnsignedNumber => {
                self.lookahead.take();
                let value = parse_unsigned_int_value(self.preprocessor.slice());
                Node::new(Expression::Primary(Primary::UnsignedInteger(value)), span)
            }

            Token::SimpleIdentifier | Token::EscapedIdentifier => {
                let ident = self.parse_hierarchical_identifier_internal(false)?; //we allow hieraichal identifers here because they are required for functions (but illegal for natures) this will just produce an error at name resolution
                let start = self.preprocessor.current_start();
                let primary = if self.look_ahead()?.0 == Token::ParenOpen {
                    self.lookahead.take();
                    if self.look_ahead()?.0 == Token::OpLess {
                        Primary::BranchAccess(
                            Self::convert_to_nature_identifier(ident)?,
                            BranchAccess::Implicit(self.parse_branch()?),
                        )
                    } else if self.look_ahead()?.0 == Token::ParenClose {
                        self.lookahead.take();
                        Primary::FunctionCall(ident.into(), Rc::default())
                    } else {
                        let mut parameters = Rc::new(vec![self.parse_expression_id()?]);
                        {
                            let parameters = Rc::get_mut(&mut parameters).unwrap();
                            self.parse_list(
                                |sel| {
                                    parameters.push(sel.parse_expression_id()?);
                                    Ok(())
                                },
                                Token::ParenClose,
                                true,
                            )?;
                        }
                        Primary::FunctionCall(ident.into(), parameters)
                    }
                } else {
                    Primary::VariableOrNetReference(ident.into())
                };
                Node::new(
                    Expression::Primary(primary),
                    self.span_to_current_end(start),
                )
            }
            Token::Potential => {
                let start = self.preprocessor.current_start();
                let res = Primary::BranchAccess(
                    Ident::new(keywords::POTENTIAL, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }
            Token::Flow => {
                let start = self.preprocessor.current_start();
                let res = Primary::BranchAccess(
                    Ident::new(keywords::FLOW, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }
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
    fn parse_unary_operator(&mut self, unary_op: UnaryOperator) -> Result<Node<Expression<'ast>>> {
        let span = self.look_ahead()?.1;
        self.lookahead.take();
        let unary_op = Node {
            source: span,
            contents: unary_op,
        };
        let expr = self.parse_atom()?;
        let expr = self.ast.push(expr);
        let span = unary_op.source.extend(self.ast[expr].source);
        Ok(Node::new(Expression::UnaryOperator(unary_op, expr), span))
    }
}