/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{BinaryOperator, BranchAccess, Expression, Primary, UnaryOperator};
use crate::ir::BuiltInFunctionCall1p::*;
use crate::ir::BuiltInFunctionCall2p::*;
use crate::ir::{BuiltInFunctionCall1p, BuiltInFunctionCall2p, Push};
use crate::ir::{ExpressionId, Node};
use crate::parser::error::Type::{UnexpectedEof, UnexpectedTokens};
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::primaries::{
    parse_real_value, parse_string, parse_unsigned_int_value, RealLiteralType,
};
use crate::parser::Parser;
use crate::symbol::{keywords, Ident};
use crate::Span;

enum BinaryOperatorOrCondition {
    Condition,
    BinaryOperator(BinaryOperator, u8),
}
impl<'lt, 'ast, 'source_map> Parser<'lt, 'ast, 'source_map> {
    pub fn parse_expression(&mut self) -> Result<Node<Expression<'ast>>> {
        let mut lhs = self.parse_atom()?;
        loop {
            match self.parse_binary_operator() {
                Ok(BinaryOperatorOrCondition::BinaryOperator(op, precedence)) => {
                    self.consume_lookahead();
                    let op_span = self.preprocessor.span();
                    let rhs = self.parse_atom()?;
                    let mut rhs = self.ast.push(rhs);
                    loop {
                        match self.parse_binary_operator() {
                            Ok(BinaryOperatorOrCondition::BinaryOperator(_, right_prec))
                                if right_prec > precedence =>
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
                Ok(BinaryOperatorOrCondition::Condition) => {
                    self.consume_lookahead();
                    let condition = self.ast.push(lhs);
                    let op_span = self.preprocessor.span();
                    let if_val = self.parse_expression_id()?;
                    self.expect(Token::Colon)?;
                    let eiter_op_span = self.preprocessor.span();
                    let else_val = self.parse_expression_id()?;
                    lhs = Node::new(
                        Expression::Condtion(condition, op_span, if_val, eiter_op_span, else_val),
                        self.ast[condition].source.extend(self.ast[else_val].source),
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

    pub(super) fn precedence_climb_expression_id(
        &mut self,
        min_prec: u8,
        mut lhs: ExpressionId<'ast>,
    ) -> Result<ExpressionId<'ast>> {
        loop {
            match self.parse_binary_operator() {
                Ok(BinaryOperatorOrCondition::Condition) if min_prec <= 1 => {
                    self.consume_lookahead();
                    let op_span = self.preprocessor.span();
                    let if_val = self.parse_expression_id()?;
                    self.expect(Token::Colon)?;
                    let eiter_op_span = self.preprocessor.span();
                    let else_val = self.parse_expression_id()?;
                    lhs = self.ast.push(Node::new(
                        Expression::Condtion(lhs, op_span, if_val, eiter_op_span, else_val),
                        self.ast[lhs].source.extend(self.ast[else_val].source),
                    ))
                }
                Ok(BinaryOperatorOrCondition::BinaryOperator(op, precedence))
                    if precedence >= min_prec =>
                {
                    self.consume_lookahead();
                    let op_span = self.preprocessor.span();
                    let rhs = self.parse_atom()?;
                    let mut rhs = self.ast.push(rhs);
                    loop {
                        match self.parse_binary_operator() {
                            Ok(BinaryOperatorOrCondition::BinaryOperator(_, right_prec))
                                if right_prec > precedence =>
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

    fn parse_binary_operator(&mut self) -> Result<BinaryOperatorOrCondition> {
        let (token, span) = self.look_ahead()?;
        let res = match token {
            Token::OpCondition => return Ok(BinaryOperatorOrCondition::Condition),
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
        Ok(BinaryOperatorOrCondition::BinaryOperator(res.0, res.1))
    }

    pub(super) fn parse_atom(&mut self) -> Result<Node<Expression<'ast>>> {
        let (token, span) = self.look_ahead()?;
        let res = match token {
            Token::Minus => {
                self.consume_lookahead();
                self.parse_unary_operator(UnaryOperator::ArithmeticNegate)?
            }
            Token::Plus => {
                self.consume_lookahead();
                self.parse_unary_operator(UnaryOperator::ExplicitPositive)?
            }
            Token::OpLogicNot => {
                self.consume_lookahead();
                self.parse_unary_operator(UnaryOperator::LogicNegate)?
            }
            Token::OpBitNot => {
                self.consume_lookahead();
                self.parse_unary_operator(UnaryOperator::BitNegate)?
            }
            Token::ParenOpen => self.parse_bracketed_expression()?,
            Token::LiteralString => {
                self.consume_lookahead();
                let val = self
                    .ast
                    .string_literals
                    .try_add_small(&parse_string(self.preprocessor.slice()))
                    .map_err(|_| Error {
                        error_type: Type::StringTooLong(span.get_len() as usize - 2),
                        source: span,
                    })?;
                Node::new(Expression::Primary(Primary::String(val)), span)
            }
            Token::LiteralRealNumberDot => {
                self.consume_lookahead();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::Dot);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberDotExp => {
                self.consume_lookahead();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::DotExp);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberDotScaleChar => {
                self.consume_lookahead();
                let value =
                    parse_real_value(self.preprocessor.slice(), RealLiteralType::DotScaleChar);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberExp => {
                self.consume_lookahead();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::Exp);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralRealNumberScaleChar => {
                self.consume_lookahead();
                let value = parse_real_value(self.preprocessor.slice(), RealLiteralType::ScaleChar);
                Node::new(Expression::Primary(Primary::Real(value)), span)
            }
            Token::LiteralUnsignedNumber => {
                self.consume_lookahead();
                let value = parse_unsigned_int_value(self.preprocessor.slice());
                Node::new(Expression::Primary(Primary::UnsignedInteger(value)), span)
            }

            Token::SimpleIdentifier(_) | Token::EscapedIdentifier => {
                //we allow hieraichal identifers here because they are required for functions
                // They are illegal for natures so this will just produce an error at name resolution

                let ident = self.parse_hierarchical_identifier()?;
                let start = self.preprocessor.current_start();
                let primary = if self.look_ahead()?.0 == Token::ParenOpen {
                    self.consume_lookahead();
                    if self.look_ahead()?.0 == Token::OpLess {
                        let start = self.preprocessor.current_start();
                        let res = Primary::BranchAccess(
                            Self::convert_to_nature_identifier(ident.names)?,
                            Node::new(
                                BranchAccess::Implicit(self.parse_branch()?),
                                self.span_to_current_end(start),
                            ),
                        );
                        self.expect(Token::ParenClose)?;
                        res
                    } else if self.look_ahead()?.0 == Token::ParenClose {
                        self.consume_lookahead();
                        Primary::FunctionCall(ident, Vec::new())
                    } else {
                        let mut parameters = vec![self.parse_expression_id()?];
                        self.parse_list_tail(
                            |sel| {
                                parameters.push(sel.parse_expression_id()?);
                                Ok(())
                            },
                            Token::ParenClose,
                            true,
                        )?;
                        Primary::FunctionCall(ident, parameters)
                    }
                } else {
                    Primary::VariableOrNetReference(ident)
                };
                Node::new(
                    Expression::Primary(primary),
                    self.span_to_current_end(start),
                )
            }
            Token::Potential => {
                let start = self.preprocessor.current_start();
                self.consume_lookahead();
                let res = Primary::BranchAccess(
                    Ident::new(keywords::POTENTIAL, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }

            Token::Flow => {
                let start = self.preprocessor.current_start();
                self.consume_lookahead();
                let res = Primary::BranchAccess(
                    Ident::new(keywords::FLOW, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }

            Token::PartialDerivative => {
                let start = self.preprocessor.current_start();
                self.consume_lookahead();
                self.expect(Token::ParenOpen)?;
                let expr_to_derive = self.parse_expression_id()?;

                self.expect(Token::Comma)?;

                let (token, span) = self.next()?;
                let disciplines_access = match token {
                    Token::Flow => Ident::new(keywords::FLOW, span),
                    Token::Potential => Ident::new(keywords::POTENTIAL, span),
                    Token::SimpleIdentifier(_) => {
                        Ident::from_str_and_span(self.preprocessor.slice(), span)
                    }
                    Token::EscapedIdentifier => Ident::from_str_and_span(
                        &self.preprocessor.slice()[1..self.preprocessor.slice().len() - 1],
                        span,
                    ),
                    _ => {
                        return Err(Error {
                            error_type: Type::UnexpectedTokens {
                                expected: vec![Expected::BranchAcess],
                            },
                            source: span,
                        })
                    }
                };
                let branch_access = self.parse_branch_access()?;
                self.expect(Token::ParenClose)?;
                Node::new(
                    Expression::Primary(Primary::DerivativeByBranch(
                        expr_to_derive,
                        disciplines_access,
                        branch_access,
                    )),
                    self.span_to_current_end(start),
                )
            }

            Token::SystemCall => {
                self.consume_lookahead();
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(Ident::from_str_and_span(
                        self.preprocessor.slice(),
                        span,
                    ))),
                    span,
                )
            }

            Token::Min => self.parse_double_parameter_built_in_function_call(Min)?,
            Token::Max => self.parse_double_parameter_built_in_function_call(Max)?,

            Token::Pow => self.parse_double_parameter_built_in_function_call(Pow)?,
            Token::Hypot => self.parse_double_parameter_built_in_function_call(Hypot)?,
            Token::Sqrt => self.parse_single_parameter_built_in_function_call(Sqrt)?,

            Token::Exp => self.parse_single_parameter_built_in_function_call(Exp)?,
            Token::Ln => self.parse_single_parameter_built_in_function_call(Ln)?,
            Token::Log => self.parse_single_parameter_built_in_function_call(Log)?,

            Token::Abs => self.parse_single_parameter_built_in_function_call(Abs)?,
            Token::Ceil => self.parse_single_parameter_built_in_function_call(Ceil)?,
            Token::Floor => self.parse_single_parameter_built_in_function_call(Floor)?,

            Token::Tan => self.parse_single_parameter_built_in_function_call(Tan)?,
            Token::Sin => self.parse_single_parameter_built_in_function_call(Sin)?,
            Token::Cos => self.parse_single_parameter_built_in_function_call(Cos)?,

            Token::ArcTan => self.parse_single_parameter_built_in_function_call(ArcTan)?,
            Token::ArcTan2 => self.parse_double_parameter_built_in_function_call(ArcTan2)?,
            Token::ArcSin => self.parse_single_parameter_built_in_function_call(ArcSin)?,
            Token::ArcCos => self.parse_single_parameter_built_in_function_call(ArcCos)?,

            Token::TanH => self.parse_single_parameter_built_in_function_call(TanH)?,
            Token::SinH => self.parse_single_parameter_built_in_function_call(SinH)?,
            Token::CosH => self.parse_single_parameter_built_in_function_call(CosH)?,

            Token::ArcTanH => self.parse_single_parameter_built_in_function_call(ArcTanH)?,
            Token::ArcSinH => self.parse_single_parameter_built_in_function_call(ArcSinH)?,
            Token::ArcCosH => self.parse_single_parameter_built_in_function_call(ArcCosH)?,

            _ => {
                return Err(Error {
                    source: span,
                    error_type: UnexpectedTokens {
                        expected: vec![Expected::Primary, Expected::UnaryOperator],
                    },
                });
            }
        };
        Ok(res)
    }

    pub fn parse_bracketed_expression(&mut self) -> Result<Node<Expression<'ast>>> {
        if let Err(error) = self.expect_lookahead(Token::ParenOpen) {
            self.non_critical_errors.push(error);
            if let Ok(expr) = self.parse_expression_id() {
                self.recover_expression_on_bracket()
            } else {
                let tmp_expr = Node::new(
                    Expression::Primary(Primary::Integer(0)),
                    Span::new_short_empty_span(0),
                );
                self.recover_on(Token::ParenClose, Token::ParenClose, true, true)?;
                Ok(tmp_expr)
            }
        } else {
            self.consume_lookahead();
            self.recover_expression_on_bracket()
        }
    }

    fn recover_expression_on_bracket(&mut self) -> Result<Node<Expression<'ast>>> {
        Ok(match self.parse_expression() {
            Ok(expr) => {
                if let Err(error) = self.expect_lookahead(Token::ParenClose) {
                    return Err(self.unrecoverable_error(UnexpectedEof {
                        expected: vec![Token::ParenClose],
                    }));
                }
                expr
            }
            Err(error) => {
                self.non_critical_errors.push(error);
                let tmp_expr = Node::new(
                    Expression::Primary(Primary::Integer(0)),
                    Span::new_short_empty_span(0),
                );
                self.recover_on(Token::ParenClose, Token::ParenClose, true, true)?;
                tmp_expr
            }
        })
    }

    fn parse_unary_operator(&mut self, unary_op: UnaryOperator) -> Result<Node<Expression<'ast>>> {
        let unary_op = Node {
            source: self.preprocessor.span(),
            contents: unary_op,
        };
        let expr = self.parse_atom()?;
        let expr = self.ast.push(expr);
        let span = unary_op.source.extend(self.ast[expr].source);
        Ok(Node::new(Expression::UnaryOperator(unary_op, expr), span))
    }

    pub fn parse_single_parameter_built_in_function_call(
        &mut self,
        call: BuiltInFunctionCall1p,
    ) -> Result<Node<Expression<'ast>>> {
        self.consume_lookahead();
        let start = self.preprocessor.current_start();

        let arg = self.parse_bracketed_expression()?;
        let arg = self.ast.push(arg);

        Ok(Node::new(
            Expression::Primary(Primary::BuiltInFunctionCall1p(call, arg)),
            self.span_to_current_end(start),
        ))
    }

    pub fn parse_double_parameter_built_in_function_call(
        &mut self,
        call: BuiltInFunctionCall2p,
    ) -> Result<Node<Expression<'ast>>> {
        self.consume_lookahead();
        let start = self.preprocessor.current_start();

        self.expect(Token::ParenOpen)?;
        let arg1 = self.parse_expression_id()?;
        self.expect(Token::Comma)?;
        let arg2 = self.parse_expression_id()?;
        self.expect(Token::ParenClose)?;

        Ok(Node::new(
            Expression::Primary(Primary::BuiltInFunctionCall2p(call, arg1, arg2)),
            self.span_to_current_end(start),
        ))
    }
}
