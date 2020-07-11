/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

#![allow(clippy::enum_glob_use)]
use crate::ast::{BinaryOperator, BranchAccess, Expression, Primary, UnaryOperator};
use crate::ir::BuiltInFunctionCall1p::*;
use crate::ir::BuiltInFunctionCall2p::*;
use crate::ir::{BuiltInFunctionCall1p, BuiltInFunctionCall2p, NoiseSource, SystemFunctionCall};
use crate::ir::{ExpressionId, Node};
use crate::parser::error::{Expected, Result};
use crate::parser::Error::UnexpectedTokens;
use crate::parser::Token;
use crate::parser::Token::ParenOpen;

use crate::parser::error::Error::UnexpectedEofExpecting;
use crate::parser::Parser;
use crate::sourcemap::span::DUMMY_SP;
use crate::symbol::{keywords, Ident};
use crate::util::format_list;

enum BinaryOperatorOrCondition {
    Condition,
    BinaryOperator(BinaryOperator, u8),
}
impl<'lt> Parser<'lt> {
    pub fn parse_expression(&mut self) -> Result<ExpressionId> {
        let lhs = self.parse_atom()?;
        self.precedence_climb_expression(0, lhs)
    }

    pub(super) fn precedence_climb_expression(
        &mut self,
        min_prec: u8,
        mut lhs: ExpressionId,
    ) -> Result<ExpressionId> {
        loop {
            match self.parse_binary_operator() {
                Ok(BinaryOperatorOrCondition::Condition) if min_prec <= 1 => {
                    self.consume(1);
                    let op_span = self.previous_span(1);
                    let if_val = self.parse_expression()?;
                    self.expect(Token::Colon)?;
                    let eiter_op_span = self.previous_span(1);
                    let else_val = self.parse_expression()?;
                    lhs = self.ast.expressions.push(Node::new(
                        Expression::Condtion(lhs, op_span, if_val, eiter_op_span, else_val),
                        self.ast[lhs].span.extend(self.ast[else_val].span),
                    ))
                }
                Ok(BinaryOperatorOrCondition::BinaryOperator(op, precedence))
                    if precedence >= min_prec =>
                {
                    self.consume(1);
                    let op_span = self.previous_span(1);
                    let mut rhs = self.parse_atom()?;
                    loop {
                        match self.parse_binary_operator() {
                            Ok(BinaryOperatorOrCondition::BinaryOperator(_, right_prec))
                                if right_prec > precedence =>
                            {
                                rhs = self.precedence_climb_expression(precedence, rhs)?
                            }
                            _ => break,
                        }
                    }
                    let op = Node::new(op, op_span);
                    let tmp = &self.ast[rhs];
                    let span = self.ast[lhs].span.extend(tmp.span);
                    lhs = self
                        .ast
                        .expressions
                        .push(Node::new(Expression::BinaryOperator(lhs, op, rhs), span))
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_binary_operator(&mut self) -> Result<BinaryOperatorOrCondition> {
        let (token, span) = self.look_ahead(0)?;
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
                return Err(UnexpectedTokens {
                    expected: format_list(vec![Expected::BinaryOperator]),
                    span,
                })
            }
        };
        Ok(BinaryOperatorOrCondition::BinaryOperator(res.0, res.1))
    }

    #[allow(clippy::too_many_lines)]
    fn parse_atom(&mut self) -> Result<ExpressionId> {
        let (token, span) = self.look_ahead(0)?;
        let res = match token {
            Token::Minus => {
                self.consume(1);
                return self.parse_unary_operator(UnaryOperator::ArithmeticNegate);
            }
            Token::Plus => {
                self.consume(1);
                return self.parse_unary_operator(UnaryOperator::ExplicitPositive);
            }
            Token::OpLogicNot => {
                self.consume(1);
                return self.parse_unary_operator(UnaryOperator::LogicNegate);
            }
            Token::OpBitNot => {
                self.consume(1);
                return self.parse_unary_operator(UnaryOperator::BitNegate);
            }

            Token::ParenOpen => return self.parse_bracketed_expression(),

            Token::LiteralString(val) => {
                self.consume(1);
                Node::new(Expression::Primary(Primary::String(val)), span)
            }

            Token::RealLiteral(val) => {
                self.consume(1);
                Node::new(Expression::Primary(Primary::Real(val)), span)
            }

            Token::IntLiteral(val) => {
                self.consume(1);
                Node::new(Expression::Primary(Primary::Integer(val as i64)), span)
            }

            Token::Ident(name) => {
                let start = self.previous_span(1);

                self.consume(1);

                let primary = if self.look_ahead(0)?.0 == Token::ParenOpen {
                    self.consume(1);

                    match self.look_ahead(0)?.0 {
                        Token::OpLess => {
                            let branch_access = BranchAccess::Implicit(self.parse_branch()?);
                            self.expect(Token::ParenClose)?;

                            Primary::BranchAccess(
                                Ident::new(name, span),
                                Node::new(branch_access, self.span_to_current_end(span)),
                            )
                        }

                        Token::ParenClose => {
                            self.consume(1);
                            Primary::FunctionCall(Ident::new(name, span), Vec::new())
                        }
                        _ => {
                            let mut parameters = vec![self.parse_expression()?];
                            self.parse_list_tail(
                                |sel| {
                                    parameters.push(sel.parse_expression()?);
                                    Ok(())
                                },
                                Token::ParenClose,
                                true,
                            )?;

                            Primary::FunctionCall(Ident::new(name, span), parameters)
                        }
                    }
                } else {
                    let path =
                        self.parse_hierarchical_identifier_with_start(Ident::new(name, span))?;
                    Primary::VariableOrNetReference(path)
                };

                Node::new(
                    Expression::Primary(primary),
                    self.span_to_current_end(start),
                )
            }
            Token::Potential => {
                let start = self.previous_span(1);
                self.consume(1);
                let res = Primary::BranchAccess(
                    Ident::new(keywords::potential, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }

            Token::Flow => {
                let start = self.previous_span(1);
                self.consume(1);
                let res = Primary::BranchAccess(
                    Ident::new(keywords::flow, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }

            Token::WhiteNoise => {
                let start = self.previous_span(1);
                self.consume(1);
                self.expect(Token::ParenOpen)?;

                let expr = self.parse_expression()?;

                let src = if self.look_ahead(0)?.0 == Token::Comma {
                    self.consume(1);
                    Some(self.expect_string()?)
                } else {
                    None
                };

                self.expect(Token::ParenClose)?;

                Node::new(
                    Expression::Primary(Primary::Noise(NoiseSource::White(expr), src)),
                    self.span_to_current_end(start),
                )
            }

            Token::FlickerNoise => {
                let start = self.previous_span(1);
                self.consume(1);
                //TODO multiarg function call recovery
                self.expect(Token::ParenOpen)?;
                let expr1 = self.parse_expression()?;
                self.expect(Token::Comma)?;
                let expr2 = self.parse_expression()?;
                let src = if self.look_ahead(0)?.0 == Token::Comma {
                    self.consume(1);
                    Some(self.expect_string()?)
                } else {
                    None
                };
                self.expect(Token::ParenClose)?;
                Node::new(
                    Expression::Primary(Primary::Noise(NoiseSource::Flicker(expr1, expr2), src)),
                    self.span_to_current_end(start),
                )
            }

            Token::TimeDerivative => {
                let start = self.previous_span(1);
                self.consume(1);
                let expr = self.parse_bracketed_expression()?;
                Node::new(
                    Expression::Primary(Primary::DerivativeByTime(expr)),
                    self.span_to_current_end(start),
                )
            }
            Token::TemperatureDerivative => {
                let start = self.previous_span(1);
                self.consume(1);
                let expr = self.parse_bracketed_expression()?;
                Node::new(
                    Expression::Primary(Primary::DerivativeByTemperature(expr)),
                    self.span_to_current_end(start),
                )
            }

            Token::PartialDerivative => {
                let start = self.previous_span(1);
                self.consume(1);
                self.expect(Token::ParenOpen)?;
                let expr_to_derive = self.parse_expression()?;

                self.expect(Token::Comma)?;

                let (token, span) = self.next()?;
                let disciplines_access = match token {
                    Token::Flow => Ident::new(keywords::flow, span),
                    Token::Potential => Ident::new(keywords::potential, span),
                    Token::Ident(name) => Ident::new(name, span),
                    _ => {
                        return Err(UnexpectedTokens {
                            expected: format_list(vec![Expected::BranchAcess]),
                            span,
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

            Token::Temperature => {
                self.consume(1);
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(
                        SystemFunctionCall::Temperature,
                    )),
                    span,
                )
            }

            Token::Vt => {
                self.consume(1);
                let temp_arg = if self.look_ahead(0)?.0 == Token::ParenOpen {
                    let expr = self.parse_bracketed_expression()?;
                    Some(expr)
                } else {
                    None
                };
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(SystemFunctionCall::Vt(
                        temp_arg,
                    ))),
                    span,
                )
            }

            Token::SimParam => {
                self.consume(1);
                self.expect(ParenOpen)?;
                let name = self.parse_expression()?;
                let default = if self.look_ahead(0)?.0 == Token::Comma {
                    self.consume(1);
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect(Token::ParenClose)?;
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(SystemFunctionCall::Simparam(
                        name, default,
                    ))),
                    span,
                )
            }

            Token::SimParamStr => {
                self.consume(1);
                let expr = self.parse_bracketed_expression()?;
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(
                        SystemFunctionCall::SimparamStr(expr),
                    )),
                    span,
                )
            }

            Token::PortConnected => {
                self.consume(1);
                self.expect(Token::ParenOpen)?;

                let name = self.parse_hierarchical_identifier()?;
                self.expect(Token::ParenClose)?;
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(
                        SystemFunctionCall::PortConnected(name),
                    )),
                    span,
                )
            }

            Token::ParamGiven => {
                self.consume(1);
                self.expect(Token::ParenOpen)?;

                let name = self.parse_identifier()?;
                let name = self.parse_hierarchical_identifier_with_start(name)?;
                self.expect(Token::ParenClose)?;
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(
                        SystemFunctionCall::ParameterGiven(name),
                    )),
                    span,
                )
            }

            Token::Min => return self.parse_double_parameter_built_in_function_call(Min),
            Token::Max => return self.parse_double_parameter_built_in_function_call(Max),

            Token::Pow => return self.parse_double_parameter_built_in_function_call(Pow),
            Token::Hypot => return self.parse_double_parameter_built_in_function_call(Hypot),
            Token::Sqrt => return self.parse_single_parameter_built_in_function_call(Sqrt),

            Token::LimExp => return self.parse_single_parameter_built_in_function_call(Exp(true)),
            Token::Exp => return self.parse_single_parameter_built_in_function_call(Exp(false)),
            Token::Ln => return self.parse_single_parameter_built_in_function_call(Ln),
            Token::Log => return self.parse_single_parameter_built_in_function_call(Log),

            Token::Abs => return self.parse_single_parameter_built_in_function_call(Abs),
            Token::Ceil => return self.parse_single_parameter_built_in_function_call(Ceil),
            Token::Floor => return self.parse_single_parameter_built_in_function_call(Floor),

            Token::Tan => return self.parse_single_parameter_built_in_function_call(Tan),
            Token::Sin => return self.parse_single_parameter_built_in_function_call(Sin),
            Token::Cos => return self.parse_single_parameter_built_in_function_call(Cos),

            Token::ArcTan => return self.parse_single_parameter_built_in_function_call(ArcTan),
            Token::ArcTan2 => return self.parse_double_parameter_built_in_function_call(ArcTan2),
            Token::ArcSin => return self.parse_single_parameter_built_in_function_call(ArcSin),
            Token::ArcCos => return self.parse_single_parameter_built_in_function_call(ArcCos),

            Token::TanH => return self.parse_single_parameter_built_in_function_call(TanH),
            Token::SinH => return self.parse_single_parameter_built_in_function_call(SinH),
            Token::CosH => return self.parse_single_parameter_built_in_function_call(CosH),

            Token::ArcTanH => return self.parse_single_parameter_built_in_function_call(ArcTanH),
            Token::ArcSinH => return self.parse_single_parameter_built_in_function_call(ArcSinH),
            Token::ArcCosH => return self.parse_single_parameter_built_in_function_call(ArcCosH),

            _ => {
                return Err(UnexpectedTokens {
                    expected: format_list(vec![Expected::Primary, Expected::UnaryOperator]),
                    span,
                })
            }
        };
        Ok(self.ast.expressions.push(res))
    }

    pub fn parse_bracketed_expression(&mut self) -> Result<ExpressionId> {
        self.try_expect(Token::ParenOpen);
        self.recover_expression_on_bracket()
    }

    pub fn recover_expression_on_bracket(&mut self) -> Result<ExpressionId> {
        Ok(match self.parse_expression() {
            Ok(expr) => {
                if let Err(error) = self.expect_lookahead(Token::ParenClose) {
                    self.non_critical_errors.add(error);
                    return Err(UnexpectedEofExpecting {
                        expected: Token::ParenClose,
                        span: self.panic_to_end(),
                    });
                }
                expr
            }
            Err(error) => {
                //debug!("Parser: Parsing expression inside bracket failed!");
                self.non_critical_errors.add(error);
                let tmp_expr = Node::new(Expression::Primary(Primary::Integer(0)), DUMMY_SP);
                self.recover_on(Token::ParenClose, |_| false, true, true)?;
                self.ast.expressions.push(tmp_expr)
            }
        })
    }

    fn parse_unary_operator(&mut self, unary_op: UnaryOperator) -> Result<ExpressionId> {
        let unary_op = Node {
            span: self.previous_span(1),
            contents: unary_op,
        };
        let expr = self.parse_atom()?;
        let span = unary_op.span.extend(self.ast[expr].span);
        let res = Node::new(Expression::UnaryOperator(unary_op, expr), span);
        Ok(self.ast.expressions.push(res))
    }

    pub fn parse_single_parameter_built_in_function_call(
        &mut self,
        call: BuiltInFunctionCall1p,
    ) -> Result<ExpressionId> {
        self.consume(1);
        let start = self.previous_span(1);

        let arg = self.parse_bracketed_expression()?;

        let res = Node::new(
            Expression::Primary(Primary::BuiltInFunctionCall1p(call, arg)),
            self.span_to_current_end(start),
        );
        Ok(self.ast.expressions.push(res))
    }

    pub fn parse_double_parameter_built_in_function_call(
        &mut self,
        call: BuiltInFunctionCall2p,
    ) -> Result<ExpressionId> {
        self.consume(1);
        let start = self.previous_span(1);

        self.expect(Token::ParenOpen)?;
        let arg1 = self.parse_expression()?;
        self.expect(Token::Comma)?;
        let arg2 = self.parse_expression()?;
        self.expect(Token::ParenClose)?;

        let res = Node::new(
            Expression::Primary(Primary::BuiltInFunctionCall2p(call, arg1, arg2)),
            self.span_to_current_end(start),
        );

        Ok(self.ast.expressions.push(res))
    }
}
