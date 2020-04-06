/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{BinaryOperator, BranchAccess, Expression, Primary, UnaryOperator};
use crate::ir::ast::BuiltInFunctionCall;
use crate::ir::Push;
use crate::ir::{ExpressionId, Node};
use crate::parser::error::Type::UnexpectedTokens;
use crate::parser::error::*;
use crate::parser::lexer::Token;
use crate::parser::primaries::{
    parse_real_value, parse_string, parse_unsigned_int_value, RealLiteralType,
};
use crate::parser::Parser;
use crate::symbol::{keywords, Ident};

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
                    self.lookahead.take();
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
                    self.lookahead.take();
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
                    self.lookahead.take();
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
                    self.lookahead.take();
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
            Token::LiteralString => {
                self.lookahead.take();
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
                        let start = self.preprocessor.current_start();
                        let res = Primary::BranchAccess(
                            Self::convert_to_nature_identifier(ident)?,
                            Node::new(
                                BranchAccess::Implicit(self.parse_branch()?),
                                self.span_to_current_end(start),
                            ),
                        );
                        self.expect(Token::ParenClose)?;
                        res
                    } else if self.look_ahead()?.0 == Token::ParenClose {
                        self.lookahead.take();
                        Primary::FunctionCall(ident.into(), Vec::new())
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
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = Primary::BranchAccess(
                    Ident::new(keywords::POTENTIAL, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }
            Token::Flow => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = Primary::BranchAccess(
                    Ident::new(keywords::FLOW, span),
                    self.parse_branch_access()?,
                );
                Node::new(Expression::Primary(res), self.span_to_current_end(start))
            }

            Token::Min => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let args = self.parse_double_parameter_built_in_function_call()?;
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Min(
                        args.0, args.1,
                    ))),
                    self.span_to_current_end(start),
                )
            }
            Token::Max => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let args = self.parse_double_parameter_built_in_function_call()?;
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Max(
                        args.0, args.1,
                    ))),
                    self.span_to_current_end(start),
                )
            }
            Token::Pow => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let args = self.parse_double_parameter_built_in_function_call()?;
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Pow(
                        args.0, args.1,
                    ))),
                    self.span_to_current_end(start),
                )
            }
            Token::Hypot => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let args = self.parse_double_parameter_built_in_function_call()?;
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Hypot(
                        args.0, args.1,
                    ))),
                    self.span_to_current_end(start),
                )
            }

            Token::Sqrt => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::Sqrt(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::Exp => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res =
                    BuiltInFunctionCall::Exp(self.parse_single_parameter_built_in_function_call()?);
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::Ln => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res =
                    BuiltInFunctionCall::Ln(self.parse_single_parameter_built_in_function_call()?);
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::Log => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res =
                    BuiltInFunctionCall::Log(self.parse_single_parameter_built_in_function_call()?);
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }

            Token::Abs => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res =
                    BuiltInFunctionCall::Abs(self.parse_single_parameter_built_in_function_call()?);
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::Ceil => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::Ceil(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::Floor => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::Floor(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }

            Token::Tan => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res =
                    BuiltInFunctionCall::Tan(self.parse_single_parameter_built_in_function_call()?);
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::Sin => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res =
                    BuiltInFunctionCall::Sin(self.parse_single_parameter_built_in_function_call()?);
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::Cos => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res =
                    BuiltInFunctionCall::Cos(self.parse_single_parameter_built_in_function_call()?);
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::ArcTan => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::ArcTan(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }

            Token::ArcTan2 => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let args = self.parse_double_parameter_built_in_function_call()?;
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(BuiltInFunctionCall::Max(
                        args.0, args.1,
                    ))),
                    self.span_to_current_end(start),
                )
            }
            Token::ArcSin => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::ArcSin(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::ArcCos => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::ArcCos(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }

            Token::TanH => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::TanH(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::SinH => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::SinH(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::CosH => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::CosH(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::ArcTanH => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::ArcTanH(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::ArcSinH => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::ArcSinH(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::ArcCosH => {
                self.lookahead.take();
                let start = self.preprocessor.current_start();
                let res = BuiltInFunctionCall::ArcCosH(
                    self.parse_single_parameter_built_in_function_call()?,
                );
                Node::new(
                    Expression::Primary(Primary::BuiltInFunctionCall(res)),
                    self.span_to_current_end(start),
                )
            }
            Token::SystemCall => {
                self.lookahead.take();
                Node::new(
                    Expression::Primary(Primary::SystemFunctionCall(Ident::from_str_and_span(
                        self.preprocessor.slice(),
                        span,
                    ))),
                    span,
                )
            }

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
    pub fn parse_single_parameter_built_in_function_call(&mut self) -> Result<ExpressionId<'ast>> {
        self.expect(Token::ParenOpen)?;
        let res = self.parse_expression_id()?;
        self.expect(Token::ParenClose)?;
        Ok(res)
    }
    pub fn parse_double_parameter_built_in_function_call(
        &mut self,
    ) -> Result<(ExpressionId<'ast>, ExpressionId<'ast>)> {
        self.expect(Token::ParenOpen)?;
        let arg1 = self.parse_expression_id()?;
        self.expect(Token::Comma)?;
        let arg2 = self.parse_expression_id()?;
        self.expect(Token::ParenClose)?;
        Ok((arg1, arg2))
    }
}
