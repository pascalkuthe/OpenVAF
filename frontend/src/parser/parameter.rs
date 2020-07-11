//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::ops::Range;

use crate::ast::{Expression, Parameter, ParameterType, Primary, VariableType};
use crate::diagnostic::Unsupported::StringParameters;
use crate::ir::{
    AttributeNode, Attributes, ExpressionId, Node, NumericalParameterRangeBound,
    NumericalParameterRangeExclude,
};
use crate::parser::error::Expected::ParameterRange;
use crate::parser::error::Result;
use crate::parser::Error::{
    MissingOrUnexpectedToken, UnexpectedToken, UnexpectedTokens, Unsupported,
};
use crate::parser::Token;
use crate::symbol_table::SymbolDeclaration;
use crate::util::format_list;
use crate::Parser;

impl<'lt> Parser<'lt> {
    pub fn parse_parameter_decl(&mut self, attributes: Attributes) -> Result {
        let (token, span) = self.next()?;
        let parameter_type = match token {
            Token::Integer | Token::Time => VariableType::INTEGER,
            Token::Real | Token::Realtime => VariableType::REAL,
            Token::String => return Err(Unsupported(StringParameters, span)),
            _ => {
                return Err(UnexpectedToken {
                    expected: format_list(vec![
                        Token::Integer,
                        Token::Real,
                        Token::Realtime,
                        Token::Time,
                        Token::String,
                    ]),
                    span,
                })
            }
        };
        self.parse_list(
            |sel| sel.parse_numerical_parameter_assignment(attributes, parameter_type),
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    fn parse_numerical_parameter_assignment(
        &mut self,
        attributes: Attributes,
        base_type: VariableType,
    ) -> Result {
        let start = self.previous_span(1);

        let name = self.parse_identifier()?;
        self.expect(Token::Assign)?;
        let default_value = self.parse_expression()?;
        let mut from_ranges = Vec::new();
        let mut excluded = Vec::new();
        loop {
            let (token, span) = self.look_ahead(0)?;
            match token {
                Token::From => {
                    self.consume(1);
                    match self.next()? {
                        (Token::SquareBracketOpen, _) => {
                            from_ranges.push(self.parse_parameter_range(true, base_type)?);
                        }
                        (Token::ParenOpen, _) => {
                            from_ranges.push(self.parse_parameter_range(false, base_type)?);
                        }
                        (_, span) => {
                            return Err(UnexpectedTokens {
                                expected: format_list(vec![ParameterRange]),
                                span,
                            })
                        }
                    }
                }
                Token::Exclude => {
                    self.consume(1);
                    match self.look_ahead(0)? {
                        (Token::SquareBracketOpen, _) => {
                            self.consume(1);
                            excluded.push(NumericalParameterRangeExclude::Range(
                                self.parse_parameter_range(true, base_type)?,
                            ));
                        }
                        (Token::ParenOpen, _) => {
                            self.consume(1);
                            excluded.push(NumericalParameterRangeExclude::Range(
                                self.parse_parameter_range(false, base_type)?,
                            ));
                        }
                        _ => {
                            excluded.push(NumericalParameterRangeExclude::Value(
                                self.parse_expression()?,
                            ));
                        }
                    }
                }
                Token::Semicolon | Token::Comma => break,
                _ => {
                    return Err(UnexpectedToken {
                        expected: format_list(vec![Token::From, Token::Exclude, Token::Semicolon]),
                        span,
                    })
                }
            }
        }
        let parameter_id = self.ast.parameters.push(AttributeNode {
            attributes,
            span: self.span_to_current_end(start),
            contents: Parameter {
                name,
                parameter_type: ParameterType::Numerical {
                    parameter_type: base_type,
                    from_ranges,
                    excluded,
                },
                default_value,
            },
        });
        self.insert_symbol(name, SymbolDeclaration::Parameter(parameter_id));
        Ok(())
    }

    fn parse_parameter_range(
        &mut self,
        inclusive: bool,
        base_type: VariableType,
    ) -> Result<Range<NumericalParameterRangeBound<ExpressionId>>> {
        let start = NumericalParameterRangeBound {
            bound: self.parse_parameter_range_expression(base_type)?,
            inclusive,
        };
        self.expect(Token::Colon)?;
        let end = self.parse_parameter_range_expression(base_type)?;
        let (token, span) = self.next()?;
        let end_inclusive = match token {
            Token::SquareBracketClose => true,
            Token::ParenClose => false,
            _ => {
                return Err(MissingOrUnexpectedToken {
                    expected: format_list(vec![Token::ParenClose, Token::SquareBracketClose]),
                    expected_at: self.previous_span(2),
                    span,
                })
            }
        };
        Ok(start..NumericalParameterRangeBound {
            bound: end,
            inclusive: end_inclusive,
        })
    }

    fn parse_parameter_range_expression(
        &mut self,
        base_type: VariableType,
    ) -> Result<ExpressionId> {
        let (token, span) = self.look_ahead(0)?;
        match token {
            Token::Infinity if base_type == VariableType::REAL => {
                self.consume(1);
                Ok(self.ast.expressions.push(Node {
                    contents: Expression::Primary(Primary::Real(f64::INFINITY)),
                    span,
                }))
            }
            Token::MinusInfinity if base_type == VariableType::REAL => {
                self.consume(1);
                Ok(self.ast.expressions.push(Node {
                    contents: Expression::Primary(Primary::Real(f64::NEG_INFINITY)),
                    span,
                }))
            }

            Token::Infinity if base_type == VariableType::INTEGER => {
                self.consume(1);
                Ok(self.ast.expressions.push(Node {
                    contents: Expression::Primary(Primary::Integer(i64::MAX)),
                    span,
                }))
            }

            Token::MinusInfinity if base_type == VariableType::INTEGER => {
                self.consume(1);
                Ok(self.ast.expressions.push(Node {
                    contents: Expression::Primary(Primary::Integer(i64::MIN)),
                    span,
                }))
            }

            _ => Ok(self.parse_expression()?),
        }
    }
}
