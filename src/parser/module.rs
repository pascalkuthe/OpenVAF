/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::collections::HashSet;
use std::ops::Range;

use copyless::VecHelper;

use crate::ast::VariableType::{INTEGER, REAL, REALTIME, TIME};
use crate::ast::{AttributeNode, Attributes, Module, ModuleItem};
use crate::error::Error;
use crate::ir::ast::{
    NumericalParameterBaseType, NumericalParameterRangeBound, NumericalParameterRangeExclude,
    Parameter, ParameterType,
};
use crate::ir::ExpressionId;
use crate::parser::error;
use crate::parser::error::Expected::ParameterRange;
use crate::parser::error::Type::{
    ParameterRangeUnboundedInIllegalDirection, UnexpectedToken, UnexpectedTokens, Unsupported,
};
use crate::parser::error::Unsupported::StringParameters;
use crate::parser::error::{Expected, Result, Type};
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::{Push, SafeRangeCreation};

impl<'lt, 'ast, 'astref, 'source_map> Parser<'lt, 'ast, 'astref, 'source_map> {
    pub(crate) const SYMBOL_TABLE_DEFAULT_SIZE: usize = 512;
    pub(super) fn parse_module(&mut self, attributes: Attributes<'ast>) -> Result {
        let start = self.preprocessor.current_start();
        let name = self.parse_identifier(false)?;
        let parameter_start = self.ast.empty_range_from_end();
        if self.look_ahead()?.0 == Token::Hash {
            self.expect(Token::ParenOpen)?;
            self.parse_parameter_list()?;
            self.expect(Token::ParenClose)?;
        }
        self.scope_stack
            .push(SymbolTable::with_capacity(Self::SYMBOL_TABLE_DEFAULT_SIZE));
        let port_list_start = self.preprocessor.current_start();
        //ports
        let port_list = self.ast.empty_range_from_end();
        let mut expected_ports = if self.look_ahead()?.0 == Token::ParenOpen {
            self.lookahead.take();
            let (next_token, next_span) = self.look_ahead()?;
            let expected_ports = match next_token {
                Token::ParenClose => None,
                Token::Input | Token::Output | Token::Inout | Token::ParenOpen => {
                    self.parse_port_declaration_list()?;
                    None
                }
                Token::SimpleIdentifier | Token::EscapedIdentifier => Some(self.parse_port_list()?),
                _ => {
                    return Err(Error {
                        error_type: error::Type::UnexpectedTokens {
                            expected: vec![Expected::PortDeclaration, Expected::Port],
                        },
                        source: next_span,
                    })
                }
            };
            self.expect(Token::ParenClose)?;
            expected_ports
        } else {
            None
        };
        let port_list_span = self
            .span_to_current_end(port_list_start)
            .negative_offset(start);

        self.expect(Token::Semicolon)?;
        let mut module_items = Vec::with_capacity(16);
        loop {
            let attributes = self.parse_attributes()?;
            let (token, span) = self.look_ahead()?;
            match token {
                Token::Inout | Token::Input | Token::Output => {
                    if let Some(ref mut expected) = expected_ports {
                        self.parse_port_declaration(attributes, expected, port_list_span)?;
                    } else {
                        let port_base = self.parse_port_declaration_base(attributes)?;
                        let source = self.ast[port_base]
                            .source //we do this here so that the error doesnt just underline the input token but the entire declaration instead
                            .negative_offset(start);
                        self.non_critical_errors.push(Error {
                            source: self.span_to_current_end(start),
                            error_type: error::Type::PortRedeclaration(source, port_list_span),
                        });
                    }
                }
                Token::EOF => {
                    return Err(Error {
                        error_type: error::Type::UnexpectedEof {
                            expected: vec![Token::EndModule],
                        },
                        source: span,
                    })
                }
                Token::EndModule => {
                    self.lookahead.take();
                    break;
                }
                _ => {
                    if let Some(module_item) = self.parse_module_item(attributes)? {
                        module_items.push(module_item);
                    }
                }
            }
        }

        if let Some(expected_ports) = expected_ports {
            for port in expected_ports {
                self.non_critical_errors.push(Error {
                    error_type: Type::PortPreDeclaredNotDefined,
                    source: port.span,
                })
            }
        }
        let module = self.ast.push(AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: Module {
                name,
                port_list: self.ast.extend_range_to_end(port_list),
                parameter_list: self.ast.extend_range_to_end(parameter_start),
                symbol_table: self.scope_stack.pop().unwrap(),
                children: module_items,
            },
        });
        self.insert_symbol(name, SymbolDeclaration::Module(module));
        Ok(())
    }
    fn parse_port_list(&mut self) -> Result<HashSet<Ident>> {
        let mut res = HashSet::with_capacity(1);
        res.insert(self.parse_identifier(false)?);
        while self.look_ahead()?.0 == Token::Comma {
            self.lookahead.take();
            res.insert(self.parse_identifier(false)?);
        }
        Ok(res)
    }

    fn parse_parameter_list(&mut self) -> Result {
        unimplemented!()
    }

    //TODO avoid code duplication
    fn parse_module_item(
        &mut self,
        attributes: Attributes<'ast>,
    ) -> Result<Option<ModuleItem<'ast>>> {
        let res = match self.look_ahead()?.0 {
            Token::Analog => {
                self.lookahead.take();
                Some(ModuleItem::AnalogStmt(self.parse_statement(attributes)?))
            }
            Token::Branch => {
                self.lookahead.take();
                self.parse_branch_declaration(attributes)?;
                None
            }
            Token::Integer => {
                self.lookahead.take();
                self.parse_variable_declaration(INTEGER, attributes)?;
                None
            }
            Token::Real => {
                self.lookahead.take();
                self.parse_variable_declaration(REAL, attributes)?;
                None
            }
            Token::Realtime => {
                self.lookahead.take();
                self.parse_variable_declaration(REALTIME, attributes)?;
                None
            }
            Token::Time => {
                self.lookahead.take();
                self.parse_variable_declaration(TIME, attributes)?;
                None
            }
            Token::Parameter => {
                self.lookahead.take();
                self.parse_parameter_decl(attributes)?;
                None
            }

            _ => {
                self.parse_net_declaration(attributes)?;
                None
            }
        };
        Ok(res)
    }

    pub fn parse_parameter_decl(&mut self, attributes: Attributes<'ast>) -> Result {
        let (token, span) = self.next()?;
        let parameter_type = match token {
            Token::Integer => NumericalParameterBaseType::Integer,
            Token::Real => NumericalParameterBaseType::Real,
            Token::Realtime => NumericalParameterBaseType::Realtime,
            Token::Time => NumericalParameterBaseType::Time,
            Token::String => {
                return Err(Error {
                    error_type: Unsupported(StringParameters),
                    source: span,
                })
            }
            _ => {
                return Err(Error {
                    error_type: UnexpectedToken {
                        expected: vec![
                            Token::Integer,
                            Token::Real,
                            Token::Realtime,
                            Token::Time,
                            Token::String,
                        ],
                    },
                    source: span,
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
        attributes: Attributes<'ast>,
        base_type: NumericalParameterBaseType,
    ) -> Result {
        let start = self.preprocessor.current_start();
        let name = self.parse_identifier(false)?;
        let default_value = if self.look_ahead()?.0 == Token::Assign {
            self.lookahead.take();
            Some(self.parse_expression_id()?)
        } else {
            None
        };
        let mut included_ranges = Vec::new();
        let mut excluded_ranges = Vec::new();
        loop {
            match self.look_ahead()? {
                (Token::From, _) => {
                    self.lookahead.take();
                    match self.next()? {
                        (Token::SquareBracketOpen, _) => {
                            included_ranges
                                .alloc()
                                .init(self.parse_parameter_range(true)?);
                        }
                        (Token::ParenOpen, _) => {
                            included_ranges
                                .alloc()
                                .init(self.parse_parameter_range(false)?);
                        }
                        (_, source) => {
                            return Err(Error {
                                error_type: UnexpectedTokens {
                                    expected: vec![ParameterRange],
                                },
                                source,
                            })
                        }
                    }
                }
                (Token::Exclude, _) => {
                    self.lookahead.take();
                    match self.look_ahead()? {
                        (Token::SquareBracketOpen, _) => {
                            self.lookahead.take();
                            excluded_ranges
                                .alloc()
                                .init(NumericalParameterRangeExclude::Range(
                                    self.parse_parameter_range(true)?,
                                ));
                        }
                        (Token::ParenOpen, _) => {
                            self.lookahead.take();
                            excluded_ranges
                                .alloc()
                                .init(NumericalParameterRangeExclude::Range(
                                    self.parse_parameter_range(false)?,
                                ));
                        }
                        _ => {
                            excluded_ranges
                                .alloc()
                                .init(NumericalParameterRangeExclude::Value(
                                    self.parse_expression_id()?,
                                ));
                        }
                    }
                }
                (Token::Semicolon, _) | (Token::Comma, _) => break,
                (_, source) => {
                    return Err(Error {
                        error_type: UnexpectedToken {
                            expected: vec![Token::From, Token::Exclude, Token::Semicolon],
                        },
                        source,
                    })
                }
            }
        }
        let parameter_id = self.ast.push(AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: Parameter {
                name,
                parameter_type: ParameterType::Numerical {
                    parameter_type: base_type,
                    included_ranges,
                    excluded_ranges,
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
    ) -> Result<Range<NumericalParameterRangeBound<'ast>>> {
        let start = NumericalParameterRangeBound {
            bound: self.parse_parameter_range_expression(true)?,
            inclusive,
        };
        self.expect(Token::Colon)?;
        let end = self.parse_parameter_range_expression(false)?;
        let end_inclusive = match self.next()? {
            (Token::SquareBracketClose, _) => true,
            (Token::ParenClose, _) => false,
            (_, source) => {
                return Err(Error {
                    error_type: UnexpectedToken {
                        expected: vec![Token::ParenClose, Token::SquareBracketClose],
                    },
                    source,
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
        lower_bound: bool,
    ) -> Result<Option<ExpressionId<'ast>>> {
        let (token, source) = self.look_ahead()?;
        match token {
            Token::Infinity => {
                self.lookahead.take();
                if !lower_bound {
                    Ok(None)
                } else {
                    Err(Error {
                        error_type: ParameterRangeUnboundedInIllegalDirection,
                        source,
                    })
                }
            }
            Token::MinusInfinity => {
                self.lookahead.take();
                if lower_bound {
                    Ok(None)
                } else {
                    Err(Error {
                        error_type: ParameterRangeUnboundedInIllegalDirection,
                        source,
                    })
                }
            }
            _ => Ok(Some(self.parse_expression_id()?)),
        }
    }
}
