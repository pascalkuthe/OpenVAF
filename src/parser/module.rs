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
use crate::ast::{
    Expression, Module, ModuleItem, NumericalParameterRangeBound, NumericalParameterRangeExclude,
    Parameter, ParameterType, Primary, VariableType,
};
use crate::error::Error;
use crate::ir::{AttributeNode, Attributes, ExpressionId, Node};
use crate::ir::{Push, SafeRangeCreation};
use crate::parser::error;
use crate::parser::error::Expected::ParameterRange;
use crate::parser::error::Type::{UnexpectedToken, UnexpectedTokens, Unsupported};
use crate::parser::error::Unsupported::StringParameters;
use crate::parser::error::{Expected, Result, Type};
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};

impl<'lt, 'ast, 'source_map> Parser<'lt, 'ast, 'source_map> {
    pub(crate) const SYMBOL_TABLE_DEFAULT_SIZE: usize = 512;

    /// Parses a Module and all its items
    /// If an error occurs while trying to parse a module items the parser tries to recover to the next semicolon
    pub(super) fn parse_module(&mut self, attributes: Attributes<'ast>) -> Result {
        let start = self.preprocessor.current_start();

        //Module head

        let name = self.parse_identifier(false);
        self.scope_stack
            .push(SymbolTable::with_capacity(Self::SYMBOL_TABLE_DEFAULT_SIZE));

        let parameter_list = self.ast.empty_range_from_end();
        let port_list = self.ast.empty_range_from_end();

        let parameter_list_start = self.preprocessor.current_start();
        let (port_list_span, mut expected_ports) = if let Err(error) = self.parse_parameter_list() {
            self.non_critical_errors.push(error);
            self.recover_on(Token::EndModule, Token::Semicolon, true)?;
            (self.span_to_current_end(parameter_list_start), None)
        } else {
            let port_list_start = self.preprocessor.current_start();
            let expected_ports = self.parse_module_ports().ok().flatten();
            let port_list_span = self
                .span_to_current_end(port_list_start)
                .negative_offset(start);
            self.expect(Token::Semicolon)?;
            (port_list_span, expected_ports)
        };

        //Module body

        let mut module_items = Vec::with_capacity(16);
        let variable_start = self.ast.empty_range_from_end();
        let branch_start = self.ast.empty_range_from_end();

        self.parse_and_recover_on_tokens(Token::Semicolon, Token::EndModule, true, |parser, _| {
            let attributes = parser.parse_attributes()?;
            match parser.look_ahead()?.0 {
                Token::Inout | Token::Input | Token::Output => {
                    if let Some(ref mut expected) = expected_ports {
                        parser.parse_port_declaration(attributes, expected, port_list_span)?;
                    } else {
                        let port_base = parser.parse_port_declaration_base(attributes)?;
                        let source = parser.ast[port_base]
                            .source //we do this here so that the error doesnt just underline the input token but the entire declaration instead
                            .negative_offset(start);
                        parser.non_critical_errors.push(Error {
                            source: parser.span_to_current_end(start),
                            error_type: error::Type::PortRedeclaration(source, port_list_span),
                        });
                    }
                }
                Token::Analog => {
                    parser.lookahead.take();
                    let module_item = ModuleItem::AnalogStmt(parser.parse_statement(attributes)?);
                    module_items.push(module_item);
                }

                Token::Branch => {
                    parser.lookahead.take();
                    parser.parse_branch_declaration(attributes)?;
                }

                Token::Integer => {
                    parser.lookahead.take();
                    parser.parse_variable_declaration(INTEGER, attributes)?;
                }

                Token::Real => {
                    parser.lookahead.take();
                    parser.parse_variable_declaration(REAL, attributes)?;
                }

                Token::Realtime => {
                    parser.lookahead.take();
                    parser.parse_variable_declaration(REALTIME, attributes)?;
                }

                Token::Time => {
                    parser.lookahead.take();
                    parser.parse_variable_declaration(TIME, attributes)?;
                }

                Token::Parameter => {
                    parser.lookahead.take();
                    parser.parse_parameter_decl(attributes)?;
                }

                _ => {
                    parser.parse_net_declaration(attributes)?;
                }
            }
            Ok(())
        })?;

        if let Some(expected_ports) = expected_ports {
            for port in expected_ports {
                self.non_critical_errors.push(Error {
                    error_type: Type::PortPreDeclaredNotDefined,
                    source: port.span,
                })
            }
        }

        if let Ok(name) = name {
            let module = self.ast.push(AttributeNode {
                attributes,
                source: self.span_to_current_end(start),
                contents: Module {
                    name,
                    port_list: self.ast.extend_range_to_end(port_list),
                    parameter_list: self.ast.extend_range_to_end(parameter_list),
                    variables: self.ast.extend_range_to_end(variable_start),
                    branches: self.ast.extend_range_to_end(branch_start),
                    symbol_table: self.scope_stack.pop().unwrap(),
                    children: module_items,
                },
            });
            self.insert_symbol(name, SymbolDeclaration::Module(module));
        }

        Ok(())
    }

    fn parse_module_ports(&mut self) -> Result<Option<HashSet<Ident>>> {
        let res = if self.look_ahead()?.0 == Token::ParenOpen {
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
        Ok(res)
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
        if self.look_ahead()?.0 == Token::Hash {
            self.lookahead.take();
            self.expect(Token::ParenOpen)?;
            self.parse_list(
                |parser| {
                    let attributes = parser.parse_attributes()?;
                    parser.parse_parameter_decl(attributes);
                    Ok(())
                },
                Token::ParenClose,
                true,
            )?;
        }
        Ok(())
    }
}
