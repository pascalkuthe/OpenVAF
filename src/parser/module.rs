/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::VariableType::{INTEGER, REAL};
use crate::ast::{Module, ModuleItem};
use crate::error::Error;
use crate::ir::ast::{Function, FunctionArg, Variable, VariableType};
use crate::ir::ids::IdRange;
use crate::ir::{AttributeNode, Attributes};
use crate::parser::error;
use crate::parser::error::{Expected, Result, Type};
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use rustc_hash::FxHashSet;

impl<'lt, 'source_map> Parser<'lt, 'source_map> {
    pub(crate) const SYMBOL_TABLE_DEFAULT_SIZE: usize = 512;

    #[allow(clippy::too_many_lines)]
    /// Parses a Module and all its items
    /// If an error occurs while trying to parse a module items the parser tries to recover to the next semicolon
    pub(super) fn parse_module(&mut self, attributes: Attributes) -> Result {
        let start = self.preprocessor.current_start();

        //Module head

        let name = self.parse_identifier(false);
        self.scope_stack.push(SymbolTable::with_capacity_and_hasher(
            Self::SYMBOL_TABLE_DEFAULT_SIZE,
            Default::default(),
        ));

        let port_start = self.ast.ports.len_idx();

        let parameter_list_start = self.preprocessor.current_start();
        let (port_list_span, mut expected_ports) = if let Err(error) = self.parse_parameter_list() {
            self.non_critical_errors.push(error);
            self.recover_on(Token::EndModule, Token::Semicolon, false, false)?;
            (self.span_to_current_end(parameter_list_start), None)
        } else {
            let port_list_start = self.preprocessor.current_start();
            let expected_ports = self.parse_module_ports().ok().flatten();
            let port_list_span = self
                .span_to_current_end(port_list_start)
                .negative_offset(start);
            (port_list_span, expected_ports)
        };

        self.try_expect(Token::Semicolon);

        //Module body

        let mut module_items = Vec::with_capacity(16);
        let branch_start = self.ast.branches.len_idx();

        self.parse_and_recover_on_tokens(
            Token::Semicolon,
            Token::EndModule,
            true,
            true,
            |parser| {
                let attributes = parser.parse_attributes()?;
                match parser.look_ahead()? {
                    Token::Inout | Token::Input | Token::Output => {
                        let declaration_start = parser.preprocessor.current_start();
                        parser.parse_port_declaration(
                            attributes,
                            expected_ports.as_mut().unwrap_or(&mut FxHashSet::default()),
                            port_list_span,
                        )?;
                        if expected_ports.is_none() {
                            let source = parser
                                .span_to_current_end(declaration_start)
                                .negative_offset(start);

                            parser.non_critical_errors.push(Error {
                                source: parser.span_to_current_end(start),
                                error_type: error::Type::PortRedeclaration(source, port_list_span),
                            });
                        }
                    }
                    Token::Analog => {
                        parser.lookahead.take();
                        if Token::Function == parser.look_ahead()? {
                            parser.consume_lookahead();
                            parser.parse_function(attributes)?;
                        } else {
                            let module_item =
                                ModuleItem::AnalogStmt(parser.parse_statement(attributes)?);
                            module_items.push(module_item);
                        }
                    }

                    Token::Branch => {
                        parser.lookahead.take();
                        parser.parse_branch_declaration(attributes)?;
                    }

                    Token::Integer | Token::Time => {
                        parser.lookahead.take();
                        parser.parse_variable_declaration(INTEGER, attributes)?;
                    }

                    Token::Real | Token::Realtime => {
                        parser.lookahead.take();
                        parser.parse_variable_declaration(REAL, attributes)?;
                    }

                    Token::Parameter => {
                        parser.lookahead.take();
                        parser.parse_parameter_decl(attributes)?;
                    }

                    _ => {
                        parser.parse_net_declaration(attributes)?;
                    }
                }
                Ok(true)
            },
        )?;

        if let Some(expected_ports) = expected_ports {
            for port in expected_ports {
                self.non_critical_errors.push(Error {
                    error_type: Type::PortPreDeclaredNotDefined,
                    source: port.span,
                })
            }
        }

        if let Ok(name) = name {
            let module = self.ast.modules.push(AttributeNode {
                attributes,
                source: self.span_to_current_end(start),
                contents: Module {
                    name,
                    port_list: IdRange(port_start..self.ast.ports.len_idx()),
                    branch_list: IdRange(branch_start..self.ast.branches.len_idx()),
                    symbol_table: self.scope_stack.pop().unwrap(),
                    children: module_items,
                },
            });
            self.insert_symbol(name, SymbolDeclaration::Module(module));
        }

        Ok(())
    }

    fn parse_module_ports(&mut self) -> Result<Option<FxHashSet<Ident>>> {
        let res = if self.look_ahead()? == Token::ParenOpen {
            self.consume_lookahead();

            let (next_token, next_span) = self.look_ahead_with_span()?;
            let expected_ports = match next_token {
                Token::ParenClose => None,

                Token::Input | Token::Output | Token::Inout | Token::ParenOpen => {
                    self.parse_port_declaration_list()?;
                    None
                }

                Token::SimpleIdentifier(_) | Token::EscapedIdentifier => {
                    Some(self.parse_port_list()?)
                }

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

    fn parse_port_list(&mut self) -> Result<FxHashSet<Ident>> {
        let mut res = FxHashSet::with_capacity_and_hasher(1, Default::default());
        res.insert(self.parse_identifier(false)?);
        while self.look_ahead()? == Token::Comma {
            self.consume_lookahead();
            res.insert(self.parse_identifier(false)?);
        }
        Ok(res)
    }

    fn parse_parameter_list(&mut self) -> Result {
        if self.look_ahead()? == Token::Hash {
            self.consume_lookahead();
            self.expect(Token::ParenOpen)?;
            self.parse_list(
                |parser| {
                    let attributes = parser.parse_attributes()?;
                    parser.parse_parameter_decl(attributes)?;
                    Ok(())
                },
                Token::ParenClose,
                true,
            )?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn parse_function(&mut self, attributes: Attributes) -> Result {
        let start = self.preprocessor.current_start();
        let return_type = match self.look_ahead()? {
            Token::Integer => {
                self.consume_lookahead();
                VariableType::INTEGER
            }
            Token::Real => {
                self.consume_lookahead();
                VariableType::REAL
            }
            _ => VariableType::REAL,
        };
        let name = self.parse_identifier(false)?;
        self.try_expect(Token::Semicolon);
        let mut args = Vec::with_capacity(8);
        let mut symbol_table = SymbolTable::with_capacity_and_hasher(16, Default::default());
        let return_variable = self.ast.variables.push(AttributeNode {
            attributes: Attributes::empty(),
            source: self.span_to_current_end(start),
            contents: Variable {
                name,
                variable_type: return_type,
                default_value: None,
            },
        });
        symbol_table.insert(name.name, SymbolDeclaration::Variable(return_variable));
        self.scope_stack.push(symbol_table);
        let mut body = None;
        self.parse_and_recover_on_tokens(
            Token::Semicolon,
            Token::EndFunction,
            false,
            true,
            |parser| {
                match parser.look_ahead()? {
                    Token::Input => {
                        parser.consume_lookahead();
                        args.push(FunctionArg {
                            name: parser.parse_identifier(false)?,
                            input: true,
                            output: false,
                        });
                        parser.try_expect(Token::Semicolon);
                    }

                    Token::Output => {
                        parser.consume_lookahead();
                        args.push(FunctionArg {
                            name: parser.parse_identifier(false)?,
                            input: false,
                            output: true,
                        });
                        parser.try_expect(Token::Semicolon);
                    }

                    Token::Inout => {
                        parser.consume_lookahead();
                        args.push(FunctionArg {
                            name: parser.parse_identifier(false)?,
                            input: true,
                            output: true,
                        });
                        parser.try_expect(Token::Semicolon);
                    }

                    _ => {
                        let attributes = parser.parse_attributes()?;
                        match parser.look_ahead()? {
                            Token::Integer | Token::Time => {
                                parser.consume_lookahead();
                                parser.parse_variable_declaration(INTEGER, attributes)?;
                            }

                            Token::Real | Token::Realtime => {
                                parser.consume_lookahead();
                                parser.parse_variable_declaration(REAL, attributes)?;
                            }

                            Token::Parameter => {
                                parser.consume_lookahead();
                                parser.parse_parameter_decl(attributes)?;
                            }
                            _ => {
                                body = Some(parser.parse_statement(attributes)?);
                                return Ok(false);
                            }
                        }
                    }
                }
                Ok(true)
            },
        )?;
        // Parser is good enough that we don't need this. So we comsume it if its there and produce a non critical error otherwise
        self.try_expect(Token::EndFunction);
        let declarations = self.scope_stack.pop().unwrap();
        if let Some(body) = body {
            let id = self.ast.functions.push(AttributeNode {
                attributes,
                source: self.span_to_current_end(start),
                contents: Function {
                    name,
                    args,
                    declarations,
                    return_variable,
                    body,
                },
            });
            self.insert_symbol(name, SymbolDeclaration::Function(id));
        } else {
            self.non_critical_errors.push(Error {
                error_type: Type::FunctionWithoutBody(name),
                source: self.span_to_current_end(start),
            });
        }
        Ok(())
    }
}
