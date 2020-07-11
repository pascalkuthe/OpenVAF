/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::VariableType::{INTEGER, REAL};
use crate::ast::{Module, ModuleItem};
use crate::ir::ast::{Function, FunctionArg, Variable, VariableType};
use crate::ir::ids::IdRange;
use crate::ir::{AttributeNode, Attributes};
use crate::parser::error::Error::{
    FunctionWithoutBody, PortPreDeclaredNotDefined, PortRedeclaration, UnexpectedTokens,
};
use crate::parser::error::{Expected, Result};
use crate::parser::statements::stmt_recover;
use crate::parser::Parser;
use crate::parser::Token;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::format_list;
use crate::HashSet;

impl<'lt> Parser<'lt> {
    pub(crate) const SYMBOL_TABLE_DEFAULT_SIZE: usize = 512;

    #[allow(clippy::too_many_lines)]
    /// Parses a Module and all its items
    /// If an error occurs while trying to parse a module items the parser tries to recover to the next semicolon
    pub(super) fn parse_module(&mut self, attributes: Attributes) -> Result {
        //Module head

        let name = self.parse_identifier()?;
        self.scope_stack.push(SymbolTable::with_capacity_and_hasher(
            Self::SYMBOL_TABLE_DEFAULT_SIZE,
            Default::default(),
        ));

        let port_start = self.ast.ports.len_idx();

        let port_list_span;

        let mut expected_ports = if let Err(error) = self.parse_parameter_list() {
            self.non_critical_errors.add(error);
            port_list_span = self.look_ahead(0)?.1;
            self.recover_on(
                Token::EndModule,
                |token| token == Token::Semicolon,
                false,
                false,
            )?;
            None
        } else {
            port_list_span = self.look_ahead(0)?.1;
            self.parse_module_ports().ok().flatten()
        };

        let port_list_span = self.span_to_current_end(port_list_span);

        self.try_expect(Token::Semicolon);

        //Module body

        let mut module_items = Vec::with_capacity(16);
        let branch_start = self.ast.branches.len_idx();

        self.parse_and_recover_on_tokens(stmt_recover(), Token::EndModule, true, true, |parser| {
            let attributes = parser.parse_attributes()?;
            let (token, span) = parser.look_ahead(0)?;
            match token {
                Token::Inout | Token::Input | Token::Output => {
                    parser.parse_port_declaration(
                        attributes,
                        expected_ports.as_mut().unwrap_or(&mut HashSet::default()),
                        port_list_span,
                    )?;
                    if expected_ports.is_none() {
                        parser.non_critical_errors.add(PortRedeclaration {
                            module_head: port_list_span,
                            body_declaration: parser.span_to_current_end(span),
                        });
                    }
                }
                Token::Analog => {
                    parser.consume(1);
                    if Token::Function == parser.look_ahead(0)?.0 {
                        parser.consume(1);
                        parser.parse_function(attributes)?;
                    } else {
                        let module_item =
                            ModuleItem::AnalogStmt(parser.parse_statement(attributes)?);
                        module_items.push(module_item);
                    }
                }

                Token::Branch => {
                    parser.consume(1);
                    parser.parse_branch_declaration(attributes)?;
                }

                Token::Integer | Token::Time => {
                    parser.consume(1);
                    parser.parse_variable_declaration(INTEGER, attributes)?;
                }

                Token::Real | Token::Realtime => {
                    parser.consume(1);
                    parser.parse_variable_declaration(REAL, attributes)?;
                }

                Token::Parameter => {
                    parser.consume(1);
                    parser.parse_parameter_decl(attributes)?;
                }

                _ => {
                    parser.parse_net_declaration(attributes)?;
                }
            }
            Ok(true)
        })?;

        if let Some(expected_ports) = expected_ports {
            for port in expected_ports {
                self.non_critical_errors
                    .add(PortPreDeclaredNotDefined(port))
            }
        }

        let module = self.ast.modules.push(AttributeNode {
            attributes,
            span: self.span_to_current_end(name.span),
            contents: Module {
                name,
                port_list: IdRange(port_start..self.ast.ports.len_idx()),
                branch_list: IdRange(branch_start..self.ast.branches.len_idx()),
                symbol_table: self.scope_stack.pop().unwrap(),
                children: module_items,
            },
        });
        self.insert_symbol(name, SymbolDeclaration::Module(module));

        Ok(())
    }

    fn parse_module_ports(&mut self) -> Result<Option<HashSet<Ident>>> {
        let res = if self.look_ahead(0)?.0 == Token::ParenOpen {
            self.consume(1);

            let (token, span) = self.look_ahead(0)?;
            let expected_ports = match token {
                Token::ParenClose => None,

                Token::Input | Token::Output | Token::Inout | Token::ParenOpen => {
                    self.parse_port_declaration_list()?;
                    None
                }

                Token::Ident(_) => Some(self.parse_port_list()?),

                _ => {
                    return Err(UnexpectedTokens {
                        expected: format_list(vec![Expected::PortDeclaration, Expected::Port]),
                        span,
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

        res.insert(self.parse_identifier()?);
        while self.look_ahead(0)?.0 == Token::Comma {
            self.consume(1);

            res.insert(self.parse_identifier()?);
        }
        Ok(res)
    }

    fn parse_parameter_list(&mut self) -> Result {
        if self.look_ahead(0)?.0 == Token::Hash {
            self.consume(1);
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
        let (token, span) = self.look_ahead(0)?;
        let return_type = match token {
            Token::Integer => {
                self.consume(1);
                VariableType::INTEGER
            }
            Token::Real => {
                self.consume(1);
                VariableType::REAL
            }
            _ => VariableType::REAL,
        };

        let name = self.parse_identifier()?;
        self.try_expect(Token::Semicolon);
        let mut args = Vec::with_capacity(8);
        let mut symbol_table = SymbolTable::with_capacity_and_hasher(16, Default::default());
        let return_variable = self.ast.variables.push(AttributeNode {
            attributes: Attributes::EMPTY,
            span: self.span_to_current_end(span),
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
            stmt_recover(),
            Token::EndFunction,
            false,
            true,
            |parser| {
                match parser.look_ahead(0)?.0 {
                    Token::Input => {
                        parser.consume(1);

                        parser.parse_list(
                            |parser| {
                                args.push(FunctionArg {
                                    name: parser.parse_identifier()?,
                                    input: true,
                                    output: false,
                                });
                                Ok(())
                            },
                            Token::Semicolon,
                            true,
                        )?;
                    }

                    Token::Output => {
                        parser.consume(1);

                        parser.parse_list(
                            |parser| {
                                args.push(FunctionArg {
                                    name: parser.parse_identifier()?,
                                    input: false,
                                    output: true,
                                });
                                Ok(())
                            },
                            Token::Semicolon,
                            true,
                        )?;
                    }

                    Token::Inout => {
                        parser.consume(1);

                        parser.parse_list(
                            |parser| {
                                args.push(FunctionArg {
                                    name: parser.parse_identifier()?,
                                    input: true,
                                    output: true,
                                });
                                Ok(())
                            },
                            Token::Semicolon,
                            true,
                        )?;
                    }

                    _ => {
                        let attributes = parser.parse_attributes()?;
                        match parser.look_ahead(0)?.0 {
                            Token::Integer | Token::Time => {
                                parser.consume(1);
                                parser.parse_variable_declaration(INTEGER, attributes)?;
                            }

                            Token::Real | Token::Realtime => {
                                parser.consume(1);
                                parser.parse_variable_declaration(REAL, attributes)?;
                            }

                            Token::Parameter => {
                                parser.consume(1);
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

        self.try_expect(Token::EndFunction);

        let declarations = self.scope_stack.pop().unwrap();

        if let Some(body) = body {
            let id = self.ast.functions.push(AttributeNode {
                attributes,
                span: self.span_to_current_end(span),
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
            self.non_critical_errors
                .add(FunctionWithoutBody(name, self.span_to_current_end(span)))
        }

        Ok(())
    }
}
