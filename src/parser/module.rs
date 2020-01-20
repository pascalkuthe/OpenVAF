/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{AttributeNode, Module, ModuleItem, Port, VariableType};
use crate::error::Error;
use crate::parser::error;
use crate::parser::error::{Expected, Result};
use crate::parser::lexer::Token;
use crate::parser::Parser;
use crate::symbol_table::SymbolTable;

impl<'source_map, 'ast> Parser<'source_map, 'ast> {
    pub(super) fn parse_module(&mut self) -> Result<Module<'ast>> {
        let start = self.preprocessor.current_start();
        let name = self.parse_identifier(false)?;
        //parameters
        if self.look_ahead()?.0 == Token::Hash {
            self.expect(Token::ParenOpen)?;
            self.parse_parameter_list()?;
            self.expect(Token::ParenClose)?;
        }
        self.scope_stack.push(SymbolTable::new());
        let port_list_start = self.preprocessor.current_start();
        //ports
        let (allow_port_declaration, port_list) = if self.look_ahead()?.0 == Token::ParenOpen {
            self.lookahead.take();
            let (next_token, next_span) = self.look_ahead()?;
            let (allow_declarations, ports) = match next_token {
                Token::Input | Token::Output | Token::Inout | Token::ParenOpen => {
                    (false, self.parse_port_declaration_list()?)
                }
                Token::SimpleIdentifier | Token::EscapedIdentifier => {
                    (true, self.parse_port_list()?)
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
            (allow_declarations, ports)
        } else {
            (false, Vec::new())
        };
        let port_list_span = self
            .span_to_current_end(port_list_start)
            .negative_offset(start);

        self.expect(Token::Semicolon)?;
        let mut declared_ports = if allow_port_declaration {
            Vec::new()
        } else {
            port_list
        };
        let mut module_items = Vec::new();
        loop {
            let (token, span) = self.look_ahead()?;
            match token {
                Token::Inout | Token::Input | Token::Output if allow_port_declaration => {
                    declared_ports.append(&mut self.parse_port_declaration()?)
                }
                Token::Inout | Token::Input | Token::Output => {
                    let source = self
                        .parse_port_declaration()?
                        .last()
                        .unwrap()
                        .source //we do this here so that the error doesnt just underline the input token but the entire declaration instead
                        .negative_offset(start);
                    return Err(Error {
                        source: self.span_to_current_end(start),
                        error_type: error::Type::PortRedeclaration(source, port_list_span),
                    });
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
                _ => module_items.append(&mut self.parse_module_item()?),
            }
        }
        //TODO build symbol table
        Ok(Module {
            name,
            port_list: self
                .ast_allocator
                .alloc_slice_copy(declared_ports.as_slice()),
            children: self.ast_allocator.alloc_slice_copy(module_items.as_slice()),
        })
    }

    fn parse_port_list(&mut self) -> Result<Vec<AttributeNode<'ast, Port>>> {
        let name = self.parse_identifier(false)?;
        let mut res = vec![AttributeNode {
            source: self.preprocessor.current_span(),
            attributes: self.parse_attributes()?,
            contents: Port {
                name,
                ..Port::default()
            },
        }];
        while self.look_ahead()?.0 == Token::Comma {
            self.lookahead.take();
            let name = self.parse_identifier(false)?;
            res.push(AttributeNode {
                source: self.preprocessor.current_span(),
                attributes: self.parse_attributes()?,
                contents: Port {
                    name,
                    ..Port::default()
                },
            })
        }
        Ok(res)
    }

    fn parse_parameter_list(&mut self) -> Result {
        unimplemented!()
    }

    fn parse_module_item(&mut self) -> Result<Vec<AttributeNode<'ast, ModuleItem<'ast>>>> {
        let attributes = self.parse_attributes()?;
        let start = self.look_ahead()?.1.get_start();
        let contents: Vec<ModuleItem> = match self.look_ahead()?.0 {
            Token::Analog => {
                self.lookahead.take();
                vec![ModuleItem::AnalogStmt(self.parse_statement()?)]
            }
            Token::Branch => {
                self.lookahead.take();
                self.parse_branch_declaration()?
                    .into_iter()
                    .map(|branch_decl| ModuleItem::BranchDecl(branch_decl))
                    .collect()
            }
            Token::Integer => {
                self.lookahead.take();
                self.parse_variable_declaration(VariableType::INTEGER)?
                    .into_iter()
                    .map(|variable_decl| ModuleItem::VariableDecl(variable_decl))
                    .collect()
            }
            Token::Real => {
                self.lookahead.take();
                self.parse_variable_declaration(VariableType::REAL)?
                    .into_iter()
                    .map(|variable_decl| ModuleItem::VariableDecl(variable_decl))
                    .collect()
            }
            Token::Realtime => {
                self.lookahead.take();
                self.parse_variable_declaration(VariableType::REALTIME)?
                    .into_iter()
                    .map(|variable_decl| ModuleItem::VariableDecl(variable_decl))
                    .collect()
            }
            Token::Time => {
                self.lookahead.take();
                self.parse_variable_declaration(VariableType::TIME)?
                    .into_iter()
                    .map(|variable_decl| ModuleItem::VariableDecl(variable_decl))
                    .collect()
            }

            _ => self
                .parse_net_declaration()?
                .into_iter()
                .map(|net_decl| ModuleItem::NetDecl(net_decl))
                .collect(),
        };
        let span = self.span_to_current_end(start);
        let res = contents
            .into_iter()
            .map(|module_items| AttributeNode {
                source: span,
                attributes,
                contents: module_items,
            })
            .collect();
        Ok(res)
    }
}
