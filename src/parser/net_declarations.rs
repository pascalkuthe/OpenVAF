/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{Net, NetType, Port};
use crate::ir::Push;
use crate::ir::{AttributeNode, Attributes, PortId};
use crate::parser::error::Type::{AlreadyDeclaredInThisScope, PortNotPreDeclaredInModuleHead};
use crate::parser::error::{Result, Type};
use crate::parser::lexer::Token;
use crate::parser::{error, Error, Parser};
use crate::symbol::{keywords, Ident};
use crate::symbol_table::SymbolDeclaration;
use crate::Span;
use rustc_hash::FxHashSet;

impl<'lt, 'ast, 'source_map> Parser<'lt, 'ast, 'source_map> {
    pub fn parse_port_declaration_list(&mut self) -> Result {
        let attributes = self.parse_attributes()?;
        let first_port = self.parse_port_declaration_base(attributes)?;
        self.insert_symbol(
            self.ast[first_port].contents.name,
            SymbolDeclaration::Port(first_port),
        );
        let mut last_port = first_port;
        while self.look_ahead()?.0 == Token::Comma {
            while self.next()?.0 == Token::Comma {
                if let Ok(name) = self.parse_identifier(true) {
                    self.ast.push(AttributeNode {
                        attributes: self.ast[last_port].attributes,
                        source: self.ast[last_port].source.extend(name.span),
                        contents: Port {
                            name,
                            ..self.ast[last_port].contents
                        },
                    });
                    self.insert_symbol(
                        self.ast[last_port].contents.name,
                        SymbolDeclaration::Port(last_port),
                    );
                } else {
                    break;
                }
            }
            let attributes = self.parse_attributes()?;
            last_port = self.parse_port_declaration_base(attributes)?;
            self.insert_symbol(
                self.ast[last_port].contents.name,
                SymbolDeclaration::Port(last_port),
            );
        }
        Ok(())
    }

    pub fn parse_port_declaration(
        &mut self,
        attributes: Attributes<'ast>,
        expected: &mut FxHashSet<Ident>,
        port_list: Span,
    ) -> Result {
        let first_port = self.parse_port_declaration_base(attributes)?;
        self.insert_port(first_port, port_list, expected);
        self.parse_list_tail(
            |sel: &mut Self| {
                let name = sel.parse_identifier(false)?;
                let current_port = sel.ast.push(AttributeNode {
                    attributes,
                    source: sel.ast[first_port].source.extend(name.span),
                    contents: Port {
                        name,
                        ..sel.ast[first_port].contents
                    },
                });
                sel.insert_port(current_port, port_list, expected);
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    fn insert_port(
        &mut self,
        port: PortId<'ast>,
        port_list: Span,
        expected: &mut FxHashSet<Ident>,
    ) {
        if expected.remove(&self.ast[port].contents.name) {
            self.insert_symbol(self.ast[port].contents.name, SymbolDeclaration::Port(port));
        } else {
            let ident = self.ast[port].contents.name;
            if let Some(old_declaration) = self
                .scope_stack
                .last()
                .unwrap_or(&self.ast.top_symbols)
                .get(&ident.name)
            {
                self.non_critical_errors.push(Error {
                    error_type: AlreadyDeclaredInThisScope {
                        other_declaration: old_declaration.span(&self.ast),
                        name: ident.name,
                    },
                    source: ident.span,
                });
            } else {
                self.non_critical_errors.push(Error {
                    error_type: PortNotPreDeclaredInModuleHead { port_list },
                    source: ident.span,
                })
            }
        }
    }

    /// this parses a port Declaration which only declares one port (for example input electrical x but not input electrical x,y)
    /// this function is a helper function to either be called from parse_port_declaration or parse_port_declaration_list which handel the extra ports declared
    pub(super) fn parse_port_declaration_base(
        &mut self,
        attributes: Attributes<'ast>,
    ) -> Result<PortId<'ast>> {
        let (token, span) = self.next()?;
        let start = self.preprocessor.current_start();
        let (input, output) = match token {
            Token::Input => (true, false),
            Token::Output => (false, true),
            Token::Inout => (true, true),
            _ => {
                return Err(Error {
                    source: span,
                    error_type: error::Type::UnexpectedToken {
                        expected: vec![Token::Inout, Token::Input, Token::Output],
                    },
                })
            }
        };

        let opt_first_identifier_or_discipline = self.parse_identifier(true);
        let mut is_discipline = false; //helps resolve the ambiguity whether an identifier refers to the first name or the discipline of a port declaration
        let port_type = self.parse_net_type()?;
        if port_type != NetType::UNDECLARED {
            is_discipline = true;
        }
        let (discipline, signed, name) =
            self.parse_net_declaration_end(is_discipline, opt_first_identifier_or_discipline)?;
        let res = self.ast.push(AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: Port {
                name,
                input,
                output,
                discipline,
                signed,
                net_type: port_type,
            },
        });
        Ok(res)
    }

    #[inline]
    pub fn insert_net(&mut self, declaration: AttributeNode<'ast, Net>) {
        if let Some(old_declaration) = self.symbol_table().get(&declaration.contents.name.name) {
            if let SymbolDeclaration::Port(id) = *old_declaration {
                if self.ast[id].contents.net_type == NetType::UNDECLARED
                    && self.ast[id].contents.discipline.name == keywords::EMPTY_SYMBOL
                    && (self.ast[id].contents.signed == declaration.contents.signed
                        || !self.ast[id].contents.signed)
                //TODO range
                {
                    self.ast[id].contents.net_type = declaration.contents.net_type;
                    self.ast[id].contents.discipline = declaration.contents.discipline;
                    return;
                }
            }
            let error_type = Type::AlreadyDeclaredInThisScope {
                other_declaration: old_declaration.span(&self.ast),
                name: declaration.contents.name.name,
            };
            self.non_critical_errors.push(Error {
                error_type,
                source: declaration.source,
            });
        } else {
            let id = self.ast.push(declaration);
            self.symbol_table_mut()
                .insert(declaration.contents.name.name, SymbolDeclaration::Net(id));
        }
    }

    pub fn parse_net_declaration(&mut self, attributes: Attributes<'ast>) -> Result {
        let start = self.preprocessor.current_start();
        let net_type = self.parse_net_type()?;
        let opt_first_identifier_or_discipline = self.parse_identifier(true);
        let (discipline, signed, first_name) =
            self.parse_net_declaration_end(false, opt_first_identifier_or_discipline)?;
        let net = AttributeNode {
            attributes,
            contents: Net {
                name: first_name,
                discipline,
                signed,
                net_type,
            },
            source: self.span_to_current_end(start),
        };
        self.insert_net(net);
        self.parse_list_tail(
            |sel: &mut Self| {
                let name = sel.parse_identifier(false)?;
                let source = sel.span_to_current_end(start);
                let net = AttributeNode {
                    attributes,
                    contents: Net {
                        name,
                        discipline,
                        signed,
                        net_type,
                    },
                    source,
                };
                sel.insert_net(net);
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    /// This is a helper method which parses the end of a (port or variable) declaration;
    /// It specifically handles the identifier/discipline ambiguity and also whether the variable is signed
    // This cant handle the type because disciplines and types have a different order in port (electrical wire) and variable delcarations (electrical
    fn parse_net_declaration_end(
        &mut self,
        mut is_discipline: bool,
        opt_first_identifier_or_discipline: Result<Ident>,
    ) -> Result<(Ident, bool, Ident)> {
        let signed = self.look_ahead()?.0 == Token::Signed;

        if signed {
            self.consume_lookahead();
            is_discipline = true;
        };

        let (name, discipline) = match opt_first_identifier_or_discipline {
            Ok(discipline) if is_discipline => (self.parse_identifier(false)?, discipline),
            Ok(first_identifier_or_discipline) => {
                if let Ok(first_identifier) = self.parse_identifier(true) {
                    (first_identifier, first_identifier_or_discipline)
                } else {
                    (first_identifier_or_discipline, Ident::empty())
                }
            }
            Err(_) => (self.parse_identifier(false)?, Ident::empty()),
        }; //TODO default discipline
        Ok((discipline, signed, name))
    }

    fn parse_net_type(&mut self) -> Result<NetType> {
        let token = self.look_ahead()?.0;
        let vtype = match token {
            Token::Wreal => NetType::WREAL,
            Token::Supply0 => NetType::SUPPLY0,
            Token::Supply1 => NetType::SUPPLY1,
            Token::Tri => NetType::TRI,
            Token::TriAnd => NetType::TRIAND,
            Token::TriOr => NetType::TRIOR,
            Token::Tri0 => NetType::TRI0,
            Token::Tri1 => NetType::TRI1,
            Token::Wire => NetType::WIRE,
            Token::Uwire => NetType::UWIRE,
            Token::Wand => NetType::WAND,
            Token::Wor => NetType::WOR,
            Token::Ground => NetType::GROUND,
            _ => return Ok(NetType::UNDECLARED),
        };
        self.consume_lookahead();
        Ok(vtype)
    }
}
