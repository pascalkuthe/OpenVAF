/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{Net, NetType, Port};

use crate::ir::{AttributeNode, Attributes, PortId};
use crate::parser::error::Error::UnexpectedToken;
use crate::parser::error::Result;
use crate::parser::Error::{AlreadyDeclaredInThisScope, PortNotPreDeclaredInModuleHead};
use crate::parser::Parser;
use crate::parser::Token;
use crate::symbol::{keywords, Ident};
use crate::symbol_table::SymbolDeclaration;
use crate::util::format_list;
use crate::HashSet;
use crate::Span;

impl<'lt> Parser<'lt> {
    pub fn parse_port_declaration_list(&mut self) -> Result {
        let attributes = self.parse_attributes()?;
        let first_port = self.parse_port_declaration_base(attributes)?;
        self.insert_symbol(
            self.ast[first_port].contents.ident,
            SymbolDeclaration::Port(first_port),
        );
        let mut last_port = first_port;
        while self.look_ahead(0)?.0 == Token::Comma {
            while self.next()?.0 == Token::Comma {
                if let Some(ident) = self.parse_opt_identifier() {
                    self.ast.ports.push(AttributeNode {
                        attributes: self.ast[last_port].attributes,
                        span: self.ast[last_port].span.extend(ident.span),
                        contents: Port {
                            ident,
                            ..self.ast[last_port].contents
                        },
                    });
                    self.insert_symbol(
                        self.ast[last_port].contents.ident,
                        SymbolDeclaration::Port(last_port),
                    );
                } else {
                    break;
                }
            }
            let attributes = self.parse_attributes()?;
            last_port = self.parse_port_declaration_base(attributes)?;
            self.insert_symbol(
                self.ast[last_port].contents.ident,
                SymbolDeclaration::Port(last_port),
            );
        }
        Ok(())
    }

    pub fn parse_port_declaration(
        &mut self,
        attributes: Attributes,
        expected: &mut HashSet<Ident>,
        port_list: Span,
    ) -> Result {
        let first_port = self.parse_port_declaration_base(attributes)?;
        self.insert_port(first_port, port_list, expected);
        self.parse_list_tail(
            |parser| {
                let name = parser.parse_identifier()?;
                let current_port = parser.ast.ports.push(AttributeNode {
                    attributes,
                    span: parser.ast[first_port].span.extend(name.span),
                    contents: Port {
                        ident: name,
                        ..parser.ast[first_port].contents
                    },
                });
                parser.insert_port(current_port, port_list, expected);
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    fn insert_port(&mut self, port: PortId, port_list: Span, expected: &mut HashSet<Ident>) {
        if expected.remove(&self.ast[port].contents.ident) {
            self.insert_symbol(self.ast[port].contents.ident, SymbolDeclaration::Port(port));
        } else {
            let ident = self.ast[port].contents.ident;
            if let Some(old_declaration) = self
                .scope_stack
                .last()
                .unwrap_or(&self.ast.top_symbols)
                .get(&ident.name)
            {
                self.non_critical_errors.add(AlreadyDeclaredInThisScope {
                    declaration: ident.span,
                    other_declaration: old_declaration.span(self.ast),
                    name: ident.name,
                });
            } else {
                self.non_critical_errors
                    .add(PortNotPreDeclaredInModuleHead {
                        port_list,
                        port: ident,
                    })
            }
        }
    }

    /// this parses a port Declaration which only declares one port (for example input electrical x but not input electrical x,y)
    /// this function is a helper function to either be called from `parse_port_declaration` or `parse_port_declaration_list` which handel the extra ports declared
    pub(super) fn parse_port_declaration_base(&mut self, attributes: Attributes) -> Result<PortId> {
        let (token, span) = self.next()?;
        let start = self.previous_span(1);
        let (input, output) = match token {
            Token::Input => (true, false),
            Token::Output => (false, true),
            Token::Inout => (true, true),
            _ => {
                return Err(UnexpectedToken {
                    expected: format_list(vec![Token::Inout, Token::Input, Token::Output]),
                    span,
                })
            }
        };

        let opt_first_identifier_or_discipline = self.parse_opt_identifier();
        let mut is_discipline = false; //helps resolve the ambiguity whether an identifier refers to the first name or the discipline of a port declaration
        let port_type = self.parse_net_type()?;
        if port_type != NetType::UNDECLARED {
            is_discipline = true;
        }
        let (discipline, signed, name) =
            self.parse_net_declaration_end(is_discipline, opt_first_identifier_or_discipline)?;

        let res = self.ast.ports.push(AttributeNode {
            attributes,
            span: self.span_to_current_end(start),
            contents: Port {
                ident: name,
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
    pub fn insert_net(&mut self, declaration: AttributeNode<Net>) {
        if let Some(old_declaration) = self.symbol_table().get(&declaration.contents.name.name) {
            if let SymbolDeclaration::Port(id) = *old_declaration {
                if self.ast[id].contents.net_type == NetType::UNDECLARED
                    && self.ast[id].contents.discipline.name == keywords::EMPTY
                    && (self.ast[id].contents.signed == declaration.contents.signed
                        || !self.ast[id].contents.signed)
                //TODO range
                {
                    self.ast[id].contents.net_type = declaration.contents.net_type;
                    self.ast[id].contents.discipline = declaration.contents.discipline;
                    return;
                }
            }
            let error = AlreadyDeclaredInThisScope {
                declaration: declaration.span,
                other_declaration: old_declaration.span(self.ast),
                name: declaration.contents.name.name,
            };

            self.non_critical_errors.add(error);
        } else {
            let id = self.ast.nets.push(declaration);
            self.symbol_table_mut()
                .insert(declaration.contents.name.name, SymbolDeclaration::Net(id));
        }
    }

    pub fn parse_net_declaration(&mut self, attributes: Attributes) -> Result {
        let start = self.previous_span(1);
        let net_type = self.parse_net_type()?;
        let opt_first_identifier_or_discipline = self.parse_opt_identifier();

        let (discipline, signed, first_name) = self.parse_net_declaration_end(
            net_type == NetType::UNDECLARED,
            opt_first_identifier_or_discipline,
        )?;
        let net = AttributeNode {
            attributes,
            contents: Net {
                name: first_name,
                discipline,
                signed,
                net_type,
            },
            span: self.span_to_current_end(start),
        };
        self.insert_net(net);
        self.parse_list_tail(
            |parser: &mut Self| {
                let name = parser.parse_identifier()?;
                let source = parser.span_to_current_end(start);
                let net = AttributeNode {
                    attributes,
                    contents: Net {
                        name,
                        discipline,
                        signed,
                        net_type,
                    },
                    span: source,
                };
                parser.insert_net(net);
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(())
    }

    /// This is a helper method which parses the end of a (port or net) declaration;
    /// It specifically handles the identifier/discipline ambiguity and also whether the variable is signed
    // This cant handle the type because disciplines and types have a different order in port (electrical wire) and net delcarations (wire electrical)
    fn parse_net_declaration_end(
        &mut self,
        mut is_discipline: bool,
        opt_first_identifier_or_discipline: Option<Ident>,
    ) -> Result<(Ident, bool, Ident)> {
        let start = self.previous_span(1);
        let signed = self.look_ahead(0)?.0 == Token::Signed;

        if signed {
            self.consume(1);
            is_discipline = true;
        };

        let (name, discipline) = match opt_first_identifier_or_discipline {
            Some(discipline) if is_discipline => (self.parse_identifier()?, discipline),
            Some(first_identifier_or_discipline) => {
                if let Some(first_identifier) = self.parse_opt_identifier() {
                    (first_identifier, first_identifier_or_discipline)
                } else {
                    (
                        first_identifier_or_discipline,
                        Ident::spanned_empty(self.span_to_current_end(start)),
                    )
                }
            }
            None => (
                self.parse_identifier()?,
                Ident::spanned_empty(self.span_to_current_end(start)),
            ),
        };

        Ok((discipline, signed, name))
    }

    fn parse_net_type(&mut self) -> Result<NetType> {
        let token = self.look_ahead(0)?.0;
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
        self.consume(1);
        Ok(vtype)
    }
}
