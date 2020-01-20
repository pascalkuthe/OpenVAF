/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::{AttributeNode, Net, NetType, Port};
use crate::parser::error::Result;
use crate::parser::lexer::Token;
use crate::parser::{error, Error, Parser};
use crate::symbol::Ident;

impl<'source_map, 'ast> Parser<'source_map, 'ast> {
    pub fn parse_port_declaration_list(&mut self) -> Result<Vec<AttributeNode<'ast, Port>>> {
        if self.look_ahead()?.0 == Token::ParenClose {
            return Ok(Vec::new());
        }
        let mut start = self.look_ahead()?.1.get_start();
        let mut attributes = self.parse_attributes()?;
        let mut port = self.parse_port_declaration_base()?;
        let mut res = vec![AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: port,
        }];
        while self.look_ahead()?.0 == Token::Comma {
            while self.next()?.0 == Token::Comma {
                if let Ok(name) = self.parse_identifier(true) {
                    port.name = name;
                    res.push(AttributeNode {
                        attributes,
                        source: self.span_to_current_end(start),
                        contents: port,
                    });
                } else {
                    break;
                }
            }
            start = self.look_ahead()?.1.get_start();
            attributes = self.parse_attributes()?;
            port = self.parse_port_declaration_base()?;
            res.push(AttributeNode {
                attributes,
                source: self.span_to_current_end(start),
                contents: port,
            });
        }
        Ok(res)
    }

    pub fn parse_port_declaration(&mut self) -> Result<Vec<AttributeNode<'ast, Port>>> {
        let start = self.look_ahead()?.1.get_start();
        let port = self.parse_port_declaration_base()?;
        let attributes = self.parse_attributes()?;
        let mut res = vec![AttributeNode {
            attributes,
            source: self.span_to_current_end(start),
            contents: port,
        }];
        self.parse_list(
            |sel: &mut Self| {
                let port = Port {
                    name: sel.parse_identifier(false)?,
                    ..port
                };
                res.push(AttributeNode {
                    attributes,
                    source: sel.span_to_current_end(start),
                    contents: port,
                });
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(res)
    }

    /// this parses a port Declaration which only declares one port (for example input electrical x but not input electrical x,y)
    /// this function is a helper function to either be called from parse_port_declaration or parse_port_declaration_list which handel the extra ports declared
    fn parse_port_declaration_base(&mut self) -> Result<Port> {
        let (token, span) = self.next()?;
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
        Ok(Port {
            name,
            input,
            output,
            discipline,
            signed,
            net_type: port_type,
        })
    }

    pub fn parse_net_declaration(&mut self) -> Result<Vec<Net>> {
        let verilog_type = self.parse_net_type()?;
        let opt_first_identifier_or_discipline = self.parse_identifier(true);
        let (discipline, signed, first_name) =
            self.parse_net_declaration_end(false, opt_first_identifier_or_discipline)?;
        let mut res = vec![Net {
            name: first_name,
            discipline,
            signed,
            net_type: verilog_type,
        }];
        self.parse_list(
            |sel: &mut Self| {
                res.push(Net {
                    name: sel.parse_identifier(false)?,
                    discipline,
                    signed,
                    net_type: verilog_type,
                });
                Ok(())
            },
            Token::Semicolon,
            true,
        )?;
        Ok(res)
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
            self.lookahead.take();
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
            _ => return Ok(NetType::UNDECLARED),
        };
        self.lookahead.take();
        Ok(vtype)
    }
}
