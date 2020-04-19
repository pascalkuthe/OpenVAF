//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::ir::ast::Discipline;
use crate::ir::{AttributeNode, Attributes, Push};
use crate::parser::error::Type::{
    AttributeAlreadyDefined, DiscreteDisciplineHasNatures, UnexpectedToken,
};
use crate::parser::lexer::Token;
use crate::parser::{Error, Result};
use crate::symbol_table::SymbolDeclaration;
use crate::Parser;

impl<'lt, 'ast, 'source_map> Parser<'lt, 'ast, 'source_map> {
    pub fn parse_discipline(&mut self, attributes: Attributes<'ast>) -> Result {
        let start = self.preprocessor.current_start();
        self.consume_lookahead();
        let name = self
            .parse_identifier(false)
            .map_err(|err| self.non_critical_errors.push(err));

        self.expect(Token::Semicolon)?;

        let mut potential_nature = None;
        let mut flow_nature = None;
        let mut continuous = None;
        self.parse_and_recover_on_tokens(
            Token::Semicolon,
            Token::EndDiscipline,
            true,
            true,
            |parser, token| {
                match parser.next()? {
                    (Token::Potential, _) if potential_nature.is_none() => {
                        potential_nature = Some(parser.parse_identifier(false)?);
                    }

                    (Token::Flow, _) if flow_nature.is_none() => {
                        flow_nature = Some(parser.parse_identifier(false)?);
                    }

                    (Token::Domain, _) if continuous.is_none() => {
                        continuous = match parser.next()? {
                            (Token::Continuous, _) => Some(true),
                            (Token::Discrete, _) => Some(false),
                            (_, source) => {
                                return Err(Error {
                                    error_type: UnexpectedToken {
                                        expected: vec![Token::Discrete, Token::Continuous],
                                    },
                                    source,
                                })
                            }
                        };
                    }

                    (Token::Potential, source)
                    | (Token::Flow, source)
                    | (Token::Domain, source) => {
                        return Err(Error {
                            error_type: AttributeAlreadyDefined,
                            source,
                        })
                    }

                    (_, source) => {
                        return Err(Error {
                            error_type: UnexpectedToken {
                                expected: vec![Token::Potential, Token::Flow, Token::Domain],
                            },
                            source,
                        })
                    }
                }
                parser.expect(Token::Semicolon)?;
                Ok(())
            },
        )?;

        let source = self.span_to_current_end(start);

        let continuous = match continuous {
            Some(true) => Some(true),
            Some(false) if potential_nature.is_some() || potential_nature.is_some() => {
                self.non_critical_errors.push(Error {
                    error_type: DiscreteDisciplineHasNatures,
                    source,
                });
                return Ok(());
            }
            None if potential_nature.is_some() || potential_nature.is_some() => Some(true),
            v => v,
        };

        if let Ok(name) = name {
            let id = self.ast.push(AttributeNode {
                attributes,
                source,
                contents: Discipline {
                    name,
                    flow_nature,
                    potential_nature,
                    continuous,
                },
            });
            self.insert_symbol(name, SymbolDeclaration::Discipline(id))
        }

        Ok(())
    }
}
