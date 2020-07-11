//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::ir::ast::Discipline;
use crate::ir::{AttributeNode, Attributes};
use crate::parser::Error::{
    AttributeAlreadyDefined, DiscreteDisciplineHasNatures, UnexpectedToken,
};
use crate::parser::Result;
use crate::parser::Token;
use crate::symbol::keywords;
use crate::symbol_table::SymbolDeclaration;
use crate::util::format_list;
use crate::Parser;

impl<'lt> Parser<'lt> {
    pub fn parse_discipline(&mut self, attributes: Attributes) -> Result {
        let start = self.previous_span(1);

        let name = self
            .parse_identifier()
            .map_err(|err| self.non_critical_errors.add(err));

        self.try_expect(Token::Semicolon);

        let mut potential_nature = None;
        let mut flow_nature = None;
        let mut continuous = None;
        self.parse_and_recover_on_tokens(
            |token| token == Token::Semicolon,
            Token::EndDiscipline,
            true,
            true,
            |parser| {
                match parser.next()? {
                    (Token::Potential, _) => {
                        let new = parser.parse_identifier()?;
                        match potential_nature {
                            None => potential_nature = Some(new),
                            Some(old) => parser.non_critical_errors.add(AttributeAlreadyDefined {
                                name: keywords::potential,
                                old: old.span,
                                new: new.span,
                            }),
                        }
                    }

                    (Token::Flow, _) => {
                        let new = parser.parse_identifier()?;
                        match flow_nature {
                            None => flow_nature = Some(new),
                            Some(old) => parser.non_critical_errors.add(AttributeAlreadyDefined {
                                name: keywords::flow,
                                old: old.span,
                                new: new.span,
                            }),
                        }
                    }

                    (Token::Domain, _) => {
                        let (token, span) = parser.next()?;
                        let new = match token {
                            Token::Continuous => true,
                            Token::Discrete => false,
                            _ => {
                                return Err(UnexpectedToken {
                                    expected: format_list(vec![Token::Discrete, Token::Continuous]),
                                    span,
                                })
                            }
                        };

                        match continuous {
                            None => continuous = Some((new, span)),
                            Some((_, old_span)) => {
                                parser.non_critical_errors.add(AttributeAlreadyDefined {
                                    name: keywords::domain,
                                    old: old_span,
                                    new: span,
                                })
                            }
                        }
                    }

                    (_, span) => {
                        return Err(UnexpectedToken {
                            expected: format_list(vec![Token::Discrete, Token::Continuous]),
                            span,
                        })
                    }
                }
                parser.expect(Token::Semicolon)?;
                Ok(true)
            },
        )?;

        let span = self.span_to_current_end(start);

        let continuous = match continuous {
            Some((true, _)) => Some(true),
            Some((false, discrete_span)) => match (potential_nature, flow_nature) {
                (Some(pot), flow) => {
                    self.non_critical_errors.add(DiscreteDisciplineHasNatures {
                        span,
                        discrete_declaration: discrete_span,
                        first_nature: pot.span,
                        second_nature: flow.map(|ident| ident.span),
                    });

                    return Ok(());
                }
                (None, Some(flow)) => {
                    self.non_critical_errors.add(DiscreteDisciplineHasNatures {
                        span,
                        discrete_declaration: discrete_span,
                        first_nature: flow.span,
                        second_nature: None,
                    });
                    return Ok(());
                }
                (None, None) => Some(false),
            },

            None if potential_nature.is_some() || potential_nature.is_some() => Some(true),
            c => c.map(|(c, _)| c),
        };

        if let Ok(name) = name {
            let id = self.ast.disciplines.push(AttributeNode {
                attributes,
                span,
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
