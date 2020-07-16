//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

#![allow(clippy::similar_names)]

use crate::ir::ast::Nature;
use crate::ir::{AttributeNode, Attributes, ExpressionId};
use crate::parser::error::Result;
use crate::parser::Error::{AttributeAlreadyDefined, RequiredAttributeNotDefined, UnexpectedToken};
use crate::parser::Parser;
use crate::parser::Token;
use crate::symbol::keywords;
use crate::symbol_table::SymbolDeclaration;
use crate::util::{format_list, ListFormatter};

impl<'lt> Parser<'lt> {
    #[allow(clippy::too_many_lines)]
    pub fn parse_nature(&mut self, attributes: Attributes) -> Result {
        let start = self.previous_span(1);
        let name = self
            .parse_identifier()
            .map_err(|err| self.non_critical_errors.add(err));

        /* derived attributes are unsupported for now
        let parent = match self.parse_parent(){
            Err(error) => {
                self.non_critical_errors.add(error);
                self.recover_on(Token::EndNature,Token::Semicolon,false,false)?;
                None
            }
            Ok(res) => res,
        };*/

        self.try_expect(Token::Semicolon);
        let mut abstol: Option<ExpressionId> = None;
        let mut units: Option<ExpressionId> = None;
        let mut access = None;
        let mut idt_nature = None;
        let mut ddt_nature = None;
        // user defined attributes are unsupported for now
        self.parse_and_recover_on_tokens(
            |token| token == Token::Semicolon,
            Token::EndNature,
            true,
            true,
            |parser| {
                let (token, span) = parser.next()?;
                match token {
                    Token::Abstol => {
                        parser.expect(Token::Assign)?;
                        let val = parser.parse_expression()?;
                        match abstol {
                            Some(old) => {
                                return Err(AttributeAlreadyDefined {
                                    name: keywords::abstol,
                                    old: parser.ast[old].span,
                                    new: parser.ast[val].span,
                                })
                            }
                            None => abstol = Some(val),
                        }
                    }

                    Token::Access => {
                        parser.expect(Token::Assign)?;

                        let ident = parser.parse_identifier()?;
                        match access {
                            None => access = Some(ident),
                            Some(old) => {
                                return Err(AttributeAlreadyDefined {
                                    name: keywords::access,
                                    old: old.span,
                                    new: ident.span,
                                })
                            }
                        }
                    }

                    Token::TimeIntegralNature => {
                        parser.expect(Token::Assign)?;

                        let ident = parser.parse_identifier()?;
                        match idt_nature {
                            None => idt_nature = Some(ident),
                            Some(old) => {
                                return Err(AttributeAlreadyDefined {
                                    name: keywords::idt_nature,
                                    old: old.span,
                                    new: ident.span,
                                })
                            }
                        }
                    }

                    Token::TimeDerivativeNature => {
                        parser.expect(Token::Assign)?;

                        let ident = parser.parse_identifier()?;
                        match ddt_nature {
                            None => ddt_nature = Some(ident),
                            Some(old) => {
                                return Err(AttributeAlreadyDefined {
                                    name: keywords::ddt_nature,
                                    old: old.span,
                                    new: ident.span,
                                })
                            }
                        }
                    }

                    Token::Units if units.is_none() => {
                        parser.expect(Token::Assign)?;
                        let val = parser.parse_expression()?;
                        match units {
                            Some(old) => {
                                return Err(AttributeAlreadyDefined {
                                    name: keywords::units,
                                    old: parser.ast[old].span,
                                    new: parser.ast[val].span,
                                })
                            }
                            None => units = Some(val),
                        }
                    }

                    _ => {
                        return Err(UnexpectedToken {
                            expected: format_list(vec![
                                Token::Abstol,
                                Token::Access,
                                Token::TimeIntegralNature,
                                Token::TimeDerivativeNature,
                                Token::Units,
                            ]),
                            span,
                        })
                    }
                }
                parser.try_expect(Token::Semicolon);
                Ok(true)
            },
        )?;

        let span = self.span_to_current_end(start);

        let mut missing = Vec::new();
        if abstol.is_none() {
            missing.push(keywords::abstol)
        }

        if units.is_none() {
            missing.push(keywords::units)
        }

        if access.is_none() {
            missing.push(keywords::access)
        }

        if missing.is_empty() {
            if let Ok(name) = name {
                let access = access.unwrap();
                let id = self.ast.natures.push(AttributeNode {
                    attributes,
                    span,
                    contents: Nature {
                        name,
                        abstol: abstol.unwrap(),
                        units: units.unwrap(),
                        access,
                        idt_nature,
                        ddt_nature,
                    },
                });
                self.insert_symbol(name, SymbolDeclaration::Nature(id));
                self.insert_symbol(access, SymbolDeclaration::Nature(id));
            }
        } else {
            self.non_critical_errors.add(RequiredAttributeNotDefined(
                ListFormatter(missing, "", " and "),
                span,
            ))
        }

        Ok(())
    }

    /* Nature inheritance is unsupported for now (I don't think anyone has used that ever and its a lot of work)
    fn parse_parent(&mut self)->Result<Option<(Ident,NatureParentType)>>{
        let res = if self.look_ahead(0)?.0 == Token::Colon{
            let ident = self.parse_identifier(self.next()?)?;
            let ptype = if self.look_ahead(0)?.0 == Token::Accessor{

                match self.next()?{
                    (Token::Potential,_) => NatureParentType::DisciplinePotential,
                    (Token::Flow,_) => NatureParentType::DisciplineFlow,
                    (_,source) => return Err(Error{
                        error_type: UnexpectedToken {expected:vec![Token::Potential,Token::Flow]},
                        source
                    })
                }
            }else{
                NatureParentType::Nature
            };
            Some((ident,ptype))
        }else{
            None
        };
        Ok(res)
    }*/
}
