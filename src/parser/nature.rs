//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::ir::ast::Nature;
use crate::ir::{AttributeNode, Attributes};
use crate::parser::error::Result;
use crate::parser::error::Type::{
    AttributeAlreadyDefined, RequiredAttributeNotDefined, UnexpectedToken,
};
use crate::parser::lexer::Token;
use crate::parser::Error;
use crate::symbol::keywords;
use crate::symbol_table::SymbolDeclaration;
use crate::Parser;

impl<'lt, 'source_map> Parser<'lt, 'source_map> {
    pub fn parse_nature(&mut self, attributes: Attributes) -> Result {
        self.consume_lookahead();
        let start = self.preprocessor.current_start();

        let name = self
            .parse_identifier(false)
            .map_err(|err| self.non_critical_errors.push(err));

        /* derived attributes are unsupported for now
        let parent = match self.parse_parent(){
            Err(error) => {
                self.non_critical_errors.push(error);
                self.recover_on(Token::EndNature,Token::Semicolon,false,false)?;
                None
            }
            Ok(res) => res,
        };*/

        self.expect(Token::Semicolon)?;
        let mut abstol = None;
        let mut units = None;
        let mut access = None;
        let mut idt_nature = None;
        let mut ddt_nature = None;
        // user defined attributes are unsupported for now
        self.parse_and_recover_on_tokens(
            Token::Semicolon,
            Token::EndNature,
            true,
            true,
            |parser| {
                let (token, source) = parser.next_with_span()?;
                match token {
                    Token::Abstol if abstol.is_none() => {
                        parser.expect(Token::Assign)?;
                        abstol = Some(parser.parse_expression_id()?);
                    }

                    Token::Access if access.is_none() => {
                        parser.expect(Token::Assign)?;
                        access = Some(parser.parse_identifier(false)?);
                    }

                    Token::TimeIntegralNature if idt_nature.is_none() => {
                        parser.expect(Token::Assign)?;
                        idt_nature = Some(parser.parse_identifier(false)?);
                    }

                    Token::TimeDerivativeNature if ddt_nature.is_none() => {
                        parser.expect(Token::Assign)?;
                        ddt_nature = Some(parser.parse_identifier(false)?);
                    }

                    Token::Units if units.is_none() => {
                        parser.expect(Token::Assign)?;
                        units = Some(parser.parse_expression_id()?);
                    }

                    Token::Abstol
                    | Token::Access
                    | Token::TimeIntegralNature
                    | Token::TimeDerivativeNature
                    | Token::Units => {
                        return Err(Error {
                            error_type: AttributeAlreadyDefined,
                            source: parser.preprocessor.span(),
                        })
                    }

                    _ => {
                        return Err(Error {
                            error_type: UnexpectedToken {
                                expected: vec![
                                    Token::Abstol,
                                    Token::Access,
                                    Token::TimeIntegralNature,
                                    Token::TimeDerivativeNature,
                                    Token::Units,
                                ],
                            },
                            source,
                        })
                    }
                }
                parser.try_expect(Token::Semicolon);
                Ok(true)
            },
        )?;

        let source = self.span_to_current_end(start);

        let mut missing = Vec::new();
        if abstol.is_none() {
            missing.push(keywords::ABSTOL)
        }

        if units.is_none() {
            missing.push(keywords::UNITS)
        }

        if access.is_none() {
            missing.push(keywords::ACCESS)
        }

        if missing.is_empty() {
            if let Ok(name) = name {
                let access = access.unwrap();
                let id = self.ast.natures.push(AttributeNode {
                    attributes,
                    source,
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
            self.non_critical_errors.push(Error {
                error_type: RequiredAttributeNotDefined(missing),
                source,
            })
        }

        Ok(())
    }
    /* Nature inheritance is unsupported for now (I don't think anyone has used that ever and its a lot of work)
    fn parse_parent(&mut self)->Result<Option<(Ident,NatureParentType)>>{
        let res = if self.look_ahead()?.0 == Token::Colon{
            let ident = self.parse_identifier(false)?;
            let ptype = if self.look_ahead()?.0 == Token::Accessor{
                self.consume_lookahead();
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
