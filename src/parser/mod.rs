/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use std::path::Path;

use ahash::AHashMap;
use ansi_term::Color::*;
use bumpalo::Bump;
use log::error;

pub use error::Error;
pub use error::Result;

use crate::ast::{Ast, Attributes, HierarchicalId};
use crate::compact_arena::SafeRange;
use crate::ir::ast::{Attribute, AttributeNode, Discipline, Nature};
use crate::ir::AttributeId;
use crate::parser::error::{Expected, Type};
use crate::parser::lexer::Token;
use crate::span::Index;
use crate::symbol::{Ident, Symbol};
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::{Push, SafeRangeCreation};
use crate::{Preprocessor, SourceMap, Span};

pub(crate) mod lexer;
pub(crate) mod preprocessor;
#[cfg(test)]
pub mod test;

mod behavior;
mod branch;
mod combinators;
pub mod error;
mod expression;
mod module;
mod net_declarations;
mod primaries;
mod variables;

pub struct Parser<'lt, 'ast, 'source_map> {
    pub preprocessor: Preprocessor<'lt, 'source_map>,
    pub scope_stack: Vec<SymbolTable<'ast>>,
    lookahead: Option<Result<(Token, Span)>>,
    pub ast: &'lt mut Ast<'ast>,
    pub non_critical_errors: Vec<Error>,
}
impl<'lt, 'ast, 'source_map> Parser<'lt, 'ast, 'source_map> {
    pub fn new(
        preprocessor: Preprocessor<'lt, 'source_map>,
        ast: &'lt mut Ast<'ast>,
        errors: Vec<Error>,
    ) -> Self {
        Self {
            preprocessor,
            scope_stack: Vec::with_capacity(32),
            lookahead: None,
            ast,
            non_critical_errors: errors,
        }
    }
    fn next(&mut self) -> Result<(Token, Span)> {
        match self.lookahead.take() {
            None => {
                self.preprocessor.advance()?;
                Ok((self.preprocessor.current_token(), self.preprocessor.span()))
            }
            Some(res) => res,
        }
    }
    fn look_ahead(&mut self) -> Result<(Token, Span)> {
        if let Some(ref lookahead) = self.lookahead {
            return lookahead.clone();
        }
        let res = self
            .preprocessor
            .advance()
            .map(|_| (self.preprocessor.current_token(), self.preprocessor.span()));
        self.lookahead = Some(res.clone());
        res
    }
    pub fn run(&mut self) {
        loop {
            let error = match self.parse_attributes() {
                Ok(attributes) => match self.next() {
                    Ok((token, source)) => match token {
                        Token::EOF => return,
                        Token::Module => {
                            if let Err(error) = self.parse_module(attributes) {
                                error
                            } else {
                                continue;
                            }
                        }

                        _ => Error {
                            error_type: error::Type::UnexpectedToken {
                                expected: vec![Token::Module],
                            },
                            source,
                        },
                    },
                    Err(error) => error,
                },
                Err(error) => error,
            }; //we can sadly not use Result::and_then altough thats exactly what this is for because it doesn't allow return which is needed here
            self.non_critical_errors.push(error);
            loop {
                match self.look_ahead() {
                    Ok((Token::Module, _)) => break,
                    Ok((Token::EOF, _)) => return,
                    Ok(_) => {
                        self.lookahead.take();
                    }
                    Err(error) => {
                        self.lookahead.take();
                        self.non_critical_errors.push(error);
                    }
                }
            }
        }
    }
    pub fn parse_identifier(&mut self, optional: bool) -> Result<Ident> {
        let (token, source) = if optional {
            self.look_ahead()?
        } else {
            self.next()?
        };
        let identifier = match token {
            Token::SimpleIdentifier => self.preprocessor.slice(),
            Token::EscapedIdentifier => {
                let raw = self.preprocessor.slice();
                &raw[1..raw.len() - 1]
            }
            _ => {
                return Err(Error {
                    source,
                    error_type: error::Type::UnexpectedTokens {
                        expected: vec![Expected::Identifier],
                    },
                })
            }
        };
        if optional {
            self.lookahead.take();
        }
        Ok(Ident::from_str_and_span(identifier, source))
    }
    pub fn parse_hierarchical_identifier(&mut self, optional: bool) -> Result<HierarchicalId> {
        Ok(HierarchicalId {
            names: self.parse_hierarchical_identifier_internal(optional)?,
        })
    }
    pub fn parse_hierarchical_identifier_internal(&mut self, optional: bool) -> Result<Vec<Ident>> {
        let mut identifier = vec![self.parse_identifier(optional)?];
        while self.look_ahead()?.0 == Token::Accessor {
            self.lookahead.take();
            identifier.push(self.parse_identifier(false)?)
        }
        Ok(identifier)
    }
    //todo attributes
    pub fn parse_attributes(&mut self) -> Result<Attributes<'ast>> {
        let attributes = self.ast.empty_range_from_end();
        let mut attribute_map: AHashMap<Symbol, AttributeId<'ast>> = AHashMap::new();
        loop {
            if self.look_ahead()?.0 != Token::AttributeStart {
                break;
            }
            self.lookahead.take();
            self.parse_list(
                |sel| sel.parse_attribute(&mut attribute_map),
                Token::AttributeEnd,
                true,
            )?;
        }
        Ok(self.ast.extend_range_to_end(attributes))
    }
    fn parse_attribute(
        &mut self,
        attribute_map: &mut AHashMap<Symbol, AttributeId<'ast>>,
    ) -> Result {
        let range: SafeRange<AttributeId<'ast>> = self.ast.empty_range_from_end();
        let name = self.parse_identifier(false)?;
        let value = if self.look_ahead()?.0 == Token::Assign {
            self.lookahead.take();
            Some(self.parse_expression_id()?)
        } else {
            None
        };
        if let Some(id) = attribute_map.get(&name.name) {
            self.ast[*id] = Attribute { name, value };
        //TODO warn
        } else {
            let id = self.ast.push(Attribute { name, value });
            attribute_map.insert(name.name, id);
        }
        /*let range = self.ast.extend_range_to_end(range);
        self.ast[range].sort_unstable_by(|attribute1, attribute2| {
            attribute1
                .name
                .name
                .as_u32()
                .cmp(&attribute2.name.name.as_u32())
        });
        sorting probably not worth it since attributes are never used in large quantaties where a sorted algoritehm would be faster
        */
        Ok(())
    }
    pub fn expect(&mut self, token: Token) -> Result {
        let (found, source) = self.look_ahead()?;
        if found != token {
            Err(Error {
                source,
                error_type: error::Type::UnexpectedToken {
                    expected: vec![token],
                },
            })
        } else {
            self.lookahead.take();
            Ok(())
        }
    }
    pub fn span_to_current_end(&self, start: Index) -> Span {
        Span::new(start, self.preprocessor.current_end())
    }
    #[inline]
    pub fn insert_symbol(&mut self, name: Ident, declaration: SymbolDeclaration<'ast>) {
        let source = declaration.span(&self.ast);
        if let Some(old_declaration) = self.symbol_table_mut().insert(name.name, declaration) {
            self.non_critical_errors.push(Error {
                error_type: Type::AlreadyDeclaredInThisScope {
                    other_declaration: old_declaration.span(&self.ast),
                    name: name.name,
                },
                source,
            });
        }
    }
    pub fn symbol_table_mut(&mut self) -> &mut SymbolTable<'ast> {
        self.scope_stack
            .last_mut()
            .unwrap_or(&mut self.ast.top_symbols)
    }
    pub fn symbol_table(&self) -> &SymbolTable<'ast> {
        self.scope_stack.last().unwrap_or(&self.ast.top_symbols)
    }
}
pub fn parse<'source_map, 'ast, 'lt>(
    main_file: &Path,
    source_map_allocator: &'source_map Bump,
    ast: &'lt mut Ast<'ast>,
) -> std::io::Result<(&'source_map SourceMap<'source_map>, Vec<Error>)> {
    let allocator = Bump::new();
    let mut preprocessor = Preprocessor::new(&allocator, source_map_allocator, main_file)?;
    let mut errors = Vec::with_capacity(64);
    while let Err(error) = preprocessor.process_token() {
        errors.push(error)
    }
    let mut parser = Parser::new(preprocessor, ast, errors);
    parser.lookahead = Some(Ok((
        parser.preprocessor.current_token(),
        parser.preprocessor.span(),
    )));
    parser.run();
    Ok((parser.preprocessor.skip_rest(), parser.non_critical_errors))
}

pub fn parse_and_print_errors<'source_map, 'ast, 'lt>(
    main_file: &Path,
    source_map_allocator: &'source_map Bump,
    ast: &'lt mut Ast<'ast>,
    translate_lines: bool,
) -> std::result::Result<&'source_map SourceMap<'source_map>, ()> {
    match parse(main_file, source_map_allocator, ast) {
        Ok((source_map, errors)) if errors.is_empty() => Ok(source_map),
        Ok((source_map, mut errors)) => {
            errors
                .drain(..)
                .for_each(|err| err.print(&source_map, translate_lines));
            Err(())
        }
        Err(error) => {
            error!(
                "{} {}",
                Red.bold().paint("error"),
                Fixed(253).bold().paint(format!(
                    ": failed to open {}: {}!",
                    main_file.to_str().unwrap(),
                    error
                ))
            );
            Err(())
        }
    }
}
pub fn insert_electrical_natures_and_disciplines(ast: &mut Ast) {
    let voltage = ast.push(AttributeNode {
        attributes: ast.empty_range_from_end(),
        source: Span::new(0, 0),
        contents: Nature {
            name: Ident::from_str("Voltage"),
        },
    });
    ast.top_symbols
        .insert(Symbol::intern("V"), SymbolDeclaration::Nature(voltage));
    let current = ast.push(AttributeNode {
        attributes: ast.empty_range_from_end(),
        source: Span::new(0, 0),
        contents: Nature {
            name: Ident::from_str("Current"),
        },
    });
    ast.top_symbols
        .insert(Symbol::intern("I"), SymbolDeclaration::Nature(current));
    let electrical = ast.push(AttributeNode {
        attributes: ast.empty_range_from_end(),
        source: Span::new(0, 0),
        contents: Discipline {
            name: Ident::from_str("electrical"),
            flow_nature: Ident::from_str("I"),
            potential_nature: Ident::from_str("V"),
        },
    });
    let charge = ast.push(AttributeNode {
        attributes: ast.empty_range_from_end(),
        source: Span::new(0, 0),
        contents: Nature {
            name: Ident::from_str("Charge"),
        },
    });
    ast.top_symbols
        .insert(Symbol::intern("Q"), SymbolDeclaration::Nature(charge));
    ast.top_symbols.insert(
        Symbol::intern("electrical"),
        SymbolDeclaration::Discipline(electrical),
    );
}
