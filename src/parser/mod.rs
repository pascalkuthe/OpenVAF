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

use bumpalo::Bump;
use copyless::VecHelper;

pub use error::Error;
pub use error::Result;

use crate::ast::{Ast, Attributes, CopyRange, HierarchicalId, TopNode};
use crate::parser::error::{Expected, Type};
use crate::parser::lexer::Token;
use crate::span::Index;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
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

pub struct Parser<'lt, 'ast, 'astref, 'source_map> {
    pub preprocessor: Preprocessor<'lt, 'source_map>,
    pub scope_stack: Vec<SymbolTable<'ast>>,
    lookahead: Option<Result<(Token, Span)>>,
    pub ast: &'astref mut Ast<'ast>,
    pub non_critical_errors: Vec<Error>,
}
impl<'lt, 'ast, 'astref, 'source_map> Parser<'lt, 'ast, 'astref, 'source_map> {
    pub fn new(preprocessor: Preprocessor<'lt, 'source_map>, ast: &'astref mut Ast<'ast>) -> Self {
        Self {
            preprocessor,
            scope_stack: Vec::with_capacity(32),
            lookahead: None,
            ast,
            non_critical_errors: Vec::with_capacity(32),
        }
    }
    fn next(&mut self) -> Result<(Token, Span)> {
        self.lookahead.take().unwrap_or_else(|| {
            self.preprocessor.advance().map(|_| {
                (
                    self.preprocessor.current_token(),
                    self.preprocessor.current_span(),
                )
            })
        })
    }
    fn look_ahead(&mut self) -> Result<(Token, Span)> {
        if let Some(lookahead) = self.lookahead.clone() {
            return lookahead;
        }
        let res = self.preprocessor.advance().map(|_| {
            (
                self.preprocessor.current_token(),
                self.preprocessor.current_span(),
            )
        });
        self.lookahead = Some(res.clone());
        res
    }
    pub fn run(&mut self) -> Result {
        loop {
            let attributes = self.parse_attributes()?;
            match self.next()? {
                //TODO multierror
                (Token::EOF, _) => break,
                (Token::Module, _) => {
                    let module = self.parse_module(attributes)?;
                    self.ast.top_nodes.alloc().init(TopNode::Module(module));
                }
                (_, source) => {
                    return Err(Error {
                        error_type: error::Type::UnexpectedToken {
                            expected: vec![Token::Module],
                        },
                        source,
                    })
                }
            }
        }
        Ok(())
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
        Ok(None) //Attributes are not yet supported
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
    pub fn symbol_table(&mut self) -> &SymbolTable<'ast> {
        self.scope_stack.last().unwrap_or(&self.ast.top_symbols)
    }
}
pub fn parse<'source_map, 'ast, 'astref>(
    main_file: &Path,
    source_map_allocator: &'source_map Bump,
    ast: &'astref mut Ast<'ast>,
) -> std::io::Result<(&'source_map SourceMap<'source_map>, Vec<Error>)> {
    let allocator = Bump::new();
    let mut preprocessor = Preprocessor::new(&allocator, source_map_allocator, main_file)?;
    let res = preprocessor.process_token();
    let mut parser = Parser::new(preprocessor, ast);
    parser.lookahead = Some(Ok((
        parser.preprocessor.current_token(),
        parser.preprocessor.current_span(),
    )));
    let res = res.and_then(|_| parser.run());
    res.map_err(|err| parser.non_critical_errors.push(err));
    Ok((parser.preprocessor.skip_rest(), parser.non_critical_errors))
}
pub fn parse_and_print_errors<'source_map, 'ast, 'astref>(
    main_file: &Path,
    source_map_allocator: &'source_map Bump,
    ast: &'astref mut Ast<'ast>,
) -> std::result::Result<(), ()> {
    let (source_map, mut errors) = parse(main_file, source_map_allocator, ast)
        .expect(&format!("{} not found!", main_file.to_str().unwrap()));
    if errors.len() > 0 {
        errors
            .drain(..)
            .for_each(|err| err.print(&source_map, true));
        Err(())
    } else {
        Ok(())
    }
}
