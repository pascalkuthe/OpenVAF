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

pub use error::Error;
pub use error::Result;

use crate::ast::{AttributeNode, Attributes, HierarchicalId, TopNode};
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
mod expression;
mod module;
mod net_declarations;
mod primaries;
mod variables;
//mod combinators;
pub mod error;

#[derive(Debug)]
pub struct Parser<'source_map, 'ast> {
    pub preprocessor: Preprocessor<'source_map>,
    pub scope_stack: Vec<SymbolTable<'ast>>,
    lookahead: Option<Result<(Token, Span)>>,
    pub ast_allocator: &'ast Bump,
}
impl<'source_map, 'ast> Parser<'source_map, 'ast> {
    pub fn new(preprocessor: Preprocessor<'source_map>, ast_allocator: &'ast Bump) -> Self {
        Self {
            preprocessor,
            scope_stack: vec![SymbolTable::new()],
            lookahead: None,
            ast_allocator,
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
    pub fn run(&mut self) -> Result<AstTop<'ast>> {
        let mut top_nodes = Vec::new(); //we allocate on the heap and copy into the ast_allocator later because other things might be allocated on the allocator in between causing a temporary leak until the allocator is freed
        loop {
            match self.next()? {
                //TODO multierror
                (Token::EOF, _) => break,
                (Token::Module, _) => {
                    let start = self.preprocessor.current_start();
                    let attributes = self.parse_attributes()?;
                    let module = self.parse_module()?;
                    let source = self.span_to_current_end(start);
                    top_nodes.push(AttributeNode {
                        attributes,
                        source,
                        contents: TopNode::Module(module),
                    });
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
        Ok(self.ast_allocator.alloc_slice_copy(top_nodes.as_slice()))
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
    pub fn parse_hierarchical_identifier(
        &mut self,
        optional: bool,
    ) -> Result<HierarchicalId<'ast>> {
        Ok(HierarchicalId {
            names: self
                .parse_hierarchical_identifier_internal(optional)?
                .into_bump_slice(),
        })
    }
    pub fn parse_hierarchical_identifier_internal(
        &mut self,
        optional: bool,
    ) -> Result<bumpalo::collections::Vec<'ast, Ident>> {
        let mut identifier = bumpalo::vec![in &self.ast_allocator;self.parse_identifier(optional)?];
        while self.look_ahead()?.0 == Token::Accessor {
            self.lookahead.take();
            identifier.push(self.parse_identifier(false)?)
        }
        Ok(identifier)
    }
    //todo attributes
    pub fn parse_attributes(&mut self) -> Result<Attributes<'ast>> {
        Ok(unsafe {
            use std::ptr::NonNull;
            &*(NonNull::<[(); 0]>::dangling() as NonNull<[()]>).as_ptr()
        }) //Attributes are not yet supported
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
    pub fn insert_symbol(&mut self, name: Ident, declaration: SymbolDeclaration<'ast>) -> Result {
        let symbol_table = self.scope_stack.last_mut().unwrap();
        let source = declaration.span();
        if let Some(old_declaration) = symbol_table.insert(name.name, declaration) {
            Err(Error {
                error_type: Type::AlreadyDeclaredInThisScope {
                    other_declaration: old_declaration.span(),
                    name: name.name,
                },
                source,
            })
        } else {
            Ok(())
        }
    }
}
pub type AstTop<'ast> = &'ast [AttributeNode<'ast, TopNode<'ast>>];
pub fn parse<'source_map, 'ast>(
    main_file: &Path,
    source_map_allocator: &'source_map Bump,
    ast_allocator: &'ast Bump,
) -> std::io::Result<(
    Box<SourceMap<'source_map>>,
    Result<(AstTop<'ast>, SymbolTable<'ast>)>,
)> {
    let mut preprocessor = Preprocessor::new(source_map_allocator, main_file)?;
    let res = preprocessor.process_token();
    let mut parser = Parser::new(preprocessor, ast_allocator);
    parser.lookahead = Some(Ok((
        parser.preprocessor.current_token(),
        parser.preprocessor.current_span(),
    )));
    let res = res
        .and_then(|_| parser.run())
        .map(|ast| (ast, parser.scope_stack.pop().unwrap()));
    Ok((parser.preprocessor.done(), res))
}
