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

use sr_alloc::{Allocator, SliceId, StrId};

pub use error::Error;
pub use error::Result;

use crate::ast::{Ast, AttributeNode, Attributes, TopNode};
use crate::parser::error::Expected;
use crate::parser::error::List::Identifier;
use crate::parser::lexer::Token;
use crate::span::Index;
use crate::{Preprocessor, SourceMap, Span};

pub(crate) mod lexer;
pub(crate) mod preprocessor;

mod module;
mod primaries;
mod variables;
//mod combinators;
pub mod error;

pub struct Parser {
    pub preprocessor: Preprocessor,
    pub lookahead: Option<Result<(Token, Span)>>,
    pub ast_allocator: Allocator,
}
impl Parser {
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
        if let Some(lookahead) = self.lookahead.take() {
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
    pub fn run(&mut self) -> Result<SliceId<AttributeNode<TopNode>>> {
        let mut top_nodes = Vec::new();
        loop {
            match self.next()? {
                //TODO multierror
                (Token::EOF, _) => break,
                (Token::Module, span) => {
                    let start = self.preprocessor.current_start();
                    let attributes = self.parse_attributes()?;
                    let module = self.parse_module()?;
                    let span = self.span_to_current_end(start);
                    top_nodes.push(AttributeNode::new(
                        span,
                        attributes,
                        TopNode::Module(module),
                    ));
                }
                (_, span) => {
                    return self.preprocessor.token_error(error::Type::UnexpectedToken {
                        expected: vec![Token::Module],
                    })
                }
            }
        }
        Ok(self.ast_allocator.alloc_slice_clone(top_nodes.as_slice()))
    }
    pub fn parse_identifier(&mut self, optional: bool) -> Result<StrId> {
        let (token, span) = if optional {
            self.look_ahead()?
        } else {
            self.next()?
        };
        let identifier = match self.next()? {
            (Token::SimpleIdentifier, _) => self.preprocessor.slice(),
            (Token::EscapedIdentifier, _) => {
                let raw = self.preprocessor.slice();
                &raw[1..raw.len() - 1]
            }
            (_, source) => {
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
        Ok(self.ast_allocator.alloc_str_copy(identifier))
    }
    //todo attributes
    pub fn parse_attributes(&mut self) -> Result<Attributes> {
        Ok(SliceId::dangling()) //Attributes are not yet supported
    }
    pub fn expect(&mut self, token: Token) -> Result {
        let (found, source) = self.next()?;
        if found != token {
            Err(Error {
                source,
                error_type: error::Type::UnexpectedToken {
                    expected: vec![token],
                },
            })
        } else {
            Ok(())
        }
    }
    pub fn span_to_current_end(&self, start: Index) -> Span {
        Span::new(start, self.preprocessor.current_end())
    }
}

pub fn parse(main_file: &Path) -> std::io::Result<(Box<SourceMap>, Result<Ast>)> {
    let mut preprocessor = Preprocessor::new(main_file)?;
    let res = preprocessor.process_token();
    let mut parser = Parser {
        preprocessor,
        lookahead: None,
        ast_allocator: Allocator::new(),
    };
    let res = parser.run();
    let mut preprocessor = parser.preprocessor;
    if let Err(error) = res {
        while preprocessor.current_token() != Token::EOF {
            //this is here to complete the sourcemap. This wont be needed when multierrors are introducted
            preprocessor.advance();
        }
        Ok((preprocessor.done(), Err(error)))
    } else {
        let ast = Ast::new(parser.ast_allocator, res.unwrap());
        Ok((preprocessor.done(), Ok(ast)))
    }
}
