/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use log::debug;

pub use error::Error;
pub use error::Result;

use crate::ast::{Ast, HierarchicalId};
use crate::diagnostic::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use crate::ir::{Attribute, AttributeId, Attributes};
use crate::lints::dispatch_early;
use crate::parser::error::Error::{
    AlreadyDeclaredInThisScope, MissingOrUnexpectedToken, UnexpectedToken,
};
use crate::parser::error::Expected;
use crate::parser::error::Expected::Identifier;
use crate::parser::lints::AtrributeOverwritten;
pub use crate::parser::tokenstream::Token;
use crate::parser::tokenstream::{SpannedToken, TokenStream};
use crate::sourcemap::span::DUMMY_SP;
use crate::sourcemap::SourceMap;
use crate::symbol::{keywords, Ident, Symbol};
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::util::format_list;
use crate::HashMap;
use crate::{Span, StringLiteral};
use std::sync::Arc;

pub mod tokenstream;

#[cfg(test)]
pub mod test;

#[macro_use]
mod combinators;
mod branch;
mod disciplines;
pub mod error;
mod expression;
mod lints;
mod module;
mod nature;
mod net_declarations;
mod parameter;
mod statements;
mod variables;

/// A reclusive decent Parser that parses the tokens created by the [`Preprocessor`](crate::parser::preprocessor::Preprocessor) into an [`Ast`](crate::ast::Ast).
pub(crate) struct Parser<'lt> {
    pub scope_stack: Vec<SymbolTable>,
    pub ast: &'lt mut Ast,
    pub non_critical_errors: MultiDiagnostic<Error>,
    unrecoverable: bool,
    pub src: TokenStream,
    position: usize,
}

impl<'lt> Parser<'lt> {
    pub fn new(src: TokenStream, ast: &'lt mut Ast) -> Self {
        Self {
            scope_stack: Vec::with_capacity(32),
            ast,
            non_critical_errors: MultiDiagnostic(Vec::with_capacity(32)),
            unrecoverable: false,
            src,
            position: 0,
        }
    }

    pub fn run(&mut self) {
        synchronize!(self;
            let attributes = self.parse_attributes()?;
            sync self.next() => {
                Token::Module => self.parse_module(attributes),
                Token::Nature => self.parse_nature(attributes),
                Token::Discipline => self.parse_discipline(attributes),
            }
        )
    }

    /// Returns the next token without consuming it so the next call to
    /// `look_ahead`/[`next`](crate::parser::Parser::next) will return the same token again
    #[inline(always)]
    fn look_ahead(&self, distance: usize) -> Result<SpannedToken> {
        self.src
            .get(self.position + distance)
            .copied()
            .ok_or_else(|| Error::UnexpectedEof {
                span: self
                    .src
                    .get(self.position)
                    .or_else(|| self.src.last())
                    .map_or(DUMMY_SP, |(_, span)| *span),
            })
    }

    /// Returns the next token without consuming it so the next call to
    /// `look_ahead`/[`next`](crate::parser::Parser::next) will return the same token again
    #[inline(always)]
    fn optional_look_ahead(&mut self, distance: usize) -> Option<SpannedToken> {
        self.src.get(self.position + distance).copied()
    }

    #[inline(always)]
    fn consume(&mut self, count: usize) {
        self.position += count
    }

    pub fn next(&mut self) -> Result<SpannedToken> {
        let res = self.look_ahead(0)?;
        self.consume(1);
        Ok(res)
    }

    // Currently unused might change in the future
    // pub fn optional_next(&mut self) -> Option<SpannedToken> {
    //     let res = self.optional_look_ahead(0)?;
    //     self.consume(1);
    //     Some(res)
    // }

    pub fn parse_identifier(&mut self) -> Result<Ident> {
        let (token, span) = self.next()?;

        match token {
            Token::Ident(name) => Ok(Ident::new(name, span)),
            _ => Err(Error::UnexpectedTokens {
                expected: format_list(vec![Identifier]),
                span,
            }),
        }
    }

    pub fn parse_opt_identifier(&mut self) -> Option<Ident> {
        let (token, span) = self.look_ahead(0).ok()?;

        match token {
            Token::Ident(name) => {
                self.consume(1);
                Some(Ident::new(name, span))
            }
            _ => None,
        }
    }

    /// This function parses an hieraichal identifier (See [parse_hierarchical_identifier])
    /// when the first identifier is given as `start`
    pub fn parse_hierarchical_identifier_with_start(
        &mut self,
        start: Ident,
    ) -> Result<HierarchicalId> {
        let mut names = vec![start];
        while self.look_ahead(0)?.0 == Token::Accessor {
            self.consume(1);

            names.push(self.parse_identifier()?)
        }

        Ok(HierarchicalId { names })
    }

    /// Parses and hierarchical identifier. A hieraichal Identifiers is a [normal identifier](crate::parser::Parser::parse_identifier) follow by an arbitrary amount of additional identifiers separated by a `.`
    ///
    ///
    /// # Examples
    /// This function can parse all of the following examples
    ///
    /// ```Verilog
    ///  foo
    ///  foo.bar
    ///  foo.bar.x
    ///  \Escaped.Identifier .bar
    /// ```
    pub fn parse_hierarchical_identifier(&mut self) -> Result<HierarchicalId> {
        let start = self.parse_identifier()?;
        self.parse_hierarchical_identifier_with_start(start)
    }

    /// Parses the attributes before an item. According to the Verilog-Ams standard an arbitrary amount of attribute lists may be specified for each item
    /// An attribute list is an arbitrary amount of [attributes](crate::parser::Parser::parse_attribute)  separated by `,` and wrapped inside `(*` and `*)`
    ///
    ///
    /// # Examples
    ///
    /// This function can parse all of the following examples
    ///
    /// ``` Verilog
    /// /* No attributes */
    /// (*x,y=2*) //parsed attributes: (x:0,y:2)
    /// (*x,z="test"*)(*y=2*)(*x=1,y=3*) //parsed attributes x=1
    /// ```
    ///
    pub fn parse_attributes(&mut self) -> Result<Attributes> {
        let attr_start = self.ast.attributes.len_idx();
        let mut attribute_map: HashMap<Symbol, AttributeId> = HashMap::default();
        loop {
            if self.look_ahead(0)?.0 != Token::AttributeStart {
                break;
            }
            self.consume(1);
            self.parse_list(
                |sel| sel.parse_attribute(&mut attribute_map),
                Token::AttributeEnd,
                true,
            )?;
        }
        Ok(Attributes::new(attr_start, self.ast.attributes.len_idx()))
    }

    /// Parses a single attribute of the form `name = value` (value may be any valid constant expression that doesn't reference a parameter or Literal*) inside an Attribute Lis.
    /// Specifying a value is optional so just `name` is also valid (in this case the value is 0) .
    /// If this Attribute is already defined for the current item it will be overwritten and a Warning will be generated
    fn parse_attribute(&mut self, attribute_map: &mut HashMap<Symbol, AttributeId>) -> Result {
        let (token, span) = self.look_ahead(0)?;
        let ident = if token == Token::Units {
            self.consume(1);
            Ident::new(keywords::units, span)
        } else {
            self.parse_identifier()?
        };

        let value = if self.look_ahead(0)?.0 == Token::Assign {
            self.consume(1);
            Some(self.parse_expression()?)
        } else {
            None
        };

        if let Some(id) = attribute_map.get(&ident.name) {
            let old_ident = self.ast[*id].ident;

            dispatch_early(Box::new(AtrributeOverwritten {
                name: old_ident.name,
                old: old_ident.span,
                new: ident.span,
            }))
        } else {
            let id = self.ast.attributes.push(Attribute { ident, value });
            attribute_map.insert(ident.name, id);
        }
        Ok(())
    }

    /// Tries to consume `token` returns an Unexpected Token error otherwise
    #[inline]
    pub fn expect(&mut self, token: Token) -> Result {
        let (found, span) = self.next()?;
        if found == token {
            Ok(())
        } else {
            debug!("Parser: Expected {} but found {}", token, found);
            Err(MissingOrUnexpectedToken {
                expected: format_list(vec![token]),
                expected_at: self.previous_span(2),
                span,
            })
        }
    }

    /// Tries to consume `token` returns an Unexpected Token error otherwise
    #[inline]
    pub fn expect_string(&mut self) -> Result<StringLiteral> {
        let (token, span) = self.next()?;
        match token {
            Token::LiteralString(literal) => Ok(literal),
            _ => Err(Error::UnexpectedTokens {
                expected: format_list(vec![Expected::StringLiteral]),
                span,
            }),
        }
    }

    /// Tries to consume `token` returns an Unexpected Token error otherwise
    #[inline]
    pub fn expect_int(&mut self) -> Result<u32> {
        let (token, span) = self.next()?;
        match token {
            Token::IntLiteral(literal) => Ok(literal),
            _ => Err(Error::MissingOrUnexpectedToken {
                expected: format_list(vec![Token::IntLiteral(0)]),
                expected_at: self.previous_span(2),
                span,
            }),
        }
    }

    #[inline]
    pub fn try_expect(&mut self, token: Token) {
        match self.look_ahead(0) {
            Ok((found, _)) if found == token => {
                self.consume(1);
            }

            Err(error) => {
                debug!("Parser: Tried expecting {} but reached EOF", token);
                self.consume(1);
                self.non_critical_errors.add(error)
            }

            Ok((_, span)) => {
                self.non_critical_errors.add(MissingOrUnexpectedToken {
                    expected: format_list(vec![token]),
                    expected_at: self.previous_span(1),
                    span,
                });
            }
        }
    }

    #[inline]
    pub fn expect_lookahead(&mut self, token: Token) -> Result {
        let (found, span) = self.look_ahead(0)?;
        if found == token {
            self.consume(1);
            Ok(())
        } else {
            Err(MissingOrUnexpectedToken {
                expected: format_list(vec![token]),
                expected_at: self.previous_span(1),
                span,
            })
        }
    }

    #[inline]
    pub fn span_to_current_end(&self, start: Span) -> Span {
        start.extend(self.previous_span(1))
    }

    #[inline(always)]
    pub fn previous_span(&self, len: usize) -> Span {
        if self.position < len {
            self.src[0].1
        } else {
            self.src[self.position - len].1
        }
    }

    #[inline]
    fn last_span(&self) -> Span {
        self.src.last().map_or(DUMMY_SP, |(_, span)| *span)
    }

    #[inline]
    pub fn insert_symbol(&mut self, name: Ident, declaration: SymbolDeclaration) {
        if let Some(old_declaration) = self.symbol_table_mut().insert(name.name, declaration) {
            self.non_critical_errors.add(AlreadyDeclaredInThisScope {
                declaration: name.span,
                other_declaration: old_declaration.ident(self.ast).span,
                name: name.name,
            });
        }
    }

    #[inline]
    pub fn symbol_table_mut(&mut self) -> &mut SymbolTable {
        self.scope_stack
            .last_mut()
            .unwrap_or(&mut self.ast.top_symbols)
    }

    #[inline]
    pub fn symbol_table(&self) -> &SymbolTable {
        self.scope_stack.last().unwrap_or(&self.ast.top_symbols)
    }
}

impl Ast {
    pub fn parse_from_token_stream(
        ts: TokenStream,
    ) -> std::result::Result<Self, MultiDiagnostic<Error>> {
        let mut res = Ast::new();
        let mut parser = Parser::new(ts, &mut res);
        parser.run();
        if parser.non_critical_errors.is_empty() {
            Ok(res)
        } else {
            Err(parser.non_critical_errors)
        }
    }

    pub fn parse_from_token_stream_user_facing(
        ts: TokenStream,
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<Self> {
        let res = Self::parse_from_token_stream(ts);
        res.map_err(|error| error.user_facing(&sm, expansion_disclaimer))
    }

    pub fn parse_from_token_stream_user_facing_with_printer<P: DiagnosticSlicePrinter>(
        ts: TokenStream,
        sm: &Arc<SourceMap>,
        expansion_disclaimer: &'static str,
    ) -> UserResult<Self, P> {
        let res = Self::parse_from_token_stream(ts);
        res.map_err(|error| error.user_facing(&sm, expansion_disclaimer))
    }
}
