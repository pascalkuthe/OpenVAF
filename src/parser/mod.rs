/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use std::path::Path;

use bumpalo::Bump;
use log::*;
use yansi_term::Color::*;

pub use error::Error;
pub use error::Result;

use crate::ast::{Ast, HierarchicalId};
use crate::ir::{Attribute, AttributeId, Attributes};
use crate::parser::error::{Expected, Type, Warning, WarningType};
use crate::parser::lexer::Token;
use crate::span::Index;
use crate::symbol::{keywords, Ident, Symbol};
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::{Preprocessor, SourceMap, Span};
use rustc_hash::FxHashMap;
use std::fmt::Display;

pub(crate) mod lexer;
pub(crate) mod preprocessor;
#[cfg(test)]
pub mod test;

#[macro_use]
mod combinators;
mod behavior;
mod branch;
mod disciplines;
pub mod error;
mod expression;
mod module;
mod nature;
mod net_declarations;
mod parameter;
mod primaries;
mod variables;

/// A reclusive decent Parser that parses the tokens created by the [`Preprocessor`](crate::parser::preprocessor::Preprocessor) into an [`Ast`](crate::ast::Ast).
pub(crate) struct Parser<'lt, 'source_map> {
    pub preprocessor: Preprocessor<'lt, 'source_map>,
    pub scope_stack: Vec<SymbolTable>,
    lookahead: Option<Result<(Token, Span, Index)>>,
    pub ast: &'lt mut Ast,
    pub non_critical_errors: Vec<Error>,
    pub warnings: Vec<Warning>,
    pub(crate) unrecoverable: bool,
}

impl<'lt, 'source_map> Parser<'lt, 'source_map> {
    pub fn new(
        preprocessor: Preprocessor<'lt, 'source_map>,
        ast: &'lt mut Ast,
        errors: Vec<Error>,
    ) -> Self {
        Self {
            preprocessor,
            scope_stack: Vec::with_capacity(32),
            lookahead: None,
            ast,
            non_critical_errors: errors,
            warnings: Vec::with_capacity(32),
            unrecoverable: false,
        }
    }

    pub fn run(&mut self) {
        synchronize!(self;
            let attributes = self.parse_attributes()?;
            sync self.next_with_span() => {
                Token::EOF => end,
                Token::Module => self.parse_module(attributes),
                Token::Nature => self.parse_nature(attributes),
                Token::Discipline => self.parse_discipline(attributes),
            }
        )
    }
    /// Returns the next token. Note that this consumes the token. If you wish to avoid this use [`look_ahead`](crate::parser::Parser::look_ahead)
    fn next(&mut self) -> Result<Token> {
        match self.lookahead.take() {
            None => {
                self.preprocessor.advance()?;
                Ok(self.preprocessor.current_token())
            }
            Some(res) => res.map(|(token, _, _)| token),
        }
    }

    /// Returns the next token and its span. Note that this consumes the token. If you wish to avoid this use [`look_ahead_with_span`](crate::parser::Parser::look_ahead_with_span)
    fn next_with_span(&mut self) -> Result<(Token, Span)> {
        match self.lookahead.take() {
            None => {
                self.preprocessor.advance()?;
                Ok((self.preprocessor.current_token(), self.preprocessor.span()))
            }
            Some(res) => res.map(|(token, span, _)| (token, span)),
        }
    }

    /// Returns the next token and its span. Note that this consumes the token. If you wish to avoid this use [`look_ahead_with_span_and_previous_end`](crate::parser::Parser::look_ahead_with_span_and_previous_end)
    fn next_with_span_and_previous_end(&mut self) -> Result<(Token, Span, Index)> {
        match self.lookahead.take() {
            None => {
                let end = self.preprocessor.current_end();
                self.preprocessor.advance()?;
                Ok((
                    self.preprocessor.current_token(),
                    self.preprocessor.span(),
                    end,
                ))
            }
            Some(res) => res,
        }
    }

    /// Returns the next token without consuming it so the next `look_ahead`/[`next`](crate::parser::Parser::next) call will return the same token again
    /// if you do decide to consume the token use [`consume_lookahead`](crate::parser::Parser::consume_lookahead)
    fn look_ahead(&mut self) -> Result<Token> {
        if let Some(ref lookahead) = self.lookahead {
            return lookahead.clone().map(|(token, _, _)| token);
        }
        let end = self.preprocessor.current_end();
        let res = self.preprocessor.advance().map(|_| {
            (
                self.preprocessor.current_token(),
                self.preprocessor.span(),
                end,
            )
        });
        self.lookahead = Some(res.clone());
        res.map(|(token, _, _)| token)
    }

    /// Returns the next token and its span without consuming it so the next `look_ahead`/[`next`](crate::parser::Parser::next) call will return the same token again
    /// if you do decide to consume the token use [`consume_lookahead`](crate::parser::Parser::consume_lookahead)
    fn look_ahead_with_span(&mut self) -> Result<(Token, Span)> {
        if let Some(ref lookahead) = self.lookahead {
            return lookahead.clone().map(|(token, span, _)| (token, span));
        }
        let end = self.preprocessor.current_end();
        let res = self.preprocessor.advance().map(|_| {
            (
                self.preprocessor.current_token(),
                self.preprocessor.span(),
                end,
            )
        });
        self.lookahead = Some(res.clone());
        res.map(|(token, span, _)| (token, span))
    }

    /// Returns the next token, its span and the end of the previous token without consuming it so the next `look_ahead`/[`next`](crate::parser::Parser::next) call will return the same token again
    /// if you do decide to consume the token use [`consume_lookahead`](crate::parser::Parser::consume_lookahead)
    fn look_ahead_with_span_and_previous_end(&mut self) -> Result<(Token, Span, Index)> {
        if let Some(ref lookahead) = self.lookahead {
            return lookahead.clone();
        }
        let end = self.preprocessor.current_end();
        let res = self.preprocessor.advance().map(|_| {
            (
                self.preprocessor.current_token(),
                self.preprocessor.span(),
                end,
            )
        });
        self.lookahead = Some(res.clone());
        res
    }

    /// Consumes the current lookahead see [`look_ahead`](crate::parser::Parser::look_ahead)
    fn consume_lookahead(&mut self) {
        self.lookahead = None
    }

    /// Parses any of the two identifiers legal in verilog-ams (see standard for more details):
    ///
    /// * Normal identifiers
    ///
    /// * Escaped identifiers - Starting with `\` and ending with a Whitespace are allow
    ///
    ///
    /// # Arguments
    /// `optional` - Indicates that parsing the identifier is optional.
    /// In that case [`look_ahead`](crate::parser::Parser::look_ahead) is used instead of `next` to allow easier recovery
    ///
    #[inline]
    pub fn parse_identifier(&mut self, optional: bool) -> Result<Ident> {
        let (token, source) = if optional {
            self.look_ahead_with_span()?
        } else {
            self.next_with_span()?
        };

        let identifier = match token {
            Token::SimpleIdentifier(_) => self.preprocessor.slice(),

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
            self.consume_lookahead();
        }

        Ok(Ident::from_str_and_span(identifier, source))
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
        //Since hierarchical_identifier are made up of multiple lexer tokens they can not be parsed optionally.
        // This will have to be handled by caller so we just pass false to parse_identifier
        let mut names = vec![self.parse_identifier(false)?];
        while self.look_ahead()? == Token::Accessor {
            self.consume_lookahead();
            names.push(self.parse_identifier(false)?)
        }
        Ok(HierarchicalId { names })
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
        let mut attribute_map: FxHashMap<Symbol, AttributeId> = FxHashMap::default();
        loop {
            if self.look_ahead()? != Token::AttributeStart {
                break;
            }
            self.consume_lookahead();
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
    fn parse_attribute(&mut self, attribute_map: &mut FxHashMap<Symbol, AttributeId>) -> Result {
        let name = if self.look_ahead()? == Token::Units {
            self.consume_lookahead();
            Ident::new(keywords::UNITS, self.preprocessor.span())
        } else {
            self.parse_identifier(false)?
        };
        let value = if self.look_ahead()? == Token::Assign {
            self.consume_lookahead();
            Some(self.parse_expression_id()?)
        } else {
            None
        };
        if let Some(id) = attribute_map.get(&name.name) {
            let old_name = self.ast[*id].name;
            self.warnings.push(Warning {
                error_type: WarningType::AttributeOverwrite(old_name, name.span),
                source: old_name.span.extend(name.span),
            });
            self.ast[*id] = Attribute { name, value };
        } else {
            let id = self.ast.attributes.push(Attribute { name, value });
            attribute_map.insert(name.name, id);
        }
        Ok(())
    }

    /// Tries to consume `token` returns an Unexpected Token error otherwise
    #[inline]
    pub fn expect(&mut self, token: Token) -> Result {
        let (found, source, expected_at) = self.next_with_span_and_previous_end()?;
        if found != token {
            debug!("Parser: Expected {} but found {}", token, found);
            Err(Error {
                source,
                error_type: error::Type::MissingOrUnexpectedToken {
                    expected: vec![token],
                    expected_at,
                },
            })
        } else {
            Ok(())
        }
    }

    #[inline]
    pub fn try_expect(&mut self, token: Token) {
        match self.look_ahead_with_span_and_previous_end() {
            Ok((found, _, _)) if found == token => {
                self.consume_lookahead();
            }
            Ok((found, source, expected_at)) if found == Token::Unexpected => {
                debug!(
                    "Parser: Expected {} but found {} lexical error",
                    token, found
                );
                self.consume_lookahead();
                self.non_critical_errors.push(Error {
                    source,
                    error_type: error::Type::MissingOrUnexpectedToken {
                        expected: vec![found],
                        expected_at,
                    },
                })
            }
            Err(error) => {
                debug!("Parser: Expected {} but found preprocessor error", token);
                self.consume_lookahead();
                self.non_critical_errors.push(error)
            }
            Ok((found, source, expected_at)) => {
                debug!(
                    "Parser: Expected {} but found {} (not consumed)",
                    token, found
                );
                self.non_critical_errors.push(Error {
                    source,
                    error_type: error::Type::MissingOrUnexpectedToken {
                        expected: vec![token],
                        expected_at,
                    },
                });
            }
        }
    }

    #[inline]
    pub fn expect_lookahead(&mut self, token: Token) -> Result {
        let (found, source, expected_at) = self.look_ahead_with_span_and_previous_end()?;
        if found != token {
            Err(Error {
                source,
                error_type: error::Type::MissingOrUnexpectedToken {
                    expected: vec![token],
                    expected_at,
                },
            })
        } else {
            self.consume_lookahead();
            Ok(())
        }
    }

    #[inline]
    pub fn span_to_current_end(&self, start: Index) -> Span {
        Span::new(start, self.preprocessor.current_end())
    }

    #[inline]
    pub fn insert_symbol(&mut self, name: Ident, declaration: SymbolDeclaration) {
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
    /// The main point of this module. Parses a verilog-ams source file into an ast and returns any errors that occur
    ///
    /// # Arguments
    ///
    /// * `main_file` - The Verilog-A source file to parse
    ///
    /// * `source_map_allocator` - A bump allocator that will be used to allocate the source map.
    /// (`Bump::new()` can be used to create one)
    ///
    ///
    /// # Returns
    ///
    /// * An **Io Error** if the `main_file` could not be read
    /// * A [`SourceMap`](crate::parser::preprocessor::SourceMap) of the parsed file generated during parsing
    /// * A list of all Errors that occurred during parsing
    /// * A list of all Warnings generated during parsing

    pub fn parse_from<'source_map, 'lt>(
        &'lt mut self,
        main_file: &Path,
        source_map_allocator: &'source_map Bump,
    ) -> std::io::Result<(
        &'source_map SourceMap<'source_map>,
        Vec<Error>,
        Vec<Warning>,
    )> {
        let allocator = Bump::new();

        let mut preprocessor = Preprocessor::new(&allocator, source_map_allocator, main_file)?;

        let mut errors = Vec::with_capacity(64);
        if let Err(error) = preprocessor.advance() {
            errors.push(error);
        }

        let mut parser = Parser::new(preprocessor, self, errors);

        // The preprocessors current token is set to the first token after initialization.
        // The parsers lookahead needs to be set to this token as it is skipped otherwise
        parser.lookahead = Some(Ok((
            parser.preprocessor.current_token(),
            parser.preprocessor.span(),
            0,
        )));

        parser.run();

        Ok((
            parser.preprocessor.done(),
            parser.non_critical_errors,
            parser.warnings,
        ))
    }

    /// Parses a verilog-ams source file into an ast and prints any errors that occur
    ///
    /// # Arguments
    ///
    /// * `main_file` - The Verilog-A source file to parse
    ///
    /// * `source_map_allocator` - A bump allocator that will be used to allocate the source map.
    /// (`Bump::new()` can be used to create one)
    ///
    /// * `translate_lines` - When this is set to true the line numbers of printed errors are translated
    /// to reflect the line in the original source file instead of the source that was expanded by the preprocessor
    ///
    /// # Returns
    /// * **Parse successful** - A Source Map of the parsed source
    /// * **Errors occurred during** - Prints the errors and returns `None`
    pub fn parse_from_and_print_errors<'source_map, 'lt>(
        &'lt mut self,
        main_file: &Path,
        source_map_allocator: &'source_map Bump,
        translate_lines: bool,
    ) -> Option<&'source_map SourceMap<'source_map>> {
        match self.parse_from(main_file, source_map_allocator) {
            Ok((source_map, errors, warnings)) if errors.is_empty() => {
                warnings
                    .into_iter()
                    .for_each(|warning| warning.print(source_map, translate_lines));
                Some(source_map)
            }
            Ok((source_map, errors, warnings)) => {
                warnings
                    .into_iter()
                    .for_each(|warning| warning.print(source_map, translate_lines));
                errors
                    .into_iter()
                    .for_each(|err| err.print(&source_map, translate_lines));
                None
            }
            Err(error) => {
                error!("failed to open : {}!\n\t caused by {}",main_file.display(),error);
                None
            }
        }
    }
}
