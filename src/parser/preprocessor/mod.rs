/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::collections::HashMap;
use std::path::Path;
use std::pin::Pin;
use std::vec::IntoIter;

use indexmap::map::IndexMap;
use intrusive_collections::__core::fmt::{Debug, Formatter};
use intrusive_collections::__core::iter::Peekable;
use log::*;

pub use source_map::SourceMap;
use source_map::SourceMapBuilder;
pub(crate) use source_map::{ArgumentIndex, CallDepth};

use crate::error::Error;
use crate::parser::error;
use crate::parser::error::List;
use crate::parser::lexer::Token;
use crate::parser::primaries::parse_string;
use crate::span::{Index, IndexOffset, Range};
use crate::{Lexer, Span};

use super::Result;

mod source_map;

#[cfg(test)]
pub mod test;

#[derive(EnumAsInner)]
enum TokenSource {
    File(Lexer<'static>),
    Insert(Peekable<IntoIter<MacroBodyElement>>, bool),
}
impl Debug for TokenSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            TokenSource::File(_) => f.write_str("FILE")?,
            TokenSource::Insert(ref iter, is_macro) => f.write_fmt(format_args!(
                "{}:{:?}",
                if *is_macro { "Macro:" } else { "" },
                iter
            ))?,
        }
        Ok(())
    }
}

type MacroBodyElement = (Span, MacroBodyToken);

#[derive(Clone, Debug)]
enum MacroBodyToken {
    ArgumentReference(ArgumentIndex, CallDepth),
    LexerToken(Token),
    MacroReference(UnresolvedMacroReference),
}

#[derive(Debug, Clone)]
struct Macro {
    body: Vec<MacroBodyElement>,
    arg_count: ArgumentIndex,
    source: String,
    span: Span,
}

#[derive(Debug, Clone)]
struct UnresolvedMacroReference {
    name: String,
    source: String,
    arg_bindings: Vec<MacroArg>,
}
#[derive(Debug)]
struct PreprocessorState {
    start: Index,
    offset: IndexOffset,
    token_source: TokenSource,
}
impl PreprocessorState {
    pub fn new(start: Index, token_source: TokenSource) -> Self {
        Self {
            start,
            offset: start as IndexOffset,
            token_source,
        }
    }
}
#[derive(Debug, Clone)]
struct MacroArg {
    tokens: Vec<MacroBodyElement>,
    source: String,
}
impl MacroArg {
    pub fn new(tokens: Vec<MacroBodyElement>, source: String) -> Self {
        Self { tokens, source }
    }
}
pub struct Preprocessor {
    //internal state
    macros: HashMap<String, Macro>,
    called_macros: IndexMap<String, Vec<MacroArg>>,
    source_map_builder: Pin<Box<SourceMapBuilder>>,
    state_stack: Vec<PreprocessorState>,
    condition_stack: Vec<Span>,
    /// Start of the current token in the source that contains it. (Either a file or a macro definition)
    current_source_start: Index,
    current_start: Index,
    current_len: Index,
    current_token: Token,
}

impl Preprocessor {
    pub fn new(main_file: &Path) -> std::io::Result<Self> {
        let (source_map_builder, main_lexer) = unsafe { SourceMapBuilder::new(main_file)? };
        let mut res = Self {
            macros: HashMap::new(),
            called_macros: IndexMap::new(),
            source_map_builder,
            state_stack: Vec::new(),
            condition_stack: Vec::new(),
            current_token: Token::EOF,
            current_source_start: 0,
            current_start: 0,
            current_len: 0,
        };
        res.current_token = main_lexer.token();
        res.current_len = main_lexer.token_len();
        res.state_stack
            .push(PreprocessorState::new(0, TokenSource::File(main_lexer)));
        Ok(res)
    }
    pub fn done(self) -> Box<SourceMap> {
        self.source_map_builder.done()
    }

    fn include(&mut self, file: &Path, include_directive_span: Span) -> Result {
        let lexer = unsafe {
            self.source_map_builder.as_mut().enter_file(
                file,
                self.current_start,
                include_directive_span,
            ) //This is save since we only use the unbounded lifetime(s) internally
        }
        .map_err(|io_err| Error {
            source: include_directive_span,
            error_type: io_err.into(),
        })?;
        self.state_stack.last_mut().unwrap().offset -=
            include_directive_span.get_len() as IndexOffset;
        self.state_stack.push(PreprocessorState::new(
            self.current_start,
            TokenSource::File(lexer),
        ));
        Ok(())
    }

    /// Advances the current state of the preprocessor
    /// This returns an error if a macro couldnt' be resolved however it always consumes to current token so the same error cant be returned twice
    fn advance_state(&mut self) -> Result {
        let (token, range) = self.next()?;
        self.current_token = token;
        self.current_source_start = range.start;
        self.current_start = (range.start as IndexOffset + self.current_offset()) as Index;
        self.current_len = range.end - range.start;
        Ok(())
    }

    /// This function advances to the next token from either a lexical source or an insertion source (macros and their arguments)
    /// This returns an error if a macro couldnt' be resolved however it always consumes to current token so the same error cant be returned twice
    fn next(&mut self) -> Result<(Token, Range)> {
        loop {
            let main_file = self.state_stack.len() == 1;
            let current_state = self.state_stack.last_mut().unwrap();
            let new_state = {
                match current_state.token_source {
                    TokenSource::File(ref mut lexer) => {
                        if *lexer.multi_line_count() == 0 {
                            lexer.advance();
                        }
                        if *lexer.multi_line_count() > 0 {
                            *lexer.multi_line_count() -= 1;
                            return Ok((Token::Newline, lexer.range())); //this is done here to avoid adding a lot of complications since newlines need to be handeled in macros arguments etc. too (and this doesnt cost much performance)
                        }
                        if lexer.token() != Token::EOF || main_file {
                            return Ok((lexer.token(), lexer.range()));
                        } else {
                            None
                        }
                    }
                    TokenSource::Insert(ref mut iter, _) => {
                        if let Some(macro_body_element) = iter.next() {
                            let span = macro_body_element.0;
                            match macro_body_element.1 {
                                MacroBodyToken::LexerToken(token) => {
                                    return Ok((token, span.into()));
                                }
                                MacroBodyToken::ArgumentReference(id, macro_depth) => {
                                    let arg = self
                                        .called_macros
                                        .get_index(
                                            self.called_macros.len() - (macro_depth as usize),
                                        )
                                        .unwrap()
                                        .1
                                        .get(id as usize)
                                        .unwrap();
                                    let token_source = TokenSource::Insert(
                                        arg.tokens.clone().into_iter().peekable(),
                                        false,
                                    );
                                    let start = (span.get_start() as IndexOffset
                                        + current_state.offset)
                                        as Index;
                                    current_state.offset -= span.get_len() as IndexOffset;
                                    self.source_map_builder
                                        .as_mut()
                                        .enter_non_root_substitution(span, arg.source.clone());
                                    Some((start, token_source))
                                }
                                MacroBodyToken::MacroReference(unresolved_reference) => {
                                    let start = (span.get_start() as IndexOffset
                                        + current_state.offset)
                                        as Index;
                                    let token_source =
                                        self.resolve_reference(span, unresolved_reference, false)?;
                                    Some((start, token_source))
                                }
                            }
                        } else {
                            None
                        }
                    }
                }
            };
            //the token source stack can only be mutated here and not inside the match
            if let Some(state) = new_state {
                self.state_stack
                    .push(PreprocessorState::new(state.0, state.1))
            } else {
                let insertion_length = self.source_map_builder.as_mut().finish_substitution();
                let old_state = self.state_stack.pop().expect("Main file was popped");
                match old_state.token_source {
                    TokenSource::Insert(_, is_macro) if is_macro => {
                        self.called_macros.pop();
                    }
                    _ => (),
                };
                let offset_change = old_state.offset - old_state.start as IndexOffset
                    + insertion_length as IndexOffset;
                self.state_stack.last_mut().unwrap().offset += offset_change
            }
        }
    }
    pub fn advance(&mut self) -> Result {
        self.advance_state()?;
        self.process_token()
    }
    pub fn process_token(&mut self) -> Result {
        loop {
            // advance state until error occurs or token not handled by the preprocessor is encountered
            match self.current_token {
                //TODO handel macro def new line here or are all matches non exhaustive anyway
                Token::UnexpectedEOF => {
                    let error = error::Type::UnexpectedEof {
                        expected: vec![Token::CommentEnd],
                    };
                    return self.token_error(error);
                }
                Token::Newline => self.source_map_builder.as_mut().new_line(),
                Token::Include => {
                    let start = self.current_start;
                    self.consume(Token::String)?;
                    let path_str = parse_string(self.slice());
                    let path = Path::new(&path_str);
                    self.include(
                        path,
                        Span::new(start, self.current_start + self.current_len),
                    )?;
                }
                Token::MacroReference => {
                    let unresolved_reference = self.parse_reference(&Vec::new(), 0)?;
                    let start = (unresolved_reference.0.get_start() as IndexOffset
                        + self.current_offset()) as Index;
                    let token_source = self.resolve_reference(
                        unresolved_reference.0,
                        unresolved_reference.1,
                        true,
                    )?;
                    self.state_stack
                        .push(PreprocessorState::new(start, token_source));
                }
                Token::MacroIf => self.process_condition(false)?,
                Token::MacroIfn => self.process_condition(true)?,
                Token::MacroEndIf => {
                    if self.condition_stack.is_empty() {
                        self.advance_state()?;
                        return self.token_error(error::Type::ConditionEndWithoutStart);
                    } else {
                        self.condition_stack.pop();
                    }
                }
                Token::MacroElse | Token::MacroElsif => self.skip_to_condition_end()?, //When a condition is first encountered we skip all irrelevant parts. So if we encounter an else or elseif that means a prior condition inside the condition block has already matched and we can skip this
                Token::EOF if !self.condition_stack.is_empty() => {
                    let error = error::Type::UnclosedConditions(self.condition_stack.clone());
                    self.condition_stack.clear(); //We call this  so this error doesnt reoccur
                    return self.token_error(error);
                }
                Token::EOF if self.state_stack.len() != 1 => {
                    unreachable!("Unclosed token_sources! {:?}", self.state_stack);
                }
                Token::MacroDef => {
                    self.parse_decl()?;
                }
                _ => {
                    return Ok(());
                }
            }
            self.advance_state()?;
        }
    }

    fn process_condition(&mut self, mut invert: bool) -> Result {
        self.advance_state()?;
        let mut start = self.current_start;
        loop {
            self.consume(Token::SimpleIdentifier)?;
            let name = self.slice();
            if invert ^ self.macros.contains_key(name) {
                debug!(
                    "Condition if{}def {} is fulfilled",
                    if invert { "n" } else { "" },
                    self.slice()
                );
                self.condition_stack
                    .push(Span::new(start, self.current_start + self.current_len));
                return Ok(());
            } else {
                debug!(
                    "Condition if{}def {} is not fulfilled",
                    if invert { "n" } else { "" },
                    self.slice()
                );
                loop {
                    match self.current_token {
                        Token::MacroElsif => {
                            invert = false; //elsif is never inverted according to standard (the compiler should (hopefully) figure out that after this point invert ^ x = x)
                            start = self.current_start;
                            break;
                        }
                        Token::MacroEndIf => return Ok(()),
                        Token::MacroElse => {
                            self.condition_stack.push(self.current_span());
                            return Ok(());
                        }
                        Token::UnexpectedEOF => {
                            let error = error::Type::UnexpectedEof {
                                expected: vec![Token::CommentEnd],
                            };
                            return self.token_error(error);
                        }
                        Token::Newline => {
                            self.source_map_builder.as_mut().new_line();
                            self.advance_state()?
                        }
                        Token::MacroIf | Token::MacroIfn => self.skip_to_condition_end()?,
                        _ => self.advance_state()?,
                    }
                }
            };
        }
    }

    fn skip_to_condition_end(&mut self) -> Result {
        self.advance_state()?;
        let mut ignore_conditions: Vec<Span> = Vec::new();
        loop {
            match self.current_token {
                Token::MacroEndIf if ignore_conditions.is_empty() => return Ok(()),
                Token::MacroIfn | Token::MacroIf => {
                    ignore_conditions.push(self.current_span());
                }
                Token::MacroEndIf => {
                    ignore_conditions.pop();
                }
                Token::Newline => {
                    self.source_map_builder.as_mut().new_line();
                    self.advance_state()?
                }
                Token::EOF => {
                    let error = error::Type::UnclosedConditions(ignore_conditions);
                    return self.token_error(error);
                }
                _ => self.advance_state()?,
            }
        }
    }

    fn parse_decl(&mut self) -> Result {
        self.advance_state()?;
        let name = self.slice().to_string();
        let mut args = Vec::new();
        let mut peek = self.peek();
        if peek.is_ok() && peek.as_ref().unwrap().1 == Token::ParenOpen {
            self.advance_state()?;
            self.advance_state()?;
            self.consume(Token::SimpleIdentifier)?;
            args.push(self.slice().to_string());
            loop {
                self.advance_state()?;
                match self.current_token {
                    Token::Newline => {
                        self.source_map_builder.as_mut().new_line();
                        self.advance_state()?
                    }
                    Token::ParenClose => {
                        break;
                    }
                    Token::Comma => {
                        self.advance_state()?;
                        self.consume(Token::SimpleIdentifier)?;
                        args.push(self.slice().to_string());
                    }
                    Token::EOF => {
                        return self.token_error(error::Type::UnexpectedEof {
                            expected: vec![Token::ParenClose],
                        });
                    }
                    _ => {
                        return self.token_error(error::Type::UnexpectedToken {
                            expected: vec![Token::ParenClose, Token::Comma],
                        });
                    }
                }
            }
            peek = self.peek();
        }
        let peek = peek?;
        let body_start = peek.0.start;
        let mut body = Vec::new();
        let mut peek = peek.1;
        while peek != Token::Newline && peek != Token::EOF {
            self.advance_state()?;
            body.push(self.current_macro_body_token(body_start, &args, 1)?);
            peek = self.peek()?.1;
        }
        let decl_range = Span::new(body_start, self.current_source_start + self.current_len);
        let decl_source = self.source()
            [decl_range.get_start() as usize..decl_range.get_end() as usize]
            .to_string();
        let maco_decl = Macro {
            body,
            arg_count: args.len() as ArgumentIndex,
            source: decl_source,
            span: decl_range,
        };
        if self.macros.insert(name, maco_decl).is_some() {
            /*warn!(
                "{}",
                self.source_map.format_error(Error {
                    source: range.into(),
                    error_type: error::WarningType::MacroOverwritten(location)
                })
            )*/ //TODO WARNINGS / ERROR printing
        }
        Ok(())
    }
    fn peek(&mut self) -> Result<(Range, Token)> {
        match self.state_stack.last_mut().unwrap().token_source {
            TokenSource::File(ref lexer) => Ok(lexer.peek()),
            TokenSource::Insert(ref mut iter, _) => {
                let res = iter.peek().unwrap();
                let token = if let MacroBodyToken::LexerToken(token) = res.1 {
                    token
                } else {
                    return Err(Error {
                        source: res.0,
                        error_type: error::Type::CompilerDirectiveSplit,
                    });
                };
                Ok((res.0.into(), token))
            }
        }
    }
    fn current_macro_body_token(
        &mut self,
        start: Index,
        args: &[String],
        depth: CallDepth,
    ) -> Result<MacroBodyElement> {
        let res = match self.current_token {
            Token::SimpleIdentifier => {
                let identifier = self.slice();
                if let Some(index) = args.iter().position(|arg_name| *arg_name == identifier) {
                    MacroBodyToken::ArgumentReference(index as ArgumentIndex, depth)
                } else {
                    MacroBodyToken::LexerToken(Token::SimpleIdentifier)
                }
            }
            Token::MacroReference => {
                let res = self.parse_reference(args, depth + 1)?;
                let span = Span::new_with_length(res.0.get_start() - start, res.0.get_len());
                return Ok((span, MacroBodyToken::MacroReference(res.1)));
            } //we only do this here to avoid recomputing call depth constantly also this parse is lightly more expensive so its nice for performance too. Not worth it for other directives (except definition; see below) since parsing is trivial for those
            //TODO parse macro definition here
            Token::MacroDef => {
                unimplemented!("Macro definitions inside macros are currently not supported")
            }
            Token::UnexpectedEOF => {
                let error = error::Type::UnexpectedEof {
                    expected: vec![Token::CommentEnd],
                };
                return self.token_error(error);
            }
            Token::MacroDefNewLine => {
                self.source_map_builder.as_mut().new_line();
                MacroBodyToken::LexerToken(Token::Newline)
            } //map to newline so we can keep track of lines inside macros
            Token::Newline => return self.token_error(error::Type::MacroEndTooEarly),
            token => MacroBodyToken::LexerToken(token),
        };
        Ok((
            Span::new_with_length(self.current_source_start - start, self.current_len),
            res,
        ))
    }

    fn parse_reference(
        &mut self,
        parent_macro_args: &[String],
        macro_call_depth: CallDepth,
    ) -> Result<(Span, UnresolvedMacroReference)> {
        let name = self.slice()[1..].to_string();
        let start = self.current_start;
        let source_start = self.current_source_start;
        let mut arg_bindings: Vec<MacroArg> = Vec::new();
        let mut source_end = self.current_source_start + self.current_len;
        let peek = self.peek();
        if peek.is_ok() && peek.unwrap().1 == Token::ParenOpen {
            self.advance_state()?; //skip bracket
            self.advance_state()?;
            let mut current_arg_body = Vec::new();
            let mut last_colon = start;
            let mut current_arg_start = self.current_source_start;
            loop {
                match self.current_token {
                    Token::ParenClose => break,
                    Token::Comma => {
                        if current_arg_body.is_empty() {
                            return Err(Error {
                                error_type: error::Type::EmptyListEntry(List::MacroArgument),
                                source: Span::new(
                                    last_colon,
                                    self.current_start + self.current_len,
                                ),
                            });
                        }
                        let source = &self.source()
                            [current_arg_start as usize..self.current_source_start as usize];
                        arg_bindings.push(MacroArg::new(current_arg_body, source.to_string()));
                        current_arg_body = Vec::new();
                        last_colon = self.current_start;
                        current_arg_start = self.current_source_start + 1;
                        //colon is not included
                    }
                    Token::EOF => {
                        return self.token_error(error::Type::UnexpectedEof {
                            expected: vec![Token::ParenClose],
                        });
                    }
                    t => current_arg_body.push(self.current_macro_body_token(
                        current_arg_start,
                        parent_macro_args,
                        macro_call_depth,
                    )?),
                }
                self.advance_state()?;
            }
            if current_arg_body.is_empty() {
                return Err(Error {
                    error_type: error::Type::EmptyListEntry(List::MacroArgument),
                    source: Span::new(last_colon, self.current_start + self.current_len),
                });
            }
            let source =
                &self.source()[current_arg_start as usize..self.current_source_start as usize];
            arg_bindings.push(MacroArg::new(current_arg_body, source.to_string()));
            source_end = self.current_source_start + 1;
        }

        let range = std::ops::Range {
            start: source_start as usize,
            end: source_end as usize,
        };
        let source = self.source()[range].to_string();
        Ok((
            Span::new(source_start, source_end),
            UnresolvedMacroReference {
                name,
                source,
                arg_bindings,
            },
        ))
    }

    fn resolve_reference(
        &mut self,
        source_span: Span,
        reference: UnresolvedMacroReference,
        root: bool,
    ) -> Result<TokenSource> {
        let span = source_span.signed_offset(self.current_offset());
        if self.called_macros.contains_key(&reference.name) {
            return Err(Error {
                error_type: error::Type::MacroRecursion,
                source: span,
            });
        }
        if let Some(definition) = self.macros.get(&reference.name) {
            if reference.arg_bindings.len() as ArgumentIndex != definition.arg_count {
                Err(Error {
                    error_type: error::Type::MacroArgumentCount {
                        found: reference.arg_bindings.len() as ArgumentIndex,
                        expected: definition.arg_count,
                    },
                    source: span,
                })
            } else {
                self.called_macros
                    .insert(reference.name, reference.arg_bindings.clone());
                self.state_stack.last_mut().unwrap().offset -= source_span.get_len() as IndexOffset;
                if root {
                    self.source_map_builder.as_mut().enter_root_macro(
                        span.get_start(),
                        source_span,
                        definition.span,
                        definition.source.clone(),
                    )
                } else {
                    self.source_map_builder
                        .as_mut()
                        .enter_non_root_substitution(source_span, definition.source.clone())
                }
                let token_source =
                    TokenSource::Insert(definition.body.clone().into_iter().peekable(), true);
                Ok(token_source)
            }
        } else {
            Err(Error {
                error_type: error::Type::MacroNotFound,
                source: span,
            })
        }
    }

    pub fn current_token(&self) -> Token {
        self.current_token
    }

    pub fn token_error<T>(&self, etype: error::Type) -> Result<T> {
        Err(Error {
            source: self.current_span(),
            error_type: etype,
        })
    }

    pub fn current_span(&self) -> Span {
        Span::new_with_length(self.current_start, self.current_len)
    }

    pub fn current_start(&self) -> Index {
        self.current_start
    }
    pub fn current_end(&self) -> Index {
        self.current_start + self.current_len
    }
    pub fn current_len(&self) -> Index {
        self.current_len
    }

    fn current_source_span(&self) -> Span {
        Span::new_with_length(self.current_source_start, self.current_len)
    }

    fn current_offset(&self) -> IndexOffset {
        self.state_stack.last().unwrap().offset
    }
    const EXPECT_TOKEN_SOURCES: &'static str =
        "Preprocessor State should at least contain the main file";

    pub fn source(&self) -> &str {
        self.source_map_builder.source()
    }
    pub fn slice(&self) -> &str {
        let source = self.source();
        &source[self.current_source_start as usize
            ..(self.current_source_start + self.current_len) as usize]
    }
    fn consume(&mut self, token: Token) -> Result<()> {
        if self.current_token != token {
            let error = error::Type::UnexpectedToken {
                expected: vec![token],
            };
            self.token_error(error)
        } else {
            Ok(())
        }
    }
}
