/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::convert::{TryFrom, TryInto};

use data_structures::index_vec::{define_index_type, IndexSlice, IndexVec};
use vfs::VfsPath;

use crate::diagnostics::PreprocessorDiagnostic;
use crate::tokenstream::{ParsedToken, ParsedTokenStream, TokenKind};
use crate::Token;
use crate::{
    lexer::RawToken,
    sourcemap::{CtxSpan, SourceContext},
    token_set::TokenSet,
    tokenstream::TokenStream,
    Diagnostics,
};
use logos::Logos;
use data_structures::text_size::{TextRange, TextSize};
use std::ops::Range;
use tracing::{debug, trace};

define_index_type! {
     struct FullTokenIdx = u32;
     IMPL_RAW_CONVERSIONS = true;
}
define_index_type! {
     struct RelevantTokenIdx = u32;
     IMPL_RAW_CONVERSIONS = true;
}

pub(crate) type SpannedRawToken = (RawToken, TextRange);

pub(crate) struct Parser<'a, 'd> {
    full_tokens: IndexVec<FullTokenIdx, SpannedRawToken>,
    relevant_tokens: IndexVec<RelevantTokenIdx, (RawToken, FullTokenIdx)>,
    curr: (RawToken, FullTokenIdx, RelevantTokenIdx),
    src: &'a str,
    pub(crate) ctx: SourceContext,
    pub(crate) dst: &'d mut TokenStream,
    pub(crate) working_dir: VfsPath,
}

fn mk_token(
    pos: RelevantTokenIdx,
    relevant_tokens: &IndexSlice<RelevantTokenIdx, [(RawToken, FullTokenIdx)]>,
    file_end: FullTokenIdx,
) -> (RawToken, FullTokenIdx) {
    relevant_tokens.get(pos).map_or((RawToken::EOF, file_end), |(token, pos)| (*token, *pos))
}

impl<'a, 'd> Parser<'a, 'd> {
    pub(crate) fn new(
        src: &'a str,
        ctx: SourceContext,
        working_dir: VfsPath,
        dst: &'d mut TokenStream,
    ) -> Self {
        let mut full_tokens: IndexVec<FullTokenIdx, _> = RawToken::lexer(src)
            .spanned()
            .map(|(token, span)| {
                let span =
                    TextRange::new(span.start.try_into().unwrap(), span.end.try_into().unwrap());
                (token, span)
            })
            .collect();
        let end_pos = TextSize::of(src).checked_sub(1.into()).unwrap_or(0.into());
        full_tokens.push((RawToken::EOF, TextRange::empty(end_pos)));

        let relevant_tokens: IndexVec<_, _> = full_tokens
            .iter_enumerated()
            .filter_map(|(pos, (token, _))| {
                match token {
                    RawToken::WhiteSpace | RawToken::BlockComment => None, // No effect
                    RawToken::Comment => Some((RawToken::Newline, pos)), // The line comment always acts like a newline
                    token => Some((*token, pos)),
                }
            })
            .collect();

        dst.reserve(dst.capacity() - dst.len() + full_tokens.len());

        let (curr, pos) = mk_token(
            RelevantTokenIdx::from_raw_unchecked(0),
            &relevant_tokens,
            full_tokens.len_idx(),
        );

        let mut res = Self {
            relevant_tokens,
            full_tokens,
            curr: (curr, pos, RelevantTokenIdx::new(0)),
            src,
            ctx,
            dst,
            working_dir,
        };
        Self::save_tokens(res.full_tokens[..pos].as_raw_slice(), &mut res.dst, ctx);
        res
    }

    pub(crate) fn current(&self) -> RawToken {
        self.curr.0
    }

    pub(crate) fn current_range(&self) -> TextRange {
        self.full_tokens[self.curr.1].1
    }

    pub(crate) fn current_span(&self) -> CtxSpan {
        CtxSpan { range: self.current_range(), ctx: self.ctx }
    }

    pub(crate) fn current_text(&self) -> &'a str {
        let range: Range<usize> = self.current_range().into();
        &self.src[range]
    }
    //
    // pub(crate) fn lookahead_nth(&self, n: usize) -> RawToken {
    //     mk_token(self.curr.2 + n, &self.relevant_tokens, self.full_tokens.len_idx()).0
    // }

    pub(crate) fn previous_span(&self) -> TextRange {
        let previous_token_pos =
            self.relevant_tokens.get(self.curr.2 - 1).map_or(self.curr.1, |(_, pos)| *pos);
        self.full_tokens[previous_token_pos].1
    }

    /// Lookahead n tokens. However trivia tokens (whitespace, comment and blockcomment) are included
    pub(crate) fn lookahead_nth_with_trivia(&self, n: usize) -> RawToken {
        self.full_tokens[self.curr.1 + n].0
    }

    fn do_bump(&mut self, save_token: bool) {
        // trace!(token = display(self.current()), save = save_token, "bump");

        if self.curr.0 == RawToken::EOF {
            return;
        }

        let start = self.curr.1;
        let (token, pos) =
            mk_token(self.curr.2 + 1, &self.relevant_tokens, self.full_tokens.len_idx());

        self.curr = (token, pos, self.curr.2 + 1);
        if save_token {
            Self::save_tokens(self.full_tokens[start..pos].as_raw_slice(), &mut self.dst, self.ctx)
        }
    }

    fn save_tokens(src: &[SpannedRawToken], dst: &mut TokenStream, ctx: SourceContext) {
        // let mut iter = src.iter();

        // if let Some((kind, range)) = iter.next().cloned() {
        //     if let Some(previous_whitespace) = previous_whitespace(dst, kind) {
        //         if previous_whitespace.ctx == ctx {
        //             previous_whitespace.range =
        //                 TextRange::new(previous_whitespace.range.start(), range.end())
        //         } else if let Ok(kind) = TokenKind::try_from(kind) {
        //             dst.push(Token { kind, span: CtxSpan { range, ctx } })
        //         }
        //     }
        // }

        dst.reserve(src.len());
        for (kind, range) in src.iter().cloned() {
            // if let Some(prev_span) = previous_whitespace(dst, kind).and_then(|prev_span|) {
            //     prev_span.range = TextRange::new(prev_span.range.start(), range.end());
            // } else 
            if let Ok(kind) = TokenKind::try_from(kind) {
                dst.push(Token { kind, span: CtxSpan { range, ctx } });
            }
        }
    }

    fn save_tokens_to_macro(src: &[SpannedRawToken], dst: &mut ParsedTokenStream) {
        dst.reserve(src.len());
        for (kind, range) in src.iter().cloned() {
            // if let Some(prev_span) = previous_macro_whitespace(dst, kind) {
            //     *prev_span = TextRange::new(prev_span.start(), range.end());
            // } else 
            if let Ok(t) = ParsedToken::try_from(kind) {
                dst.push((t, range));
            }
        }
    }

    pub(crate) fn bumpy_any_to_macro(&mut self, dst: &mut ParsedTokenStream) {
        // trace!(token = display(self.current()), "bump to macro");
        if self.curr.0 == RawToken::EOF {
            return;
        }

        let start = self.curr.1;
        let (token, pos) =
            mk_token(self.curr.2 + 1, &self.relevant_tokens, self.full_tokens.len_idx());

        self.curr = (token, pos, self.curr.2 + 1);
        Self::save_tokens_to_macro(self.full_tokens[start..pos].as_raw_slice(), dst)
    }

    pub(crate) fn expect(&mut self, expected: RawToken, errors: &mut Diagnostics) -> bool {
        self.expect_with(expected, expected, errors)
    }

    pub(crate) fn expect_with(
        &mut self,
        token: RawToken,
        expected: RawToken,
        errors: &mut Diagnostics,
    ) -> bool {
        if !self.eat(token) {
            debug!("syntax error: expected {} but found {}", token, self.current());
            errors.push(PreprocessorDiagnostic::MissingOrUnexpectedToken {
                expected,
                expected_at: CtxSpan { range: self.previous_span(), ctx: self.ctx },
                span: CtxSpan { range: self.current_range(), ctx: self.ctx },
            });
            self.eat(RawToken::Unexpected); // Only report lexer errors once
            false
        } else {
            true
        }
    }

    pub(crate) fn expect_with_recovery(
        &mut self,
        token: RawToken,
        expected: RawToken,
        recover: TokenSet,
        errors: &mut Diagnostics,
    ) -> bool {
        if !self.eat(token) {
            debug!("syntax error: expected {} but found {}", token, self.current());
            errors.push(PreprocessorDiagnostic::MissingOrUnexpectedToken {
                expected,
                expected_at: CtxSpan { range: self.previous_span(), ctx: self.ctx },
                span: CtxSpan { range: self.current_range(), ctx: self.ctx },
            });
            if !self.at_ts(recover) {
                self.bump_any()
            }
            false
        } else {
            true
        }
    }

    pub(crate) fn at(&self, token: RawToken) -> bool {
        self.current() == token
    }
    pub(crate) fn at_ts(&self, token: TokenSet) -> bool {
        token.contains(self.current())
    }

    pub(crate) fn ctx(&self) -> SourceContext {
        self.ctx
    }

    // /// Lookahead operation: returns the kind of the next nth
    // /// token.
    // pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
    //     assert!(n <= 2);
    //     self.lookahead_nth(n)
    // }
    //
    // /// Checks if the current token is `kind`.
    // pub(crate) fn at(&self, token: RawToken) -> bool {
    //     self.current() == token
    // }
    //
    // pub(crate) fn nth_at(&self, n: usize, token: RawToken) -> bool {
    //     self.lookahead_nth(n) == token
    // }

    /// Consume the next token if `kind` matches.
    pub(crate) fn eat(&mut self, token: RawToken) -> bool {
        if !self.at(token) {
            return false;
        }
        self.do_bump(false);
        true
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn bump(&mut self, token: RawToken) {
        assert!(self.eat(token));
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn bump_any(&mut self) {
        self.do_bump(false);
    }

    /// Advances the parser by one token
    pub(crate) fn save_token(&mut self) {
        self.do_bump(true)
    }
}

fn previous_whitespace(dst: &mut TokenStream, t: RawToken) -> Option<&mut CtxSpan> {
    if matches!(t, RawToken::WhiteSpace | RawToken::Newline | RawToken::MacroDefNewLine) {
        if let Token { kind: TokenKind::WhiteSpace, span } = dst.last_mut()? {
            return Some(span);
        }
    }
    None
}

fn previous_macro_whitespace<'a>(
    dst: &'a mut ParsedTokenStream,
    t: RawToken,
) -> Option<&'a mut TextRange> {
    if matches!(t, RawToken::WhiteSpace | RawToken::Newline | RawToken::MacroDefNewLine) {
        if let (ParsedToken::ResolvedToken(TokenKind::WhiteSpace), span) = dst.last_mut()? {
            return Some(span);
        }
    }
    None
}
