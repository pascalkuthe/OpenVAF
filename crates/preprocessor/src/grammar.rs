/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::diagnostics::PreprocessorDiagnostic::{self, UnexpectedEof};
use crate::lexer::RawToken;
use crate::sourcemap::{CtxSpan, SourceMap};
use crate::token_set::TokenSet;
use crate::tokenstream::{Macro, MacroArg, MacroCall, ParsedToken, ParsedTokenStream};
use crate::{parser::Parser, processor::Processor, Diagnostics};
use data_structures::index_vec::IndexVec;
use data_structures::text_size::TextRange;
use tracing::{debug, error, trace, trace_span};

pub(crate) fn parse_condition<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    processor: &mut Processor<'a>,
    inverted: bool,
) {
    let tspan = trace_span!("parsing macro condition");
    let _tspan = tspan.enter();

    let name = p.current_text();
    if p.expect(RawToken::SimpleIdentifier, err) {
        trace!(name = name, "condition");
        if processor.is_macro_defined(name) != inverted {
            parse_if_body::<true, false>(p, err, processor); // condition is true
        } else {
            parse_if_body::<false, true>(p, err, processor); // condition is false. find an else or endif
        }
    } else {
        // Just try to skip to the end of the if
        parse_if_body::<false, false>(p, err, processor)
    }
}

fn parse_if_body<'a, const PROCESS: bool, const CONSIDER_ELSE: bool>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    processor: &mut Processor<'a>,
) {
    let mut depth = 0;
    loop {
        match p.current() {
            RawToken::MacroEndIf if depth == 0 => {
                p.bump(RawToken::MacroEndIf);
                break;
            }

            RawToken::MacroEndIf => {
                depth -= 1;
            }
            RawToken::MacroIfn | RawToken::MacroIf if !PROCESS => {
                depth += 1;
            }

            RawToken::EOF => {
                err.push(UnexpectedEof { expected: RawToken::MacroEndIf, span: p.current_span() });
                break;
            }
            RawToken::MacroElse | RawToken::MacroElsif if PROCESS && depth == 0 => {
                parse_if_body::<false, false>(p, err, processor);
                return;
            }

            RawToken::MacroElse if CONSIDER_ELSE && depth == 0 => {
                p.bump(RawToken::MacroElse);
                parse_if_body::<true, false>(p, err, processor);
                break;
            }

            RawToken::MacroElsif if CONSIDER_ELSE && depth == 0 => {
                p.bump(RawToken::MacroElsif);
                parse_condition(p, err, processor, false);
                break;
            }

            _ => (),
        }
        if PROCESS {
            processor.process_token(p, err)
        } else {
            // ignore tokens
            p.bump_any()
        }
    }
}

pub(crate) fn parse_include<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
) -> Option<(&'a str, TextRange)> {
    let tspan = trace_span!("parsing `include");
    let _tspan = tspan.enter();

    let start = p.current_range().start();
    p.bump(RawToken::Include);
    let path = p.current_text();
    if p.expect(RawToken::LitString, err) {
        Some((&path[1..path.len() - 1], TextRange::new(start, p.current_range().start())))
    } else {
        None
    }
}

const MACRO_ARG_DEF_TERMINATOR_SET: TokenSet =
    TokenSet::new(&[RawToken::ParenClose]).union(MACRO_TERMINATOR_SET);

const MACRO_TERMINATOR_SET: TokenSet = TokenSet::new(&[RawToken::EOF, RawToken::Newline]);

pub(crate) fn parse_define<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    sm: &mut SourceMap,
) -> Option<(&'a str, Macro<'a>)> {
    let start = p.current_range();
    p.bump(RawToken::MacroDef);
    let name = p.current_text();
    let tspan = trace_span!("parsing `define", name = name);
    let _tspan = tspan.enter();

    let followed_by_bracket = p.lookahead_nth_with_trivia(1) == RawToken::ParenOpen;
    let mut success = p.expect(RawToken::SimpleIdentifier, err);
    let args = if followed_by_bracket {
        p.bump(RawToken::ParenOpen);
        let mut args = Vec::new();
        while !p.at_ts(MACRO_ARG_DEF_TERMINATOR_SET) {
            args.push(p.current_text());
            if !p.expect(RawToken::SimpleIdentifier, err) {
                success = false
            }
            if !p.at(RawToken::ParenClose) {
                success &= p.expect_with_recovery(
                    RawToken::Comma,
                    RawToken::ParenClose,
                    MACRO_ARG_DEF_TERMINATOR_SET,
                    err,
                )
            }
        }

        p.eat(RawToken::ParenClose);
        args
    } else {
        Vec::new()
    };

    let head = p.previous_span().end() - start.start();

    let mut body = Vec::new();

    while !p.at_ts(MACRO_TERMINATOR_SET) {
        parse_macro_token::<true>(p, err, &args, &mut body, sm)
    }
    p.bump_any();

    let range = start.cover(p.previous_span());
    if success {
        Some((
            name,
            Macro { head, body, arg_cnt: args.len(), span: p.current_span().with_range(range) },
        ))
    } else {
        None
    }
}

fn parse_macro_token<'a, const INSIDE_MACRO: bool>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    args: &[&'a str],
    dst: &mut ParsedTokenStream<'a>,
    sm: &mut SourceMap,
) {
    // trace!(token = display(p.current()), "parse macro token");

    if p.at(RawToken::SimpleIdentifier) {
        if let Some(arg) = args.iter().position(|x| *x == p.current_text()) {
            debug!(name = p.current_text(), idx = arg, "macro arg reference");
            p.bump(RawToken::SimpleIdentifier);
            dst.push((ParsedToken::ArgumentReference(MacroArg::new(arg)), p.current_range()));
            return;
        }
    }

    if p.at(RawToken::MacroCall) {
        let (call, span) = parse_macro_call::<INSIDE_MACRO>(p, err, args, sm);
        dst.push((ParsedToken::MacroCall(call), span));
        return;
    }

    p.bumpy_any_to_macro(dst)
}

const MACRO_ARG_CALL_TERMINATOR_SET: TokenSet =
    TokenSet::new(&[RawToken::ParenClose, RawToken::EOF]);

pub(crate) fn parse_macro_call<'a, const INSIDE_MACRO: bool>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    args: &[&'a str],
    sm: &mut SourceMap,
) -> (MacroCall<'a>, TextRange) {
    let tspan = trace_span!("parsing macro call");
    let _tspan = tspan.enter();

    let start = p.current_range();
    let name = &p.current_text()[1..];
    p.bump(RawToken::MacroCall);

    let terminator = if INSIDE_MACRO {
        MACRO_ARG_CALL_TERMINATOR_SET.union(MACRO_TERMINATOR_SET)
    } else {
        MACRO_ARG_CALL_TERMINATOR_SET
    };

    let arg_bindings = if p.eat(RawToken::ParenOpen) {
        let mut arg_bindings = IndexVec::<MacroArg, _>::with_capacity(4);
        'outer: while !p.at_ts(terminator) {
            let mut dst = Vec::with_capacity(18);
            let start = p.current_range().start();

            let mut depth = 0; // allow for nested brackets inside macros
            loop {
                match p.current() {
                    RawToken::ParenClose if depth != 0 => depth -= 1,
                    RawToken::ParenOpen => depth += 1,
                    RawToken::ParenClose | RawToken::Comma if depth == 0 => break,
                    RawToken::EOF => break,
                    RawToken::Newline if INSIDE_MACRO => {
                        error!("BREAK INSIDE MACRO");
                        err.push(PreprocessorDiagnostic::MissingOrUnexpectedToken {
                            expected: RawToken::ParenClose,
                            expected_at: CtxSpan { range: p.previous_span(), ctx: p.ctx },
                            span: CtxSpan { range: p.current_range(), ctx: p.ctx },
                        });
                        break 'outer;
                    }
                    _ => (),
                }

                parse_macro_token::<INSIDE_MACRO>(p, err, args, &mut dst, sm)
            }

            if !p.at(RawToken::ParenClose) {
                p.expect_with_recovery(RawToken::Comma, RawToken::ParenClose, terminator, err);
            }
            let end = p.previous_span().end();
            arg_bindings.push((dst, TextRange::new(start, end)));
        }
        p.eat(RawToken::ParenClose);
        arg_bindings
    } else {
        IndexVec::new()
    };

    let span = start.cover(p.previous_span());
    (MacroCall { name, arg_bindings }, span)
}
