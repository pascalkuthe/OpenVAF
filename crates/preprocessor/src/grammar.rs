/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use text_size::TextRange;
// use tracing::{debug, trace, trace_span};
use typed_index_collections::TiVec;

use crate::diagnostics::PreprocessorDiagnostic::{self, UnexpectedEof};
use crate::parser::{CompilerDirective, FullTokenIdx, Parser, PreprocessorToken};
use crate::processor::{Macro, MacroArg, MacroCall, ParsedToken, ParsedTokenKind, Processor};
use crate::sourcemap::{CtxSpan, SourceMap};
use crate::Diagnostics;

pub(crate) fn parse_condition<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    processor: &mut Processor<'a>,
    inverted: bool,
) {
    // let tspan = trace_span!("parsing macro condition");
    // let _tspan = tspan.enter();

    let name = p.current_text();

    if p.expect(PreprocessorToken::SimpleIdent, "an identifier", err) {
        // trace!(name = name, "condition");
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
            PreprocessorToken::CompilerDirective => match p.compiler_directive() {
                CompilerDirective::IfDef | CompilerDirective::IfNotDef if !PROCESS => depth += 1,

                CompilerDirective::EndIf if depth == 0 => {
                    p.bump();
                    break;
                }

                CompilerDirective::Else | CompilerDirective::ElseIf if PROCESS && depth == 0 => {
                    parse_if_body::<false, false>(p, err, processor);
                    return;
                }

                CompilerDirective::Else if CONSIDER_ELSE && depth == 0 => {
                    p.bump();
                    parse_if_body::<true, false>(p, err, processor);
                    break;
                }

                CompilerDirective::ElseIf if CONSIDER_ELSE && depth == 0 => {
                    p.bump();
                    parse_condition(p, err, processor, false);
                    break;
                }

                CompilerDirective::EndIf => depth -= 1,
                _ => (),
            },
            PreprocessorToken::Eof => {
                err.push(UnexpectedEof { expected: "`endif", span: p.current_span() });
                break;
            }
            _ => (),
        }

        if PROCESS {
            processor.process_token(p, err)
        } else {
            // ignore tokens
            p.bump()
        }
    }
}

pub(crate) fn parse_include<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
) -> Option<(&'a str, TextRange)> {
    // let tspan = trace_span!("parsing `include");
    // let _tspan = tspan.enter();

    let start = p.current_range().start();
    p.bump();
    let path = p.current_text();
    if p.expect(PreprocessorToken::StrLit, "a string literal", err) {
        Some((&path[1..path.len() - 1], TextRange::new(start, p.current_range().start())))
    } else {
        None
    }
}

// const MACRO_ARG_DEF_TERMINATOR_SET: TokenSet =
//     TokenSet::new(&[RawToken::ParenClose]).union(MACRO_TERMINATOR_SET);

// const MACRO_TERMINATOR_SET: TokenSet = TokenSet::new(&[RawToken::Eof, RawToken::Newline]);

pub(crate) fn parse_define<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    sm: &mut SourceMap,
    end: FullTokenIdx,
) -> Option<(&'a str, Macro<'a>)> {
    let start = p.current_range();
    p.bump();
    let name = p.current_text();
    // let tspan = trace_span!("parsing `define", name = name);
    // let _tspan = tspan.enter();

    let followed_by_bracket = p.followed_by_bracket_without_space();
    let mut success = p.expect(PreprocessorToken::SimpleIdent, "an identifier", err);
    let args = if followed_by_bracket {
        debug_assert!(p.at(PreprocessorToken::OpenParen));
        p.bump();
        let mut args = Vec::new();
        loop {
            if !p.before(end) {
                success = false;
                err.push(UnexpectedEof {
                    expected: ")",
                    span: CtxSpan { ctx: p.ctx(), range: p.current_range() },
                });
                break;
            }
            args.push(p.current_text());
            if !p.expect(PreprocessorToken::SimpleIdent, "an identifier", err) {
                success = false
            }

            if !p.before(end) {
                success = false;
                err.push(UnexpectedEof {
                    expected: ")",
                    span: CtxSpan { ctx: p.ctx(), range: p.current_range() },
                });
                break;
            }

            if p.eat(PreprocessorToken::CloseParen) {
                break;
            } else {
                let expect = p.expect(PreprocessorToken::Comma, ")", err);
                success &= expect;
            }
        }
        args
    } else {
        Vec::new()
    };

    let head = p.previous_range().end() - start.start();

    let mut body = Vec::new();

    while p.before(end) {
        parse_macro_token(p, err, &args, &mut body, sm, end)
    }
    // p.bump();

    let range = TextRange::new(start.start(), p.end_pos(end));
    if success {
        Some((
            name,
            Macro { head, body, arg_cnt: args.len(), span: p.current_span().with_range(range) },
        ))
    } else {
        None
    }
}

fn parse_macro_token<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    args: &[&'a str],
    dst: &mut Vec<ParsedToken<'a>>,
    sm: &mut SourceMap,
    end: FullTokenIdx,
) {
    // trace!(token = display(p.current()), "parse macro token");

    if p.at(PreprocessorToken::SimpleIdent) {
        if let Some(arg) = args.iter().position(|x| *x == p.current_text()) {
            // debug!(name = p.current_text(), idx = arg, "macro arg reference");
            p.bump();
            dst.push(ParsedToken {
                range: p.current_range(),
                kind: ParsedTokenKind::ArgumentReference(MacroArg::from(arg)),
            });
            return;
        }
    }

    if p.at(PreprocessorToken::CompilerDirective) {
        if p.compiler_directive() == CompilerDirective::Macro {
            let (call, range) = parse_macro_call(p, err, args, sm, end);
            dst.push(ParsedToken { range, kind: ParsedTokenKind::MacroCall(call) });
        } else {
            // TODO nicer error?
            err.push(PreprocessorDiagnostic::UnexpectedToken(CtxSpan {
                ctx: p.ctx,
                range: p.current_range(),
            }))
        }
        return;
    }

    p.bump_to_macro(dst, end, err)
}
pub(crate) fn parse_macro_call<'a>(
    p: &mut Parser<'a, '_>,
    err: &mut Diagnostics,
    args: &[&'a str],
    sm: &mut SourceMap,
    end: FullTokenIdx,
) -> (MacroCall<'a>, TextRange) {
    // let tspan = trace_span!("parsing macro call");
    // let _tspan = tspan.enter();

    let start = p.current_range();
    let name = &p.current_text()[1..];
    let followed = p.followed_by_bracket_without_space();
    p.bump();
    let arg_bindings = if followed {
        p.bump();
        let mut arg_bindings = TiVec::<MacroArg, _>::with_capacity(4);
        'outer: while !p.eat(PreprocessorToken::CloseParen) {
            let mut dst = Vec::with_capacity(18);
            let start = p.current_range().start();

            let mut depth = 0; // allow for nested brackets inside macros
            loop {
                match p.current() {
                    PreprocessorToken::OpenParen => depth += 1,
                    PreprocessorToken::CloseParen | PreprocessorToken::Comma if depth == 0 => {
                        break;
                    }
                    PreprocessorToken::CloseParen => depth -= 1,
                    _ if p.before(end) => (),
                    _ => {
                        let end = p.previous_range().end();
                        arg_bindings.push((dst, TextRange::new(start, end)));
                        err.push(UnexpectedEof {
                            expected: ")",
                            span: CtxSpan { ctx: p.ctx(), range: p.current_range() },
                        });
                        break 'outer;
                    }
                }
                parse_macro_token(p, err, args, &mut dst, sm, end)
            }

            p.eat(PreprocessorToken::Comma);

            let end = p.previous_range().end();
            arg_bindings.push((dst, TextRange::new(start, end)));
        }
        arg_bindings
    } else {
        TiVec::new()
    };

    let span = start.cover(p.previous_range());
    (MacroCall { name, arg_bindings }, span)
}
