//! See [`TextTreeSink`].

use std::mem;
use std::sync::Arc;

use parser::TreeSink;
use preprocessor::sourcemap::{CtxSpan, SourceContext, SourceMap};
use preprocessor::{SourceProvider, Token};
use vfs::FileId;

use crate::syntax_node::{GreenNode, SyntaxTreeBuilder};
use crate::{SyntaxError, SyntaxKind, TextRange, TextSize, T};

/// Bridges the parser with our specific syntax tree representation.
///
/// `TextTreeSink` also handles attachment of trivia (whitespace) to nodes.
pub(crate) struct TextTreeSink<'a> {
    tokens: &'a [Token],
    text_pos: TextSize,
    token_pos: usize,

    state: State,

    inner: SyntaxTreeBuilder,

    db: &'a dyn SourceProvider,

    current_src: Arc<str>,
    panic: bool,
    at_err: bool,
    sm: &'a SourceMap,
    ranges: Vec<(TextRange, SourceContext, TextSize)>,
    current_range: CtxSpan,
}

enum State {
    PendingStart,
    Normal,
    PendingFinish,
}

impl<'a> TreeSink for TextTreeSink<'a> {
    fn token(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingStart => unreachable!(),
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => (),
        }
        self.eat_trivias();
        let span = self.tokens[self.token_pos].span;
        self.panic &= !matches!(
            kind,
            T![;] | T![end] | T![endnature] | T![endmodule] | T![enddiscipline] | T![endfunction]
        ) | self.at_err;
        self.do_token(kind, span);
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingStart => {
                self.inner.start_node(kind);
                // No need to attach trivias to previous node: there is no
                // previous node.
                return;
            }
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => (),
        }

        self.at_err = kind == SyntaxKind::ERROR;

        // let n_trivias =
        //     self.tokens[self.token_pos..].iter().take_while(|it| it.is_trivia()).count();
        // let leading_trivias = &self.tokens[self.token_pos..self.token_pos + n_trivias];
        // let mut trivia_end = self.text_pos + leading_trivias.iter().map(|it| it.len()).sum();

        // let n_attached_trivias = {
        //     let leading_trivias = leading_trivias.iter().rev().map(|it| {
        //         let next_end = trivia_end - it.len;
        //         let range = TextRange::new(next_end, trivia_end);
        //         trivia_end = next_end;
        //         (it.kind, &self.current_src[range])
        //     });
        //     n_attached_trivias(kind, leading_trivias)
        // };
        // self.eat_n_trivias(n_trivias);
        self.eat_trivias();
        self.inner.start_node(kind);
        // self.eat_n_trivias(n_attached_trivias);
    }

    fn finish_node(&mut self) {
        match mem::replace(&mut self.state, State::PendingFinish) {
            State::PendingStart => unreachable!(),
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => (),
        }
        self.at_err = false
    }

    fn error(&mut self, error: parser::SyntaxError) {
        let n_trivias =
            self.tokens[self.token_pos..].iter().take_while(|it| it.kind.is_trivia()).count();
        let leading_trivias = &self.tokens[self.token_pos..self.token_pos + n_trivias];
        let pos =
            self.text_pos + leading_trivias.iter().map(|it| it.span.range.len()).sum::<TextSize>();
        let len = self.tokens[self.token_pos + n_trivias].span.range.len();
        let parser::SyntaxError::UnexpectedToken { expected, found }: parser::SyntaxError = error;
        let missing_delimeter = found == T![end];

        let panic = mem::replace(&mut self.panic, true);
        if panic && !missing_delimeter {
            return;
        }

        let expected_at = expected
            .data
            .iter()
            .any(|t| *t == T![;] || *t == T![')'])
            .then(|| TextRange::at(self.text_pos, 0.into()));
        let error = SyntaxError::UnexpectedToken {
            expected,
            found,
            span: TextRange::at(pos, len),
            expected_at,
            missing_delimeter,
        };
        self.inner.error(error)
    }
}

impl<'a> TextTreeSink<'a> {
    pub(super) fn new(
        db: &'a dyn SourceProvider,
        root_file: FileId,
        tokens: &'a [Token],
        sm: &'a SourceMap,
    ) -> Self {
        let current_src = db.file_text(root_file).unwrap_or_else(|_| Arc::from(""));
        Self {
            tokens,
            text_pos: 0.into(),
            token_pos: 0,
            state: State::PendingStart,
            inner: SyntaxTreeBuilder::default(),
            db,
            sm,
            current_src,
            ranges: Vec::with_capacity(128),
            current_range: CtxSpan {
                ctx: SourceContext::ROOT,
                range: TextRange::empty(TextSize::from(0)),
            },
            panic: false,
            at_err: false,
        }
    }

    pub(super) fn finish(
        mut self,
    ) -> (GreenNode, Vec<SyntaxError>, Vec<(TextRange, SourceContext, TextSize)>) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingFinish => {
                self.eat_trivias();
                self.inner.finish_node()
            }
            State::PendingStart | State::Normal => unreachable!(),
        }
        let start = self.ranges.last().map_or(0.into(), |(range, _, _)| range.end());
        let range = TextRange::new(start, self.text_pos);
        self.ranges.push((range, self.current_range.ctx, self.current_range.range.start()));
        let (root_node, errors) = self.inner.finish_raw();
        (root_node, errors, self.ranges)
    }

    fn eat_trivias(&mut self) {
        while let Some(&token) = self.tokens.get(self.token_pos) {
            if !token.kind.is_trivia() {
                break;
            }
            self.do_token(token.kind, token.span);
        }
    }

    // fn eat_n_trivias(&mut self, n: usize) {
    //     for _ in 0..n {
    //         let token = self.tokens[self.token_pos].unwrap_token();
    //         assert!(token.kind.is_trivia());
    //         self.do_token(token.kind, token.len);
    //     }
    // }

    fn do_token(&mut self, kind: SyntaxKind, span: CtxSpan) {
        let same_ctx = span.ctx == self.current_range.ctx;
        let is_continous = same_ctx && span.range.end() == self.current_range.range.start();

        if !is_continous {
            let start = self.ranges.last().map_or(0.into(), |(range, _, _)| range.end());
            let range = TextRange::new(start, self.text_pos);
            let old_range = mem::replace(&mut self.current_range, span);
            self.ranges.push((range, old_range.ctx, old_range.range.start()))
        }

        if !same_ctx {
            // We are in a different ctx and therefore the text comes from somewhere else...
            // Switch the src code
            // Unwrap is okay here because the file was already read succesffully by he preprocessor or the SourceContext wouldn't exist
            let decl = self.sm.ctx_data(span.ctx).decl;
            let src = self.db.file_text(decl.file).unwrap();
            self.current_src = src;
        }

        let range = span.to_file_span(self.sm).range;
        let text = &self.current_src[range];
        self.text_pos += range.len();
        self.token_pos += 1;
        self.inner.token(kind, text);
    }
}

// TODO this would be nice but is pretty complicated with src code switchting
// fn n_attached_trivias<'a>(
//     kind: SyntaxKind,
//     trivias: impl Iterator<Item = (SyntaxKind, &'a str)>,
// ) -> usize {
//     match kind {
//         NATURE_DECL | MODULE_DECL | DISCIPLINE_DECL => {
//             let mut res = 0;
//             let mut trivias = trivias.enumerate().peekable();

//             while let Some((i, (kind, text))) = trivias.next() {
//                 match kind {
//                     WHITESPACE if text.contains("\n\n") => {
//                         break;
//                     }
//                     COMMENT => {
//                         res = i + 1;
//                     }
//                     _ => (),
//                 }
//             }
//             res
//         }
//         _ => 0,
//     }
// }
