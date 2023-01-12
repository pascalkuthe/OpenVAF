use std::mem;
use std::sync::Arc;

use preprocessor::sourcemap::{CtxSpan, SourceContext, SourceMap};
use preprocessor::{SourceProvider, Token};
use rowan::{GreenNodeBuilder, Language};
use vfs::FileId;

use crate::syntax_node::{GreenNode, VerilogALanguage};
use crate::{SyntaxError, SyntaxKind, TextRange, TextSize, T};

pub(crate) struct SyntaxTreeBuilder<'a> {
    tokens: &'a [Token],
    text_pos: TextSize,
    token_pos: usize,

    state: State,

    errors: Vec<SyntaxError>,
    last_error: Option<SyntaxError>,

    inner: GreenNodeBuilder<'static>,

    db: &'a dyn SourceProvider,

    current_src: Arc<str>,
    panic: bool,
    err_depth: u32,
    sm: &'a SourceMap,
    ranges: Vec<(TextRange, SourceContext, TextSize)>,
    current_range: CtxSpan,
}

enum State {
    PendingStart,
    Normal,
    PendingFinish,
}

impl<'a> SyntaxTreeBuilder<'a> {
    pub(super) fn token(&mut self, kind: SyntaxKind) {
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
        ) || self.err_depth != u32::MAX;
        self.do_token(kind, span);
    }

    pub(super) fn start_node(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingStart => {
                self.inner.start_node(VerilogALanguage::kind_to_raw(kind));
                // No need to attach trivias to previous node: there is no
                // previous node.
                return;
            }
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => (),
        }

        if self.err_depth != u32::MAX {
            self.err_depth += 1
        } else if kind == SyntaxKind::ERROR {
            self.err_depth = 0
        } else {
            self.eat_trivias();
        }
        self.inner.start_node(VerilogALanguage::kind_to_raw(kind));
    }

    pub(super) fn finish_node(&mut self) {
        match mem::replace(&mut self.state, State::PendingFinish) {
            State::PendingStart => unreachable!(),
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => (),
        }
        if self.err_depth == 0 {
            if let Some(mut err) = self.last_error.take() {
                if let SyntaxError::UnexpectedToken { span, panic_end, .. } = &mut err {
                    if span.end() < self.text_pos {
                        *panic_end = Some(self.text_pos);
                    }
                }

                self.errors.push(err)
            }
            self.err_depth = u32::MAX;
        } else if self.err_depth != u32::MAX {
            self.err_depth -= 1;
        }
    }

    pub(super) fn error(&mut self, error: parser::SyntaxError) {
        let n_trivias =
            self.tokens[self.token_pos..].iter().take_while(|it| it.kind.is_trivia()).count();
        let leading_trivias = &self.tokens[self.token_pos..self.token_pos + n_trivias];
        let pos =
            self.text_pos + leading_trivias.iter().map(|it| it.span.range.len()).sum::<TextSize>();
        let parser::SyntaxError::UnexpectedToken { expected, found }: parser::SyntaxError = error;
        let missing_delimeter = found == T![end];
        if self.token_pos + n_trivias == self.tokens.len() {
            let expected_at = expected
                .data
                .iter()
                .any(|t| *t == T![;] || *t == T![')'])
                .then(|| TextRange::at(self.text_pos, 0.into()));
            let error = SyntaxError::UnexpectedToken {
                expected,
                found,
                span: TextRange::at(
                    self.text_pos,
                    self.tokens.last().map_or_else(|| TextSize::from(0), |t| t.span.range.len()),
                ),
                expected_at,
                missing_delimeter,
                panic_end: None,
            };
            self.errors.push(error);
            return;
        }
        let len = self.tokens[self.token_pos + n_trivias].span.range.len();

        let panic = mem::replace(&mut self.panic, true);
        if panic && !missing_delimeter || self.last_error.is_some() {
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
            panic_end: None,
        };
        self.last_error = Some(error)
    }

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
            inner: Default::default(),
            db,
            sm,
            current_src,
            ranges: Vec::with_capacity(128),
            current_range: CtxSpan {
                ctx: SourceContext::ROOT,
                range: TextRange::empty(TextSize::from(0)),
            },
            panic: false,
            err_depth: u32::MAX,
            errors: Vec::new(),
            last_error: None,
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
        (self.inner.finish(), self.errors, self.ranges)
    }

    fn eat_trivias(&mut self) {
        while let Some(&token) = self.tokens.get(self.token_pos) {
            if !token.kind.is_trivia() {
                break;
            }
            self.do_token(token.kind, token.span);
        }
    }

    fn do_token(&mut self, kind: SyntaxKind, span: CtxSpan) {
        let same_ctx = span.ctx == self.current_range.ctx;
        let is_continous = same_ctx && span.range.start() == self.current_range.range.end();
        if is_continous {
            self.current_range.range = self.current_range.range.cover(span.range);
        } else {
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
        self.inner.token(VerilogALanguage::kind_to_raw(kind), text);
    }
}
