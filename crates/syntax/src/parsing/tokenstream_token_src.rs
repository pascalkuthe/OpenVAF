//! See `TsTokenSource` docs.

use std::convert::TryInto;

use crate::SyntaxKind::{self, EOF};
use parser::TokenSource;

use super::preprocessor::Token;

/// Implementation of `parser::TokenSource` that takes tokens from source code text.
pub(crate) struct TsTokenSource {
    /// token and its start position (non-whitespace/comment tokens)
    /// ```non-rust
    ///  struct Foo;
    ///  ^------^--^-
    ///  |      |    \________
    ///  |      \____         \
    ///  |           \         |
    ///  (struct, 0) (Foo, 7) (;, 10)
    /// ```
    /// `[(struct, 0), (Foo, 7), (;, 10)]`
    token_offset_pairs: Vec<(SyntaxKind, usize)>,

    /// Current token and position
    curr: (SyntaxKind, usize),
}

impl TokenSource for TsTokenSource {
    fn current(&self) -> SyntaxKind {
        self.curr.0
    }

    fn lookahead_nth(&self, n: usize) -> SyntaxKind {
        mk_token(self.curr.1 + n, &self.token_offset_pairs)
    }

    fn bump(&mut self) {
        if self.curr.0 == EOF {
            return;
        }

        let pos = self.curr.1 + 1;
        self.curr = (mk_token(pos, &self.token_offset_pairs), pos);
    }
}

fn mk_token(pos: usize, token_offset_pairs: &[(SyntaxKind, usize)]) -> SyntaxKind {
    token_offset_pairs.get(pos).map_or(EOF, |(token, _)| *token)
}

impl TsTokenSource {
    /// Generate input from tokens(expect comment and whitespace).
    pub(super) fn new(raw_tokens: &[Token]) -> TsTokenSource {
        let token_offset_pairs: Vec<_> = raw_tokens
            .iter()
            .enumerate()
            .filter_map(|(pos, t)| {
                (*t).try_into()
                    .ok()
                    .and_then(|t: Token| (!t.kind.is_trivia()).then(|| (t.kind, pos)))
            })
            .collect();
        let curr = (mk_token(0, &token_offset_pairs), 0);
        TsTokenSource { token_offset_pairs, curr }
    }
}
