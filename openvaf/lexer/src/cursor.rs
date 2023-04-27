use std::str::Chars;

use text_size::TextSize;
use tokens::lexer::{Token, TokenKind};

/// Peekable iterator over a char sequence.
///
/// Next characters can be peeked via `nth_char` method,
/// and position can be shifted forward via `bump` method.
pub(crate) struct Cursor<'a> {
    initial_len: TextSize,
    chars: Chars<'a>,
    #[cfg(debug_assertions)]
    prev: char,
    dst: Vec<Token>,
    marker: Option<usize>,
}

pub(crate) const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub(crate) fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            initial_len: TextSize::of(input),
            chars: input.chars(),
            #[cfg(debug_assertions)]
            prev: EOF_CHAR,
            // Tokens are on average a length of about 4
            dst: Vec::with_capacity(input.len() / 4),
            marker: None,
        }
    }

    /// Returns the last eaten symbol (or `'\0'` in release builds).
    /// (For debug assertions only.)
    pub(crate) fn prev(&self) -> char {
        #[cfg(debug_assertions)]
        {
            self.prev
        }

        #[cfg(not(debug_assertions))]
        {
            EOF_CHAR
        }
    }

    /// Returns nth character relative to the current cursor position.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    fn nth_char(&self, n: usize) -> char {
        self.chars().nth(n).unwrap_or(EOF_CHAR)
    }

    /// Peeks the next symbol from the input stream without consuming it.
    pub(crate) fn first(&self) -> char {
        self.nth_char(0)
    }

    /// Peeks the second symbol from the input stream without consuming it.
    pub(crate) fn second(&self) -> char {
        self.nth_char(1)
    }

    /// Checks if there is nothing more to consume.
    pub(crate) fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Returns amount of already consumed symbols.
    fn len_consumed(&self) -> TextSize {
        self.initial_len - TextSize::of(self.chars.as_str())
    }

    /// Returns a `Chars` iterator over the remaining characters.
    fn chars(&self) -> Chars<'a> {
        self.chars.clone()
    }

    /// Moves to the next character.
    pub(crate) fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }

        Some(c)
    }

    pub(crate) fn finish_token(&mut self, kind: TokenKind) {
        let len = self.len_consumed();
        self.initial_len -= len;
        self.dst.push(Token { kind, len })
    }

    pub(crate) fn finish_marker(&mut self) -> bool {
        if let Some(marker) = self.marker.take() {
            self.dst[marker].kind = TokenKind::Define { end: self.dst.len() };
            true
        } else {
            false
        }
    }

    pub(crate) fn set_marker(&mut self) -> TokenKind {
        // we do not allow nested define statements
        if self.marker.is_none() {
            self.marker = Some(self.dst.len())
        }
        TokenKind::IllegalDefine
    }

    pub(crate) fn finish(mut self) -> Vec<Token> {
        self.finish_marker();
        self.dst
    }
}
