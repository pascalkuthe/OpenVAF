//! Lexical analysis for .clif files.

use crate::error::Location;
use mir::{Block, Value};
use std::str::CharIndices;

#[cfg(test)]
mod tests;

/// A Token returned from the `Lexer`.
///
/// Some variants may contains references to the original source text, so the `Token` has the same
/// lifetime as the source.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'a> {
    Comment(&'a str),
    LPar,                 // '('
    RPar,                 // ')'
    LBrace,               // '{'
    RBrace,               // '}'
    LBracket,             // '['
    RBracket,             // ']'
    Minus,                // '-'
    Plus,                 // '+'
    Comma,                // ','
    Dot,                  // '.'
    Colon,                // ':'
    Equal,                // '='
    Not,                  // '!'
    Arrow,                // '->'
    Float(&'a str),       // Floating point immediate
    Integer(&'a str),     // Integer immediate
    Value(Value),         // v12, v7
    Block(Block),         // block3
    Name(&'a str),        // %9arbitrary_alphanum, %x3, %0, %function ...
    String(&'a str),      // "arbitrary quoted string with no escape" ...
    FuncRef(u32),         // fn2
    HexSequence(&'a str), // #89AF
    Identifier(&'a str),  // Unrecognized identifier (opcode, enumerator, ...)
    SourceLoc(&'a str),   // @00c7
}

/// A `Token` with an associated location.
#[derive(Debug, PartialEq, Eq)]
pub struct LocatedToken<'a> {
    pub token: Token<'a>,
    pub location: Location,
}

/// Wrap up a `Token` with the given location.
fn token(token: Token, loc: Location) -> Result<LocatedToken, LocatedError> {
    Ok(LocatedToken { token, location: loc })
}

/// An error from the lexical analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexError {
    InvalidChar,
    InvalidEscapeSequence,
}

/// A `LexError` with an associated Location.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocatedError {
    pub error: LexError,
    pub location: Location,
}

/// Wrap up a `LexError` with the given location.
fn error<'a>(error: LexError, loc: Location) -> Result<LocatedToken<'a>, LocatedError> {
    Err(LocatedError { error, location: loc })
}

/// Get the number of decimal digits at the end of `s`.
fn trailing_digits(s: &str) -> usize {
    // It's faster to iterate backwards over bytes, and we're only counting ASCII digits.
    s.as_bytes().iter().rev().take_while(|&&b| b.is_ascii_digit()).count()
}

/// Pre-parse a supposed entity name by splitting it into two parts: A head of lowercase ASCII
/// letters and numeric tail.
pub fn split_entity_name(name: &str) -> Option<(&str, u32)> {
    let (head, tail) = name.split_at(name.len() - trailing_digits(name));
    if tail.len() > 1 && tail.starts_with('0') {
        None
    } else {
        tail.parse().ok().map(|n| (head, n))
    }
}

/// Lexical analysis.
///
/// A `Lexer` reads text from a `&str` and provides a sequence of tokens.
///
/// Also keep track of a line number for error reporting.
///
pub struct Lexer<'a> {
    // Complete source being processed.
    source: &'a str,

    // Iterator into `source`.
    chars: CharIndices<'a>,

    // Next character to be processed, or `None` at the end.
    lookahead: Option<char>,

    // Index into `source` of lookahead character.
    pos: usize,

    // Current line number.
    line_number: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut lex =
            Self { source: s, chars: s.char_indices(), lookahead: None, pos: 0, line_number: 1 };
        // Advance to the first char.
        lex.next_ch();
        lex
    }

    // Advance to the next character.
    // Return the next lookahead character, or None when the end is encountered.
    // Always update cur_ch to reflect
    fn next_ch(&mut self) -> Option<char> {
        if self.lookahead == Some('\n') {
            self.line_number += 1;
        }
        match self.chars.next() {
            Some((idx, ch)) => {
                self.pos = idx;
                self.lookahead = Some(ch);
            }
            None => {
                self.pos = self.source.len();
                self.lookahead = None;
            }
        }
        self.lookahead
    }

    // Get the location corresponding to `lookahead`.
    fn loc(&self) -> Location {
        Location { line_number: self.line_number }
    }

    // Starting from `lookahead`, are we looking at `prefix`?
    fn looking_at(&self, prefix: &str) -> bool {
        self.source[self.pos..].starts_with(prefix)
    }

    // Starting from `lookahead`, are we looking at a number?
    fn looking_at_numeric(&self) -> bool {
        if let Some(c) = self.lookahead {
            if matches!(c, '0'..='9' | '-' | '+' | '.')
                || self.looking_at("NaN")
                || self.looking_at("Inf")
                || self.looking_at("sNaN")
            {
                return true;
            }
        }
        false
    }

    // Scan a single-char token.
    fn scan_char(&mut self, tok: Token<'a>) -> Result<LocatedToken<'a>, LocatedError> {
        assert_ne!(self.lookahead, None);
        let loc = self.loc();
        self.next_ch();
        token(tok, loc)
    }

    // Scan a multi-char token.
    fn scan_chars(
        &mut self,
        count: usize,
        tok: Token<'a>,
    ) -> Result<LocatedToken<'a>, LocatedError> {
        let loc = self.loc();
        for _ in 0..count {
            assert_ne!(self.lookahead, None);
            self.next_ch();
        }
        token(tok, loc)
    }

    /// Get the rest of the current line.
    /// The next token returned by `next()` will be from the following lines.
    pub fn rest_of_line(&mut self) -> &'a str {
        let begin = self.pos;
        loop {
            match self.next_ch() {
                None | Some('\n') => return &self.source[begin..self.pos],
                _ => {}
            }
        }
    }

    // Scan a comment extending to the end of the current line.
    fn scan_comment(&mut self) -> Result<LocatedToken<'a>, LocatedError> {
        let loc = self.loc();
        let text = self.rest_of_line();
        token(Token::Comment(text), loc)
    }

    // Scan a number token which can represent either an integer or floating point number.
    //
    // Accept the following forms:
    //
    // - `10`: Integer
    // - `-10`: Integer
    // - `0xff_00`: Integer
    // - `0.0`: Float
    // - `0x1.f`: Float
    // - `-0x2.4`: Float
    // - `0x0.4p-34`: Float
    //
    // This function does not filter out all invalid numbers. It depends in the context-sensitive
    // decoding of the text for that. For example, the number of allowed digits in an `Ieee32` and
    // an `Ieee64` constant are different.
    fn scan_number(&mut self) -> Result<LocatedToken<'a>, LocatedError> {
        let begin = self.pos;
        let loc = self.loc();
        let mut is_float = false;

        // Skip a leading sign.
        match self.lookahead {
            Some('-') => {
                self.next_ch();
                if !self.looking_at_numeric() {
                    // If the next characters won't parse as a number, we return Token::Minus
                    return token(Token::Minus, loc);
                }
            }
            Some('+') => {
                self.next_ch();
                if !self.looking_at_numeric() {
                    // If the next characters won't parse as a number, we return Token::Plus
                    return token(Token::Plus, loc);
                }
            }
            _ => {}
        }

        // Check for NaNs with payloads.
        if self.looking_at("NaN:") || self.looking_at("sNaN:") {
            // Skip the `NaN:` prefix, the loop below won't accept it.
            // We expect a hexadecimal number to follow the colon.
            while self.next_ch() != Some(':') {}
            is_float = true;
        } else if self.looking_at("NaN") || self.looking_at("Inf") {
            // This is Inf or a default quiet NaN.
            is_float = true;
        }

        // Look for the end of this number. Detect the radix point if there is one.
        loop {
            match self.next_ch() {
                Some('.') => is_float = true,
                Some('-' | '_' | '0'..='9' | 'a'..='z' | 'A'..='Z') => {}
                _ => break,
            }
        }
        let text = &self.source[begin..self.pos];
        if is_float {
            token(Token::Float(text), loc)
        } else {
            token(Token::Integer(text), loc)
        }
    }

    // Scan a 'word', which is an identifier-like sequence of characters beginning with '_' or an
    // alphabetic char, followed by zero or more alphanumeric or '_' characters.
    fn scan_word(&mut self) -> Result<LocatedToken<'a>, LocatedError> {
        let begin = self.pos;
        let loc = self.loc();

        assert!(self.lookahead == Some('_') || self.lookahead.unwrap().is_ascii_alphabetic());
        while let Some('_' | '0'..='9' | 'a'..='z' | 'A'..='Z') = self.next_ch() {}
        let text = &self.source[begin..self.pos];

        // Look for numbered well-known entities like block15, v45, ...
        token(
            split_entity_name(text)
                .and_then(|(prefix, number)| Self::numbered_entity(prefix, number))
                .unwrap_or(Token::Identifier(text)),
            loc,
        )
    }

    // If prefix is a well-known entity prefix and suffix is a valid entity number, return the
    // decoded token.
    fn numbered_entity(prefix: &str, number: u32) -> Option<Token<'a>> {
        match prefix {
            "v" => Value::with_number(number).map(Token::Value),
            "block" => Block::with_number(number).map(Token::Block),
            "fn" => Some(Token::FuncRef(number)),
            _ => None,
        }
    }

    fn scan_name(&mut self) -> Result<LocatedToken<'a>, LocatedError> {
        let loc = self.loc();
        let begin = self.pos + 1;

        assert_eq!(self.lookahead, Some('%'));

        while matches!(self.next_ch(), Some('_' | '0'..='9' | 'a'..='z' | 'A'..='Z')) {}

        let end = self.pos;
        token(Token::Name(&self.source[begin..end]), loc)
    }

    /// Scan for a multi-line quoted string with  escape character.
    fn scan_string(&mut self) -> Result<LocatedToken<'a>, LocatedError> {
        let loc = self.loc();
        let begin = self.pos + 1;

        assert_eq!(self.lookahead, Some('"'));

        while let Some(c) = self.next_ch() {
            if c == '\\' {
                if let Some(ch) = self.next_ch() {
                    if !matches!(ch, '0' | 'n' | 'r' | 't' | '\\' | '"') {
                        return error(LexError::InvalidEscapeSequence, self.loc());
                    }
                } else {
                    break;
                }
            }
            if c == '"' {
                break;
            }
        }

        let end = self.pos;
        if self.lookahead != Some('"') {
            return error(LexError::InvalidChar, self.loc());
        }
        self.next_ch();
        token(Token::String(&self.source[begin..end]), loc)
    }

    fn scan_hex_sequence(&mut self) -> Result<LocatedToken<'a>, LocatedError> {
        let loc = self.loc();
        let begin = self.pos + 1;

        assert_eq!(self.lookahead, Some('#'));

        while let Some(c) = self.next_ch() {
            if !char::is_digit(c, 16) {
                break;
            }
        }

        let end = self.pos;
        token(Token::HexSequence(&self.source[begin..end]), loc)
    }

    fn scan_srcloc(&mut self) -> Result<LocatedToken<'a>, LocatedError> {
        let loc = self.loc();
        let begin = self.pos + 1;

        assert_eq!(self.lookahead, Some('@'));

        while let Some(c) = self.next_ch() {
            if !char::is_digit(c, 16) {
                break;
            }
        }

        let end = self.pos;
        token(Token::SourceLoc(&self.source[begin..end]), loc)
    }

    /// Get the next token or a lexical error.
    ///
    /// Return None when the end of the source is encountered.
    pub fn next(&mut self) -> Option<Result<LocatedToken<'a>, LocatedError>> {
        loop {
            let loc = self.loc();
            return match self.lookahead {
                None => None,
                Some(';') => Some(self.scan_comment()),
                Some('(') => Some(self.scan_char(Token::LPar)),
                Some(')') => Some(self.scan_char(Token::RPar)),
                Some('{') => Some(self.scan_char(Token::LBrace)),
                Some('}') => Some(self.scan_char(Token::RBrace)),
                Some('[') => Some(self.scan_char(Token::LBracket)),
                Some(']') => Some(self.scan_char(Token::RBracket)),
                Some(',') => Some(self.scan_char(Token::Comma)),
                Some('.') => Some(self.scan_char(Token::Dot)),
                Some(':') => Some(self.scan_char(Token::Colon)),
                Some('=') => Some(self.scan_char(Token::Equal)),
                Some('!') => Some(self.scan_char(Token::Not)),
                Some('+' | '0'..='9') => Some(self.scan_number()),
                Some('-') => {
                    if self.looking_at("->") {
                        Some(self.scan_chars(2, Token::Arrow))
                    } else {
                        Some(self.scan_number())
                    }
                }
                Some('a'..='z' | 'A'..='Z') => {
                    if self.looking_at("NaN") || self.looking_at("Inf") {
                        Some(self.scan_number())
                    } else {
                        Some(self.scan_word())
                    }
                }
                Some('%') => Some(self.scan_name()),
                Some('"') => Some(self.scan_string()),
                Some('#') => Some(self.scan_hex_sequence()),
                Some('@') => Some(self.scan_srcloc()),
                // all ascii whitespace
                Some(' ' | '\x09'..='\x0d') => {
                    self.next_ch();
                    continue;
                }
                _ => {
                    // Skip invalid char, return error.
                    self.next_ch();
                    Some(error(LexError::InvalidChar, loc))
                }
            };
        }
    }
}
