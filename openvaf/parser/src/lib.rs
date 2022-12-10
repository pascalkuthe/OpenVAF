//! The VerilogA parser.
//!
//! The parser doesn't know about concrete representation of tokens and syntax
//! trees. Abstract `TokenSource` and `TreeSink` traits are used instead. As a
//! consequence, this openvaf does not contain a lexer.
//!
//! The `Parser` struct from the `parser` module is a cursor into the sequence
//! of tokens. Parsing routines use `Parser` to inspect current state and
//! advance the parsing.
//!
//! The actual parsing happens in the `grammar` module.

#[macro_use]
mod token_set;
mod error;
mod event;
mod grammar;
mod output;
mod parser;

pub use error::SyntaxError;
use stdx::pretty;
pub(crate) use token_set::TokenSet;
//pub(crate) use token_set::TokenSet;
pub(crate) use tokens::parser::SyntaxKind;

pub use crate::output::{Output, Step};

/// `TokenSource` abstracts the source of the tokens parser operates on.
///
/// Hopefully this will allow us to treat text and token trees in the same way!
pub trait TokenSource {
    fn current(&self) -> Token;

    /// Lookahead n token
    fn lookahead_nth(&self, n: usize) -> Token;

    /// bump cursor to next token
    fn bump(&mut self);
}

pub type Token = SyntaxKind;

/// Parse given tokens into the given sink as a rust file.
pub fn parse(tokens: &[SyntaxKind]) -> Output {
    let mut p = parser::Parser::new(tokens);
    grammar::source_file(&mut p);
    event::process(p.finish())
}

pub struct Error {
    pub expected: pretty::List<Vec<Token>>,
    pub found: Token,
}
