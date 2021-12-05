//! The VerilogA parser.
//!
//! The parser doesn't know about concrete representation of tokens and syntax
//! trees. Abstract `TokenSource` and `TreeSink` traits are used instead. As a
//! consequence, this crates does not contain a lexer.
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
mod parser;
pub use error::SyntaxError;
pub(crate) use token_set::TokenSet;
//pub(crate) use token_set::TokenSet;
pub(crate) use tokens::parser::SyntaxKind;

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

/// `TreeSink` abstracts details of a particular syntax tree implementation.
pub trait TreeSink {
    /// Adds new token to the current branch.
    fn token(&mut self, kind: SyntaxKind);

    /// Start new branch and make it current.
    fn start_node(&mut self, kind: SyntaxKind);

    /// Finish current branch and restore previous
    /// branch as current.
    fn finish_node(&mut self);

    fn error(&mut self, error: SyntaxError);
}

fn parse_from_tokens<F>(token_source: &mut dyn TokenSource, tree_sink: &mut dyn TreeSink, f: F)
where
    F: FnOnce(&mut parser::Parser),
{
    let mut p = parser::Parser::new(token_source);
    f(&mut p);
    let events = p.finish();
    event::process(tree_sink, events);
}

/// Parse given tokens into the given sink as a rust file.
pub fn parse(token_source: &mut dyn TokenSource, tree_sink: &mut dyn TreeSink) {
    parse_from_tokens(token_source, tree_sink, grammar::source_file);
}

// #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
// pub enum FragmentKind {
//     Path,
//     Expr,
//     Statement,
//     StatementOptionalSemi,
//     Type,
//     Pattern,
//     Item,
//     Block,
//     Visibility,
//     MetaItem,
//
//     // These kinds are used when parsing the result of expansion
//     // FIXME: use separate fragment kinds for macro inputs and outputs?
//     Items,
//     Statements,
//
//     Attr,
// }

// pub fn parse_fragment(
//     token_source: &mut dyn TokenSource,
//     tree_sink: &mut dyn TreeSink,
//     fragment_kind: FragmentKind,
// ) {
//     let parser: fn(&'_ mut parser::Parser) = match fragment_kind {
//         FragmentKind::Path => grammar::fragments::path,
//         FragmentKind::Expr => grammar::fragments::expr,
//         FragmentKind::Type => grammar::fragments::type_,
//         FragmentKind::Pattern => grammar::fragments::pattern_single,
//         FragmentKind::Item => grammar::fragments::item,
//         FragmentKind::Block => grammar::fragments::block_expr,
//         FragmentKind::Visibility => grammar::fragments::opt_visibility,
//         FragmentKind::MetaItem => grammar::fragments::meta_item,
//         FragmentKind::Statement => grammar::fragments::stmt,
//         FragmentKind::StatementOptionalSemi => grammar::fragments::stmt_optional_semi,
//         FragmentKind::Items => grammar::fragments::macro_items,
//         FragmentKind::Statements => grammar::fragments::macro_stmts,
//         FragmentKind::Attr => grammar::fragments::attr,
//     };
//     parse_from_tokens(token_source, tree_sink, parser)
// }
//
// /// A parsing function for a specific braced-block.
// pub struct Reparser(fn(&mut parser::Parser));
//
// impl Reparser {
//     /// If the node is a braced block, return the corresponding `Reparser`.
//     pub fn for_node(
//         node: SyntaxKind,
//         first_child: Option<SyntaxKind>,
//         parent: Option<SyntaxKind>,
//     ) -> Option<Reparser> {
//         grammar::reparser(node, first_child, parent).map(Reparser)
//     }
//
//     /// Re-parse given tokens using this `Reparser`.
//     ///
//     /// Tokens must start with `{`, end with `}` and form a valid brace
//     /// sequence.
//     pub fn parse(self, token_source: &mut dyn TokenSource, tree_sink: &mut dyn TreeSink) {
//         let Reparser(r) = self;
//         let mut p = parser::Parser::new(token_source);
//         r(&mut p);
//         let events = p.finish();
//         event::process(tree_sink, events);
//     }
// }
