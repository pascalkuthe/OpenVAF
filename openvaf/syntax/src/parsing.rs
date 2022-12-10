mod tree_builder;

use ::preprocessor::sourcemap::SourceContext;
use ::preprocessor::{Preprocess, SourceProvider};
use rowan::{TextRange, TextSize};
use vfs::FileId;

use crate::parsing::tree_builder::SyntaxTreeBuilder;
use crate::syntax_node::GreenNode;
use crate::SyntaxError;

pub(crate) fn parse_text(
    sources: &dyn SourceProvider,
    root_file: FileId,
    Preprocess { ts, sm, .. }: &Preprocess,
) -> (GreenNode, Vec<SyntaxError>, Vec<(TextRange, SourceContext, TextSize)>) {
    // tokens without whitespaces/comments
    let parser_tokens: Vec<_> = ts
        .iter()
        .filter_map(|token| {
            if token.kind.is_trivia() {
                return None;
            }
            Some(token.kind)
        })
        .collect();
    let mut builder = SyntaxTreeBuilder::new(sources, root_file, ts, sm);
    for step in parser::parse(&parser_tokens).iter() {
        match step {
            parser::Step::Token { kind } => builder.token(kind),
            parser::Step::Enter { kind } => builder.start_node(kind),
            parser::Step::Exit => builder.finish_node(),
            parser::Step::Error { err } => builder.error(err.clone()),
        }
    }

    let (tree, parser_errors, ctx_map) = builder.finish();

    (tree, parser_errors, ctx_map)
}
