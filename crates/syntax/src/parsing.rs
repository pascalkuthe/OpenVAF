mod text_tree_sink;
mod tokenstream_token_src;

use ::preprocessor::{preprocess, sourcemap::SourceContext, Preprocess, SourceProvider};
use vfs::FileId;
use rowan::{TextRange, TextSize};
use text_tree_sink::TextTreeSink;

use crate::{syntax_node::GreenNode, SyntaxError};

use self::tokenstream_token_src::TsTokenSource;

pub(crate) fn parse_text(
    sources: &dyn SourceProvider,
    root_file: FileId,
) -> (GreenNode, Vec<SyntaxError>, Vec<(TextRange, SourceContext, TextSize)>) {
    // TODO integrate the preprocessor into a salsa DB (with pre file parse trees and so forth) to
    // allow suggesting and renaming macros

    let Preprocess { ts, sm, .. }: Preprocess = preprocess(sources, root_file);

    let ts: Vec<_> = ts.iter().cloned().map(preprocessor::Token::from).collect();
    let mut token_source = TsTokenSource::new(&ts);
    let mut tree_sink = TextTreeSink::new(sources, root_file, &ts, &sm);

    parser::parse(&mut token_source, &mut tree_sink);

    let (tree, parser_errors, ctx_map) = tree_sink.finish();

    (tree, parser_errors, ctx_map)
}

// /// Returns `text` parsed as a `T` provided there are no parse errors.
// pub(crate) fn parse_text_fragment<T: AstNode>(
//     text: &str,
//     fragment_kind: parser::FragmentKind,
// ) -> Result<T, ()> {
//     let (tokens, lexer_errors) = tokenize(text);
//     if !lexer_errors.is_empty() {
//         return Err(());
//     }

//     let mut token_source = TextTokenSource::new(text, &tokens);
//     let mut tree_sink = TextTreeSink::new(text, &tokens);

//     // TextTreeSink assumes that there's at least some root node to which it can attach errors and
//     // tokens. We arbitrarily give it a SourceFile.
//     use parser::TreeSink;
//     tree_sink.start_node(SyntaxKind::SOURCE_FILE);
//     parser::parse_fragment(&mut token_source, &mut tree_sink, fragment_kind);
//     tree_sink.finish_node();

//     let (tree, parser_errors) = tree_sink.finish();
//     use parser::TokenSource;
//     if !parser_errors.is_empty() || token_source.current().kind != SyntaxKind::EOF {
//         return Err(());
//     }

//     SyntaxNode::new_root(tree).first_child().and_then(T::cast).ok_or(())
// }
