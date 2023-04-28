pub mod ast;
mod error;
pub mod name;
mod parsing;
mod ptr;
mod syntax_node;
mod token_text;
mod validation;

use std::cmp::Ordering;
use std::marker::PhantomData;
use std::sync::Arc;

pub use ast::AstNode;
pub use error::SyntaxError;
pub use preprocessor::diagnostics::PreprocessorDiagnostic;
use preprocessor::sourcemap::{CtxSpan, FileSpan, SourceContext};
pub use preprocessor::{preprocess, sourcemap, Preprocess, SourceProvider};
pub use ptr::{AstPtr, SyntaxNodePtr};
pub use rowan::{
    Direction, GreenNode, NodeOrToken, SyntaxText, TextRange, TextSize, TokenAtOffset, WalkEvent,
};
pub use syntax_node::{SyntaxNode, SyntaxToken};
pub use token_text::TokenText;
pub use tokens::{SyntaxKind, T};
use vfs::FileId;

/// `Parse` is the result of the parsing: a syntax tree and a collection of
/// errors.
///
/// Note that we always produce a syntax tree, even for completely invalid
/// files.
#[derive(Debug, PartialEq, Eq)]
pub struct Parse<T> {
    green: GreenNode,
    errors: Arc<Vec<SyntaxError>>,
    pub ctx_map: Arc<Vec<(TextRange, SourceContext, TextSize)>>,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Parse<T> {
    fn clone(&self) -> Parse<T> {
        Parse {
            green: self.green.clone(),
            errors: self.errors.clone(),
            ctx_map: self.ctx_map.clone(),
            _ty: PhantomData,
        }
    }
}

impl<T> Parse<T> {
    fn new(
        green: GreenNode,
        errors: Vec<SyntaxError>,
        ctx_map: Vec<(TextRange, SourceContext, TextSize)>,
    ) -> Parse<T> {
        Parse { green, errors: Arc::new(errors), ctx_map: Arc::new(ctx_map), _ty: PhantomData }
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    fn find_ctx_range(&self, global_pos: TextSize) -> (TextRange, SourceContext, TextSize) {
        self.ctx_map
            .binary_search_by(|(range, _, _)| {
                if range.end() <= global_pos {
                    Ordering::Less
                } else if global_pos < range.start() {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .ok()
            .map(|i| self.ctx_map[i])
            .expect("No range in the sourcemap covers the requested position")
    }
    pub fn ctx(&self, global_pos: TextSize) -> (SourceContext, TextSize) {
        let (range, ctx, _offset) = self.find_ctx_range(global_pos);
        let relative_pos = global_pos - range.start();
        (ctx, relative_pos)
    }

    pub fn to_ctx_span(&self, range: TextRange, sm: &sourcemap::SourceMap) -> CtxSpan {
        let (ctx_range1, ctx1, offset1) = self.find_ctx_range(range.start());
        if range.end() <= ctx_range1.end() {
            // Special case common case that start and end are in the same SourceContext
            CtxSpan { range: range - ctx_range1.start() + offset1, ctx: ctx1 }
        } else {
            let (ctx_range2, ctx2, offset2) = self.find_ctx_range(range.end());
            let start = range.start() - ctx_range1.start() + offset1;
            let end = range.end() - ctx_range2.start() + offset2;
            if ctx1 == ctx2 {
                CtxSpan { range: TextRange::new(start, end), ctx: ctx1 }
            } else {
                let (ctx, range1, range2) = sm.lowest_common_parent(
                    CtxSpan { range: TextRange::empty(start), ctx: ctx1 },
                    CtxSpan { range: TextRange::empty(end), ctx: ctx2 },
                );

                CtxSpan { range: range1.cover(range2), ctx }
            }
        }
    }

    pub fn to_file_span(&self, range: TextRange, sm: &sourcemap::SourceMap) -> FileSpan {
        self.to_ctx_span(range, sm).to_file_span(sm)
    }
}

impl<T: AstNode> Parse<T> {
    pub fn to_syntax(self) -> Parse<SyntaxNode> {
        Parse { green: self.green, errors: self.errors, ctx_map: self.ctx_map, _ty: PhantomData }
    }

    pub fn tree(&self) -> T {
        T::cast(self.syntax_node()).unwrap()
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    pub fn ok(self) -> Result<T, Arc<Vec<SyntaxError>>> {
        if self.errors.is_empty() {
            Ok(self.tree())
        } else {
            Err(self.errors)
        }
    }
}

impl Parse<SyntaxNode> {
    pub fn cast<N: AstNode>(self) -> Option<Parse<N>> {
        if N::cast(self.syntax_node()).is_some() {
            Some(Parse {
                green: self.green,
                errors: self.errors,
                ctx_map: self.ctx_map,
                _ty: PhantomData,
            })
        } else {
            None
        }
    }
}

impl Parse<SourceFile> {
    // pub fn debug_dump(&self) -> String {
    //     use std::fmt::Write;

    //     let mut buf = format!("{:#?}", self.tree().syntax());
    //     for err in self.errors.iter() {
    //         writeln!(buf, "error {:?}: {}", err.range(), err).unwrap();
    //     }
    //     buf
    // }

    // pub fn reparse(&self, indel: &Indel) -> Parse<SourceFile> {
    //     self.full_reparse(indel)
    // self.incremental_reparse(indel).unwrap_or_else(|| self.full_reparse(indel))
    // }

    // fn incremental_reparse(&self, indel: &Indel) -> Option<Parse<SourceFile>> {
    //     // FIXME: validation errors are not handled here
    //     parsing::incremental_reparse(self.tree().syntax(), indel, self.errors.to_vec()).map(
    //         |(green_node, errors, _reparsed_range)| Parse {
    //             green: green_node,
    //             errors: Arc::new(errors),
    //             _ty: PhantomData,
    //         },
    //     )
    // }

    // fn full_reparse(&self, indel: &Indel) -> Parse<SourceFile> {
    //     let mut text = self.tree().syntax().text().to_string();
    //     indel.apply(&mut text);
    //     SourceFile::parse(&text)
    // }
}

/// `SourceFile` represents a parse tree for a single Rust file.
pub use crate::ast::SourceFile;

impl SourceFile {
    pub fn parse(
        db: &dyn SourceProvider,
        root_file: FileId,
        preprocess: &Preprocess,
    ) -> Parse<SourceFile> {
        let (green, mut errors, ctx_map) = parsing::parse_text(db, root_file, preprocess);
        let root = SyntaxNode::new_root(green.clone());

        validation::validate(&root, &mut errors);

        assert_eq!(root.kind(), SyntaxKind::SOURCE_FILE);

        Parse::new(green, errors, ctx_map)
    }
}

/// Matches a `SyntaxNode` against an `ast` type.
///
/// # Example:
///
/// ```ignore
/// match_ast! {
///     match node {
///         ast::CallExpr(it) => { ... },
///         ast::MethodCallExpr(it) => { ... },
///         ast::MacroCall(it) => { ... },
///         _ => None,
///     }
/// }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:ident) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = ast::$ast::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}
