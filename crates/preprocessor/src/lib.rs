use std::sync::Arc;

use diagnostics::PreprocessorDiagnostic;
use sourcemap::{CtxSpan, SourceMap};
use vfs::{FileId, FileReadError, VfsPath};

use crate::processor::Processor;
// use tracing::trace_span;

pub mod diagnostics;
mod grammar;
mod parser;
mod processor;
pub mod sourcemap;

mod scoped_arc_arena;
#[cfg(test)]
#[rustfmt::skip]
mod tests;

type Text = Arc<str>;
type ScopedTextArea = scoped_arc_arena::ScopedArea<Text>;
type Diagnostics = Vec<PreprocessorDiagnostic>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Preprocess {
    pub ts: Arc<Vec<Token>>,
    pub sm: Arc<SourceMap>,
    pub diagnostics: Arc<Diagnostics>,
}

/// # Panics
/// This function panics if called multiple times in the same OpenVAF session
pub fn preprocess(sources: &dyn SourceProvider, file: FileId) -> Preprocess {
    // let span = trace_span!("preprocessor", main_file = display(sources.file_path(file)));
    // let _scope = span.enter();

    let storage = ScopedTextArea::new();
    let (ts, diagnostics, sm) = match Processor::new(&storage, file, sources) {
        Ok(mut processor) => {
            let (ts, diagnostics) = processor.run(file);
            (ts, diagnostics, processor.source_map)
        }
        Err(FileReadError::Io(error)) => (
            vec![],
            vec![PreprocessorDiagnostic::FileNotFound {
                file: sources.file_path(file),
                error,
                span: None,
            }],
            SourceMap::new(file, 0.into()),
        ),
        Err(FileReadError::InvalidTextFormat(err)) => (
            vec![],
            vec![PreprocessorDiagnostic::InvalidTextFormat {
                file: sources.file_path(file),
                span: None,
                err,
            }],
            SourceMap::new(file, 0.into()),
        ),
    };

    Preprocess { ts: Arc::new(ts), diagnostics: Arc::new(diagnostics), sm: Arc::new(sm) }
}

pub trait SourceProvider {
    fn include_dirs(&self, root_file: FileId) -> Arc<[VfsPath]>;
    fn macro_flags(&self, file_root: FileId) -> Arc<[Arc<str>]>;

    fn file_text(&self, file: FileId) -> Result<Arc<str>, FileReadError>;
    fn file_path(&self, file: FileId) -> VfsPath;
    fn file_id(&self, path: VfsPath) -> FileId;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: CtxSpan,
    pub kind: tokens::parser::SyntaxKind,
}
