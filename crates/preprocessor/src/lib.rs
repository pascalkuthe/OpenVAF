/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */


use data_structures::sync::Arc;
use sourcemap::SourceMap;

use crate::processor::Processor;
pub use crate::tokenstream::{Token, TokenKind, TokenStream};
use diagnostics::PreprocessorDiagnostic;
pub use vfs::{FileId, VfsPath};
use tracing::trace_span;

pub mod diagnostics;
mod grammar;
mod lexer;
mod parser;
mod processor;
pub mod sourcemap;
mod token_set;
mod tokenstream;

type Text = Arc<str>;
type ScopedTextArea = data_structures::ScopedArea<Text>;
type Diagnostics = Vec<PreprocessorDiagnostic>;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Preprocess {
    pub ts: Arc<TokenStream>,
    pub sm: Arc<SourceMap>,
    pub diagnostics: Arc<Diagnostics>,
}


/// # Panics
/// This function panics if called multiple times in the same OpenVAF session
pub fn preprocess(sources: &dyn SourceProvider, file: FileId) -> Preprocess {
    let span = trace_span!("preprocessor", main_file = display(sources.file_path(file)));
    let _scope = span.enter();

    let storage = ScopedTextArea::new();
    let (ts, diagnostics, sm) = match Processor::new(&storage, file, sources) {
        Ok(mut processor) => {
            let (ts, diagnostics) = processor.run(file);
            (ts, diagnostics, processor.source_map)
        }
        Err(FileReadError::Io) => (
            vec![],
            vec![PreprocessorDiagnostic::IoErrorRoot(sources.file_path(file))],
            SourceMap::new(file, 0.into()),
        ),
        Err(FileReadError::InvalidTextFormat) => (
            vec![],
            vec![PreprocessorDiagnostic::InvalidTextFormatRoot(sources.file_path(file))],
            SourceMap::new(file, 0.into()),
        ),
    };

    Preprocess { ts: Arc::new(ts), diagnostics: Arc::new(diagnostics), sm: Arc::new(sm) }
}


#[derive(PartialEq, Eq,Hash,Debug,Clone, Copy)]
pub enum FileReadError{
    Io,
    InvalidTextFormat,
}

pub trait SourceProvider {
    fn include_dirs(&self, root_file: FileId)->Arc<[VfsPath]>;
    fn macro_flags(&self, file_root: FileId)->Arc<[Arc<str>]>;

    fn file_text(&self, file: FileId)->Result<Arc<str>,FileReadError>;
    fn file_path(&self, file: FileId)->VfsPath;
    fn file_id(&self, path: VfsPath)->FileId;
}
