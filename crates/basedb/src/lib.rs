/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub mod line_index;
pub mod lints;

use data_structures::sync::Arc;

use line_index::{LineCol, LineIndex};
use lints::{Lint, LintLevel, LintResolver};
use data_structures::index_vec::IndexSlice;
use syntax::{
    sourcemap::{FileSpan, SourceMap},
    FileReadError, Parse, Preprocess, SourceFile, SourceProvider,
};
use vfs::VfsPath;
use salsa::Durability;

pub use vfs::FileId;

pub trait FileReader {
    fn read(&self, file: FileId) -> Result<Arc<str>, FileReadError>;
    fn file_path(&self, file: FileId) -> VfsPath;
    fn file_id(&self, path: VfsPath) -> FileId;
}

#[salsa::query_group(BaseDatabase)]
pub trait BaseDB: LintResolver + FileReader + salsa::Database {
    fn parse(&self, root_file: FileId) -> Parse<SourceFile>;
    fn preprocess(&self, root_file: FileId) -> Preprocess;
    #[salsa::transparent]
    fn sourcemap(&self, root_file: FileId) -> Arc<SourceMap>;

    #[salsa::input]
    fn global_lint_overwrites(
        &self,
        root_file: FileId,
    ) -> Arc<IndexSlice<Lint, [Option<LintLevel>]>>;

    #[salsa::input]
    fn include_dirs(&self, root_file: FileId) -> Arc<[VfsPath]>;
    #[salsa::input]
    fn macro_flags(&self, file_root: FileId) -> Arc<[Arc<str>]>;

    /// Returns the line index of a file
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
    #[salsa::transparent]
    fn line_col(&self, span: FileSpan) -> LineCol;

    fn file_text(&self, file: FileId) -> Result<Arc<str>, FileReadError>;
}

fn line_index(db: &dyn BaseDB, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
    Arc::new(LineIndex::new(text.as_ref().map_or("", |t| &*t)))
}

fn line_col(db: &dyn BaseDB, span: FileSpan) -> LineCol {
    db.line_index(span.file).line_col(span.range.start())
}

fn parse(db: &dyn BaseDB, root_file: FileId) -> Parse<SourceFile> {
    SourceFile::parse(&db.as_src_provider(), root_file)
}

fn preprocess(db: &dyn BaseDB, root_file: FileId) -> Preprocess {
    syntax::preprocess(&db.as_src_provider(), root_file)
}

fn file_text(db: &dyn BaseDB, file: FileId) -> Result<Arc<str>, FileReadError> {
    db.salsa_runtime().report_synthetic_read(Durability::LOW);
    db.read(file)
}

fn sourcemap(db: &dyn BaseDB, root_file: FileId) -> Arc<SourceMap> {
    db.preprocess(root_file).sm
}

pub const CONSTANTS_PATHS: [&str; 3] = ["constants.vams", "constants.va", "constants.h"];
pub const DISCIPLINES_PATHS: [&str; 3] = ["disciplines.vams", "disciplines.va", "disciplines.h"];
pub const STANDARD_FLAGS: [&str; 3] = [" __OPENVAF__", "__VAMS__", "__VAMS_COMPACT_MODELING__"];

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

impl<'a> dyn BaseDB + 'a {
    pub fn as_src_provider(&self) -> impl SourceProvider + '_ {
        SourceProviderDelegate(self)
    }
}

struct SourceProviderDelegate<'a>(&'a dyn BaseDB);

impl<'a> SourceProvider for SourceProviderDelegate<'_> {
    #[inline(always)]
    fn include_dirs(&self, root_file: FileId) -> Arc<[VfsPath]> {
        self.0.include_dirs(root_file)
    }

    #[inline(always)]
    fn macro_flags(&self, root_file: FileId) -> Arc<[Arc<str>]> {
        self.0.macro_flags(root_file)
    }

    #[inline(always)]
    fn file_text(&self, file: FileId) -> Result<Arc<str>, FileReadError> {
        self.0.file_text(file)
    }

    #[inline(always)]
    fn file_path(&self, file: FileId) -> VfsPath {
        self.0.file_path(file)
    }

    #[inline(always)]
    fn file_id(&self, path: VfsPath) -> FileId {
        self.0.file_id(path)
    }
}


#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl ::salsa::InternKey for $name {
            fn from_intern_id(v: ::salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> ::salsa::InternId {
                self.0
            }
        }
    };
}
