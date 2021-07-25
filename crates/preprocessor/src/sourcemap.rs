/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe.  This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::{convert::TryFrom, ops::RangeBounds};

use data_structures::{
    index_vec::{define_index_type, index_vec, IndexVec},
    sync::Arc,
    text_size::{TextRange, TextSize},
    HashMap,
};
use vfs::{FileId, VfsPath};

use std::convert::TryInto;

use crate::SourceProvider;

/// Representents a continous range of Text inside a particular file.
/// This representation is the only representation that can be used to actually obtain the src code
/// of a particular TextRange
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct FileSpan {
    pub range: TextRange,
    pub file: FileId,
}

impl FileSpan {
    pub fn with_file(self, file: FileId) -> FileSpan {
        FileSpan { range: self.range, file }
    }

    pub fn with_range(self, range: TextRange) -> FileSpan {
        FileSpan { range, file: self.file }
    }
    pub fn with_len(self, len: TextSize) -> FileSpan {
        FileSpan { range: TextRange::at(self.range.start(), len), file: self.file }
    }

    pub fn with_subrange(self, relative_range: TextRange) -> FileSpan {
        let range = relative_range + self.range.start();
        assert!(
            range.end() <= self.range.end(),
            "subrange {:?} must fit into the total range {:?}",
            relative_range,
            self.range
        );
        FileSpan { range, file: self.file }
    }

    #[inline]
    #[must_use]
    pub fn path(&self, sources: &dyn SourceProvider) -> VfsPath {
        sources.file_path(self.file)
    }

    #[inline]
    #[must_use]
    pub fn file_text(&self, db: &dyn SourceProvider) -> Arc<str> {
        db.file_text(self.file).expect("FileSpan was created with invalid FileId")
        // Spans are only created after a file was read succesfully
    }

    pub fn extend_to_line_end(&mut self, db: &dyn SourceProvider) -> TextRange {
        let src = self.file_text(db);

        let range = self.range;
        // Map or is used because the start of slice is inclusive but we dont want to include the newline
        let line_start =
            src[..range.start().into()].rfind('\n').map_or(0, |pos| pos + 1).try_into().unwrap();
        let line_end = src[range.end().into()..]
            .find('\n')
            .map_or(TextSize::of(&*src), |pos| TextSize::try_from(pos).unwrap() + range.end());

        self.range = TextRange::new(line_start, line_end);
        range - line_start
    }
}

/// A CtxSpan refers a contious range of Text a SourceContext (macro expansion or file).
/// The range is relative to the start of the particular context so thgat ranges can be changed
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct CtxSpan {
    pub range: TextRange,
    /// Information about where the code came from
    pub ctx: SourceContext,
}

impl CtxSpan {
    pub fn with_ctx(self, ctx: SourceContext, sm: &SourceMap) -> CtxSpan {
        CtxSpan { range: self.range, ctx }
    }
    pub fn with_range(self, range: TextRange) -> CtxSpan {
        CtxSpan { range, ctx: self.ctx }
    }

    pub fn cover(self, range: TextRange) -> CtxSpan {
        CtxSpan { range: self.range.cover(range), ctx: self.ctx }
    }

    pub fn with_len(self, len: TextSize) -> CtxSpan {
        CtxSpan { range: TextRange::at(self.range.start(), len), ctx: self.ctx }
    }

    #[inline]
    pub fn dummy() -> CtxSpan {
        Self { range: TextRange::empty(0.into()), ctx: SourceContext::ROOT }
    }

    /// Extends `self` to reach to also include `to`
    /// Note that this is a non-trivial algorithm and should be called conservatively
    #[must_use]
    pub fn extend(self, other: CtxSpan, sm: &SourceMap) -> CtxSpan {
        if self.ctx == other.ctx {
            Self { range: self.range.cover(other.range), ctx: self.ctx }
        } else {
            let (ctx, range1, range2) = sm.lowest_common_parent(self.ctx, other.ctx);
            let range1 = range1.unwrap_or(self.range);
            let range2 = range2.unwrap_or(other.range);

            Self { range: range1.cover(range2), ctx }
        }
    }

    #[must_use]
    pub fn lookup_expansion(&self, sm: &SourceMap) -> Vec<FileSpan> {
        sm.lookup_expansion(*self)
    }

    #[must_use]
    pub fn to_file_span(self, sm: &SourceMap) -> FileSpan {
        sm.ctx_tree[self.ctx].decl.with_subrange(self.range)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct SourceMap {
    ctx_tree: IndexVec<SourceContext, SourceContextData>,
    // ranges: Vec<(TextRange, SourceContext, isize)>,
}

impl SourceMap {
    pub(crate) fn new(root_file: FileId, root_file_len: TextSize) -> SourceMap {
        SourceMap {
            ctx_tree: index_vec![SourceContextData {
                decl: FileSpan { range: TextRange::up_to(root_file_len), file: root_file },
                call_site: None,
            }],
        }
    }

    #[must_use]
    pub fn lookup_expansion(&self, span: CtxSpan) -> Vec<FileSpan> {
        self.lookup_expansion_until(span, SourceContext::ROOT)
    }

    #[must_use]
    pub fn lookup_expansion_until(&self, span: CtxSpan, until: SourceContext) -> Vec<FileSpan> {
        let mut res = Vec::with_capacity(8);
        let mut current = span;

        loop {
            res.push(current.to_file_span(self));
            if current.ctx == until {
                return res;
            }
            match self.ctx_tree[current.ctx].call_site {
                Some(call_site) => current = call_site,
                None => return res,
            }
        }
    }

    /// Finds the smallest `SourceContext` that contains both `self` and `other`
    /// # Note
    /// This is a fairly efficient algorithm but still requires a building a HashMap
    /// so avoid calling this if not necessary.
    ///
    /// Furthremore this algorithm does not special case self==other
    /// This is a special case that tends to be of interest to the caller and and as such
    /// is checked there. Even if this is not of interest to you this is a fairly common case
    /// so you should special case it for better performance
    #[must_use]
    pub fn lowest_common_parent(
        &self,
        sctx1: SourceContext,
        sctx2: SourceContext,
    ) -> (SourceContext, Option<TextRange>, Option<TextRange>) {
        // self.index() is used as a capacity because it is the lowest upper bound we know
        let mut ancestors = HashMap::with_capacity(sctx2.index());
        ancestors.insert(sctx1, None);
        let mut current = sctx1;
        while let Some(call_site) = self.ctx_tree[current].call_site {
            current = call_site.ctx;
            ancestors.insert(current, Some(call_site.range));
        }

        current = sctx2;
        let mut current_call_span = None;

        loop {
            if let Some(call_span) = ancestors.get(&current).copied() {
                return (current, call_span, current_call_span);
            }

            let span = self.ctx_tree[current].call_site.expect("CTXT paths dont intersect at root");
            current = span.ctx;
            current_call_span = Some(span.range);
        }
    }

    pub fn ctx_data(&self, ctx: SourceContext) -> &SourceContextData {
        &self.ctx_tree[ctx]
    }

    pub(crate) fn add_ctx(&mut self, decl: FileSpan, call_site: CtxSpan) -> SourceContext {
        self.ctx_tree.push(SourceContextData { decl, call_site: Some(call_site) })
    }
}

define_index_type! {
            pub struct SourceContext = u32;
            DISPLAY_FORMAT = "syntax_ctxt{}";
            DEBUG_FORMAT = "syntax_ctxt{}";
            IMPL_RAW_CONVERSIONS = true;
}

impl SourceContext {
    pub const ROOT: Self = Self::from_raw_unchecked(0);

    pub fn call_site(self, sm: &SourceMap) -> Option<CtxSpan> {
        sm.ctx_tree[self].call_site
    }
}

// TODO find rowan like solution to make this cheap to update
// (currently requires full preprocessor and parser rerun)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct SourceContextData {
    pub decl: FileSpan,
    pub call_site: Option<CtxSpan>,
}
