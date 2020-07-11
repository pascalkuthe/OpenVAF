use index_vec::{define_index_type, IndexVec};

mod context;
pub mod span;

use crate::sourcemap::span::SpanData;
pub use context::SyntaxContext;
pub(crate) use context::SyntaxContextInterner;
use log::error;
use more_asserts::{assert_le, assert_lt, debug_assert_le, debug_assert_lt};
pub use span::Span;
pub(crate) use span::SpanInterner;
use std::cmp::Ordering;
use std::ops::{Index, Range};
use std::path::PathBuf;

//Indices

define_index_type! {
            pub struct BytePos = u32;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<BytePos {}>";

            IMPL_RAW_CONVERSIONS = true;

            // Checks are done when files are added
            DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);
}

define_index_type! {
            pub struct FileId = u16;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<FileId {}>";

            IMPL_RAW_CONVERSIONS = true;

            // Checks are done when files are aadded
            DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);
}

define_index_type! {
            pub struct CharPos = u32;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<CharPos {}>";

            IMPL_RAW_CONVERSIONS = true;

            // Checks are done when files are added
            DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Location {
    pub file: FileId,
    pub range: Range<usize>,
}

impl Location {
    #[inline]
    #[must_use]
    pub fn src<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        &self.file_src(source_map)[self.range.clone()]
    }

    #[inline]
    #[must_use]
    pub fn file_src<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        &source_map[self.file].contents
    }

    #[inline]
    #[must_use]
    pub fn origin<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        source_map[self.file]
            .path
            .to_str()
            .unwrap_or("File name is not valid UTF8")
    }

    pub fn extend_to_line_ends(&mut self, source_map: &SourceMap) -> Range<usize> {
        let src = source_map[self.file].contents();
        // Map or is used because the start of slice is inclusive but we dont want to include the newline
        let start = src[..self.range.start].rfind('\n').map_or(0, |pos| pos + 1);
        let end = src[self.range.end..]
            .find('\n')
            .map_or(src.len(), |pos| pos + self.range.end);

        let rel_start = self.range.start - start;
        let rel_end = self.range.end - start;
        self.range = start..end;
        rel_start..rel_end
    }

    #[inline]
    #[must_use]
    pub fn line(&self, source_map: &SourceMap) -> usize {
        let text = &source_map[self.file].contents()[..self.range.start];
        bytecount::count(text.as_bytes(), b'\n') + 1
    }
}

pub type Expansion = Vec<Location>;

#[derive(Debug, Default, Clone)]
pub struct SourceMap {
    files: IndexVec<FileId, File>,
}

impl SourceMap {
    #[inline]
    pub fn new_with_mainfile(main_file: impl Into<PathBuf>) -> std::io::Result<(Self, FileId)> {
        let mut res = Self {
            files: IndexVec::with_capacity(8),
        };

        let main_file = res.add_file_from_fs(main_file.into())?;
        Ok((res, main_file))
    }

    /// Translate a Position to a user facing location
    #[must_use]
    pub fn lookup(&self, lo: BytePos, hi: BytePos) -> Location {
        debug_assert_le!(lo, hi);

        let (file_id, file) = self
            .files
            .binary_search_by(|file| {
                if file.hi < lo {
                    Ordering::Less
                } else if file.lo > lo {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .ok()
            .map(|i| (i, &self.files[i]))
            .expect("Mapping unknown source location");

        debug_assert_lt!(hi, file.hi);

        Location {
            file: file_id,
            range: ((lo - file.lo).index()..(hi - file.lo).index()),
        }
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    #[must_use]
    pub fn lookup_span(&self, span: &SpanData) -> Location {
        self.lookup(span.lo, span.hi)
    }

    #[must_use]
    pub fn lookup_expansion(&self, span: Span) -> Expansion {
        self.lookup_expansion_until(span, SyntaxContext::ROOT)
    }

    #[must_use]
    pub fn lookup_expansion_until(&self, span: Span, until: SyntaxContext) -> Expansion {
        let mut res = Vec::with_capacity(8);
        let mut current = span.data();

        loop {
            res.push(self.lookup(current.lo, current.hi));
            if current.ctxt == until {
                return res;
            }
            match current.ctxt.call_site() {
                Some(call_site) => current = call_site.data(),
                None => return res,
            }
        }
    }

    pub fn add_file(&mut self, path: PathBuf, contents: String) -> FileId {
        let lo = self
            .files
            .last()
            .map_or(BytePos::from_raw_unchecked(0), |last| last.hi);
        let hi = lo.index() + contents.len();

        if hi > BytePos::MAX_INDEX {
            error!(
                "Only projects up to 4GB are allowed! (reached {}B after adding {})",
                hi,
                path.display()
            );
            panic!(
                "Project files exceed 4GB (reached {}B after adding {})",
                hi,
                path.display()
            )
        }

        if self.files.len_idx() > FileId::MAX_INDEX {
            error!(
                "Atmost {} files are supported per project. Exceeded this limit when adding {}",
                u16::MAX,
                path.display()
            );
            panic!(
                "Atmost {} files are supported per project. Exceeded this limit when adding {}",
                u16::MAX,
                path.display()
            )
        }

        let file = File {
            lo,
            hi: BytePos::from_usize_unchecked(hi),
            path,
            contents,
        };

        self.files.push(file)
    }

    pub fn add_file_from_fs(&mut self, path: PathBuf) -> std::io::Result<FileId> {
        // FIXME read straight to Bump allocator
        // BLOCK allocator abstraction in rust

        // FIXME Properly handle tabs
        // BLOCK https://github.com/rust-lang/annotate-snippets-rs/issues/25
        let contents = std::fs::read_to_string(&path)?.replace("\t", " ");
        Ok(self.add_file(path, contents))
        // we transmute to a static string so we can have self referential dat
    }
}

impl Index<FileId> for SourceMap {
    type Output = File;

    fn index(&self, index: FileId) -> &Self::Output {
        &self.files[index]
    }
}

#[derive(Clone, Debug)]
pub struct File {
    pub path: PathBuf,
    contents: String,
    pub lo: BytePos,
    pub hi: BytePos,
}

impl File {
    pub fn contents(&self) -> &str {
        &self.contents
    }
}
