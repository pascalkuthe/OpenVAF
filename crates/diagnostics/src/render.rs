/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use core::fmt::Debug;
use core::fmt::Formatter;
use basedb::FileId;
use data_structures::sync::Arc;
use std::fmt::Display;
use std::ops::Range;

use basedb::BaseDB;
use data_structures::{
    text_size::{TextRange, TextSize},
    ScopedArcArea,
};
use syntax::sourcemap::{FileSpan, SourceContext, SourceMap};
use std::fmt;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, Slice, Snippet, SourceAnnotation},
};

use crate::{AnnotationType, Diagnostic, DiagnosticSlice, FooterItem, Text};

struct SourceArea<'a> {
    area: ScopedArcArea<str>,
    pub db: &'a dyn BaseDB,
}

impl<'a> SourceArea<'a> {
    pub fn new(db: &'a dyn BaseDB) -> Self {
        Self { area: ScopedArcArea::new(), db }
    }

    pub fn span_origin(&self, span: FileSpan) -> &str {
        let origin = span.path(&self.db.as_src_provider()).to_string();
        let origin = Arc::from(origin);
        self.area.ensure(origin)
    }

    pub fn span_text(&self, span: FileSpan) -> &str {
        &self.area.ensure(self.db.file_text(span.file).unwrap())[span.range]
    }
}
#[allow(non_snake_case)]
#[doc(hidden)]
pub mod _macro_reexports {
    pub use once_cell;
    pub use paste;
}

#[derive(Debug, Clone)]
pub struct DiagnosticsRender(pub Vec<DiagnosticRender>);

impl DiagnosticsRender {
    pub fn new() -> DiagnosticsRender {
        Self(Vec::new())
    }

    pub fn new_with<T: Diagnostic>(
        diagnostics: &[T],
        db: &dyn BaseDB,
        root_file: FileId,
    ) -> DiagnosticsRender {
        let mut res = DiagnosticsRender::new();
        res.add(diagnostics, db, root_file);
        res
    }

    pub fn add<T: Diagnostic>(&mut self, diagnostics: &[T], db: &dyn BaseDB, root_file: FileId) {
        self.0
            .extend(diagnostics.iter().filter_map(|diagnostic| diagnostic.to_render(db, root_file)))
    }

    pub fn display<'a>(
        &'a self,
        render_backtrace: bool,
        db: &'a dyn BaseDB,
        root_file: FileId,
    ) -> DiagnosticsDisplay<'a> {
        DiagnosticsDisplay { diagnostics: &self.0, render_backtrace, db, root_file }
    }

    pub fn is_critical(&self) -> bool {
        self.0.iter().any(|x| x.annotation_type == AnnotationType::Error)
    }
}

#[derive(Clone)]
pub struct DiagnosticsDisplay<'a> {
    diagnostics: &'a [DiagnosticRender],
    render_backtrace: bool,
    db: &'a dyn BaseDB,
    root_file: FileId,
}

impl<'a> Display for DiagnosticsDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let arena = SourceArea::new(self.db);
        for diagnostic in self.diagnostics {
            diagnostic.fmt(f, self.db, self.root_file, self.render_backtrace, &arena)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticRender {
    pub(crate) id: Option<&'static str>,
    pub(crate) title: Text,
    pub(crate) annotation_type: AnnotationType,

    pub(crate) slices: Vec<DiagnosticSlice>,
    pub(crate) footer: Vec<FooterItem>,
}

impl DiagnosticRender {
    pub fn display<'a>(
        &'a self,
        db: &'a dyn BaseDB,
        root_file: FileId,
        render_backtrace: bool,
    ) -> DiagnosticDisplay<'a> {
        DiagnosticDisplay { db, render_backtrace, diagnostic: self, root_file }
    }

    fn fmt(
        &self,
        f: &mut Formatter<'_>,
        db: &dyn BaseDB,
        root_file: FileId,
        render_backtrace: bool,
        arena: &SourceArea,
    ) -> fmt::Result {
        // Space for expansion disclaimer

        let mut footer = Vec::with_capacity(self.footer.len() + 1);

        for footer_item in &self.footer {
            footer.push(Annotation {
                id: footer_item.id,
                label: Some(&footer_item.label),
                annotation_type: footer_item.annotation_type,
            })
        }

        let sm = db.sourcemap(root_file);

        // Enough space so reallocation is very unlikely
        let mut slices = Vec::with_capacity(self.slices.len() * 2);

        for slice in &self.slices {
            slice.render(&sm, &mut slices, &arena);
            if render_backtrace {
                let mut current = slice.slice_span;

                while let Some(call_site) = current.ctx.call_site(&sm) {
                    let mut pos = call_site.to_file_span(&sm);
                    let range = pos.extend_to_line_end(&db.as_src_provider());
                    slices.push(Slice {
                        source: arena.span_text(pos),
                        line_start: db.line_col(pos).line as usize + 1,
                        origin: Some(arena.span_origin(pos)),
                        annotations: vec![SourceAnnotation {
                            range: (range.start().into(), range.end().into()),
                            label: "Expanded from this compiler directive",
                            annotation_type: AnnotationType::Note,
                        }],
                        fold: false,
                    });
                    current = call_site;
                }
            } else if slice.slice_span.ctx != SourceContext::ROOT {
                footer.push(Annotation {
                    id: None,
                    label: Some(BACKTRACE_HINT),
                    annotation_type: AnnotationType::Note,
                });
            }
        }

        let snippet = Snippet {
            title: Some(Annotation {
                id: self.id,
                label: Some(&self.title),
                annotation_type: self.annotation_type,
            }),
            footer,
            slices,
            opt: FormatOptions { color: true, anonymized_line_numbers: false, margin: None },
        };

        write!(f, "{}", DisplayList::from(snippet))
    }
}

pub struct DiagnosticDisplay<'a> {
    db: &'a dyn BaseDB,
    root_file: FileId,
    render_backtrace: bool,
    diagnostic: &'a DiagnosticRender,
}

impl<'a> fmt::Display for DiagnosticDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.diagnostic.fmt(
            f,
            self.db,
            self.root_file,
            self.render_backtrace,
            &SourceArea::new(self.db),
        )
    }
}

pub const BACKTRACE_HINT: & str = "This occurred inside a macro/file include! The expansion backtrace may be helpful (-b/--backtrace)";

impl DiagnosticSlice {
    /// Renders this slice into annotate snippet data structures
    fn render<'a>(&'a self, sm: &SourceMap, dst: &mut Vec<Slice<'a>>, arena: &'a SourceArea<'_>) {
        let Self { slice_span, messages, fold } = self;

        let mut pos = slice_span.to_file_span(sm);
        pos.extend_to_line_end(&arena.db.as_src_provider());

        let mut annotations = Vec::with_capacity(messages.len());

        for (annotation_type, text, mut span) in messages.iter() {
            while span.ctx != slice_span.ctx {
                let mut call_site = span.ctx.call_site(sm).expect(
                    "Illegal Diagnostic. Annotation was not contained inside parent slice!",
                );
                // special case for missing token
                if span.range.is_empty() {
                    call_site.range = TextRange::at(call_site.range.start(), 0.into())
                }

                span = call_site
            }

            let mut range = span.to_file_span(sm).range;

            // panic!("{:?} at {:?}",range, pos.range);
            // special handeling for empty spans (produced during parsing when smoething is missing)
            if range.is_empty() {
                if range.end() < pos.range.end() {
                    range = TextRange::at(range.start(), 1.into())
                } else {
                    range = TextRange::at(range.start().checked_sub(1.into()).unwrap(), 1.into())
                }
            }

            let range: Range<usize> = (range - pos.range.start()).into();

            annotations.push(SourceAnnotation {
                range: (range.start, range.end),
                label: &text,
                annotation_type: *annotation_type,
            })
        }

        dst.push(Slice {
            source: arena.span_text(pos),
            line_start: arena.db.line_col(pos).line as usize + 1,
            origin: Some(arena.span_origin(pos)),
            annotations,
            fold: *fold,
        });
    }
}
