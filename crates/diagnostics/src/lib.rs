/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use annotate_snippets::display_list::{DisplayList, FormatOptions};
pub use annotate_snippets::snippet::AnnotationType;
use annotate_snippets::snippet::{Annotation, Slice, Snippet, SourceAnnotation};
use beef::lean::Cow;
use core::fmt::Debug;
use core::fmt::Formatter;
use core::option::Option::Some;
use openvaf_session::sourcemap::span::SpanData;
use openvaf_session::sourcemap::{Location, SourceMap, SyntaxContext};
use openvaf_session::with_sourcemap;
use std::error::Error;
use std::fmt::{Display};
use std::ops::Deref;

pub mod lints;

#[allow(non_snake_case)]
#[doc(hidden)]
pub mod _macro_reexports {
    pub use once_cell;
    pub use paste;
}

pub fn format_list_with_seperator<C>(list: C, seperator: &'static str) -> ListFormatter<C> {
    ListFormatter(list, seperator, " or ")
}

pub fn format_list<C>(list: C) -> ListFormatter<C> {
    ListFormatter(list, "", " or ")
}

#[derive(Clone)]
pub struct ListFormatter<C>(pub C, pub &'static str, pub &'static str);

impl<C: Debug> Debug for ListFormatter<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}



impl<X: Display, T: Deref<Target=[X]>> Display for ListFormatter<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.0.deref() {
            [] => f.write_str(" "),
            [x] => {
                f.write_str(self.1)?;
                x.fmt(f)?;
                f.write_str(self.1)
            }
            [ref body @ .., second_last, last] => {
                for x in body {
                    f.write_str(self.1)?;
                    x.fmt(f)?;
                    f.write_str(self.1)?;
                    f.write_str(", ")?;
                }
                f.write_str(self.1)?;
                second_last.fmt(f)?;
                f.write_str(self.1)?;
                f.write_str(self.2)?;
                f.write_str(self.1)?;
                last.fmt(f)?;
                f.write_str(self.1)
            }
        }
    }
}

// impl<T: Display> Display for ListFormatter<Vec<T>> {
//     #[inline]
//     fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
//         ListFormatter(self.0.as_slice(), self.1, self.2).fmt(f)
//     }
// }
//
// impl<'lt, T: Display + Clone> Display for ListFormatter<beef::Cow<'lt, [T]>> {
//     #[inline]
//     fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
//         Display::fmt(&ListFormatter(self.0.as_ref(), self.1, self.2), f)
//     }
// }
//
// impl<'lt, T: Display + Clone> Display for ListFormatter<beef::lean::Cow<'lt, [T]>> {
//     #[inline]
//     fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
//         Display::fmt(&ListFormatter(self.0.as_ref(), self.1, self.2), f)
//     }
// }

pub type UserResult<T, Printer = StandardPrinter> = Result<T, UserMultiDiagnostic<Printer>>;
#[derive(Clone, Copy, Debug)]
pub enum Unsupported {
    ConstantFunctionCalls,
}

impl Display for Unsupported {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::ConstantFunctionCalls => {
                f.write_str("Function calls inside constant expressions")
            }
        }
    }
}

/// Slight performance save because most lints are actually static strings
/// Using a cow allows optional allocating
pub type Text = Cow<'static, str>;

#[derive(Clone, Debug)]
pub struct MultiDiagnostic<T: LibraryDiagnostic>(pub Vec<T>);

impl<T: LibraryDiagnostic> Display for MultiDiagnostic<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for err in &self.0 {
            Display::fmt(err, f)?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl<T: LibraryDiagnostic> MultiDiagnostic<T> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T: LibraryDiagnostic> Error for MultiDiagnostic<T> {}

impl<T: LibraryDiagnostic> MultiDiagnostic<T> {
    pub fn user_facing<Printer: DiagnosticSlicePrinter>(self) -> UserMultiDiagnostic<Printer> {
        let res = self
            .0
            .into_iter()
            .filter_map(|diagnostic| diagnostic.user_facing())
            .collect();

        UserMultiDiagnostic(res)
    }

    #[inline]
    pub fn add(&mut self, diagnostic: impl Into<T>) {
        self.0.push(diagnostic.into());
    }

    #[inline]
    pub fn add_all<S: LibraryDiagnostic + Into<T>>(&mut self, other: MultiDiagnostic<S>) {
        self.0.extend(other.0.into_iter().map(|err| err.into()))
    }
}

#[derive(Debug, Clone)]
pub struct UserMultiDiagnostic<Printer: DiagnosticSlicePrinter = StandardPrinter>(
    pub Vec<UserDiagnostic<Printer>>,
);

impl<'a, Printer: DiagnosticSlicePrinter> Display for UserMultiDiagnostic<Printer> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.0.is_empty() {
            f.write_str("\n")?;
        }
        for err in &self.0 {
            Display::fmt(err, f)?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl<Printer: DiagnosticSlicePrinter> Error for UserMultiDiagnostic<Printer> {}

pub trait LibraryDiagnostic: Display + Error {
    #[inline(always)]
    fn user_facing<Printer: DiagnosticSlicePrinter>(&self) -> Option<UserDiagnostic<Printer>> {
        if let Some(annotation_type) = self.annotation_type() {
            Some(UserDiagnostic {
                id: self.id(),
                title: self.title(),
                annotation_type,
                slices: self.slices(),
                footer: self.footer(),
                printer: Printer::default(),
            })
        } else {
            None
        }
    }

    #[inline(always)]
    fn id(&self) -> Option<&'static str> {
        None
    }

    #[inline(always)]
    fn title(&self) -> Text {
        Cow::owned(format!("{}", self))
    }

    fn annotation_type(&self) -> Option<AnnotationType>;

    fn slices(&self) -> Vec<DiagnosticSlice>;

    #[inline(always)]
    fn footer(&self) -> Vec<FooterItem> {
        Vec::new()
    }
}

#[derive(Debug, Clone)]
pub struct UserDiagnostic<Printer: DiagnosticSlicePrinter = StandardPrinter> {
    id: Option<&'static str>,
    title: Text,
    pub annotation_type: AnnotationType,

    slices: Vec<DiagnosticSlice>,
    footer: Vec<FooterItem>,

    printer: Printer,
}

impl<Printer: DiagnosticSlicePrinter> Display for UserDiagnostic<Printer> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        with_sourcemap(|sm| {
            // Space for expansion disclaimer
            let mut footer = Vec::with_capacity(self.footer.len() + 1);

            for footer_item in &self.footer {
                footer.push(Annotation {
                    id: footer_item.id,
                    label: Some(&footer_item.label),
                    annotation_type: footer_item.annotation_type,
                })
            }

            // Enough space so reallocation is very unlikely
            let mut slices = Vec::with_capacity(self.slices.len() * 2);

            for slice in &self.slices {
                Printer::render(slice, sm, &mut slices, &mut footer);
            }

            let snippet = Snippet {
                title: Some(Annotation {
                    id: self.id,
                    label: Some(&self.title),
                    annotation_type: self.annotation_type,
                }),
                footer,
                slices,
                opt: FormatOptions {
                    color: true,
                    anonymized_line_numbers: false,
                    margin: None,
                },
            };

            Display::fmt(&DisplayList::from(snippet), f)
        })
    }
}

impl<Printer: DiagnosticSlicePrinter + Debug> Error for UserDiagnostic<Printer> {}

#[derive(Debug, Clone)]
pub struct FooterItem {
    pub id: Option<&'static str>,
    pub label: Text,
    pub annotation_type: AnnotationType,
}

pub trait DiagnosticSlicePrinter: Default + Debug + Sync + Send {
    fn render<'a>(
        slice: &'a DiagnosticSlice,
        source_map: &'a SourceMap,
        dst: &mut Vec<Slice<'a>>,
        footer: &mut Vec<Annotation>,
    );
}

// TODO abstract using trait so we can add an expansion printer

#[derive(Clone, Debug)]
pub struct DiagnosticSlice {
    pub slice_span: SpanData,
    pub messages: Vec<(AnnotationType, Text, SpanData)>,
    pub fold: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub struct StandardPrinter;

impl StandardPrinter {
    ///# Panics
    /// if a message is not contained inside the `slice_span`
    fn render<'a>(slice: &'a DiagnosticSlice, source_map: &'a SourceMap, dst: &mut Vec<Slice<'a>>) {
        let DiagnosticSlice {
            slice_span,
            messages,
            fold,
        } = slice;

        let mut location = source_map.lookup_span(&slice_span);
        location.extend_to_line_ends(&source_map);

        let mut annotations = Vec::with_capacity(messages.len());

        for (annotation_type, text, mut span) in messages.iter() {
            while span.ctxt != slice_span.ctxt {
                span = if let Some(call_site) = span.ctxt.call_site() {
                    if span.hi == span.lo {
                        let mut call_site = call_site.data();
                        call_site.lo = call_site.hi;
                        call_site
                    } else {
                        call_site.data()
                    }
                } else {
                    let sub_location = source_map.lookup_span(&span);
                    let src = &source_map[sub_location.file].contents()[sub_location.range];
                    let file_name = source_map[sub_location.file].path.display();

                    let slice_src = &source_map[location.file].contents()[location.range];
                    let slice_file_name = source_map[location.file].path.display();
                    unreachable!(
                        "Message span \n\n {:#?} \n {} \n {} \n\n is not inside slice_span\n\n {:#?} \n {} \n {}",

                        span,
                        src,
                        file_name,

                        slice_span,
                        slice_src,
                        slice_file_name
                    )
                };
            }

            let Location { mut range, .. } = source_map.lookup_span(&span);
            if range.start == range.end {
                if range.end < location.file_src(source_map).len() {
                    range.end += 1;
                } else {
                    range.start -= 1;
                }
            }
            annotations.push(SourceAnnotation {
                range: (
                    range.start - location.range.start,
                    range.end - location.range.start,
                ),
                label: &text,
                annotation_type: *annotation_type,
            })
        }

        dst.push(Slice {
            source: location.src(source_map),
            line_start: location.line(source_map),
            origin: Some(location.origin(source_map)),
            annotations,
            fold: *fold,
        });
    }

    // TODO make this a type argument
    pub const BACKTRACE_HINT: &'static str = "This occurred inside a macro/file include! The expansion backtrace may be helpful (-b/--backtrace)";
}

impl DiagnosticSlicePrinter for StandardPrinter {
    fn render<'a>(
        slice: &'a DiagnosticSlice,
        source_map: &'a SourceMap,
        dst: &mut Vec<Slice<'a>>,
        footer: &mut Vec<Annotation>,
    ) {
        Self::render(slice, source_map, dst);
        if slice.slice_span.ctxt != SyntaxContext::ROOT {
            footer.push(Annotation {
                id: None,
                label: Some(Self::BACKTRACE_HINT),
                annotation_type: AnnotationType::Note,
            });
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub struct ExpansionPrinter;

impl DiagnosticSlicePrinter for ExpansionPrinter {
    ///# Panics
    /// if a message is not contained inside the `slice_span`
    fn render<'a>(
        slice: &'a DiagnosticSlice,
        source_map: &'a SourceMap,
        dst: &mut Vec<Slice<'a>>,
        _footer: &mut Vec<Annotation>,
    ) {
        StandardPrinter::render(slice, source_map, dst);

        let mut current = slice.slice_span;

        while let Some(call_site) = current.ctxt.call_site() {
            let call_site = call_site.data();
            let mut location = source_map.lookup_span(&call_site);
            let range = location.extend_to_line_ends(source_map);
            dst.push(Slice {
                source: location.src(source_map),
                line_start: location.line(source_map),
                origin: Some(location.origin(source_map)),
                annotations: vec![SourceAnnotation {
                    range: (range.start, range.end),
                    label: "Expanded from this compiler directive",
                    annotation_type: AnnotationType::Note,
                }],
                fold: false,
            });
            current = call_site;
        }
    }
}
