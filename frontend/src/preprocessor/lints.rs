/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::diagnostic::{DiagnosticSlice, FooterItem, Text};
use crate::lints::{builtin, Lint, LintDiagnostic};
use crate::sourcemap::Span;
use crate::symbol::Symbol;
use annotate_snippets::snippet::AnnotationType;
use core::fmt::Formatter;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct MacroOverwritten {
    pub old: Span,
    pub new: Span,
    pub name: Symbol,
}

impl Error for MacroOverwritten {}
impl Display for MacroOverwritten {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Macro {} was overwritten", self.name))
    }
}

impl LintDiagnostic for MacroOverwritten {
    #[inline(always)]
    fn lint(&self) -> Lint {
        builtin::macro_overwritten
    }

    fn slices(&self, main_type: AnnotationType) -> Vec<DiagnosticSlice> {
        let old = DiagnosticSlice {
            slice_span: self.old.data(),
            messages: vec![(
                AnnotationType::Info,
                Text::const_str("First declared here"),
                self.old.data(),
            )],
            fold: false,
        };

        let new = DiagnosticSlice {
            slice_span: self.new.data(),
            messages: vec![(
                main_type,
                Text::const_str("Later overwritten here"),
                self.new.data(),
            )],
            fold: false,
        };

        vec![old, new]
    }
}

#[derive(Debug, Clone)]
pub struct MacroCutOffAtFileEnd {
    pub span: Span,
    pub name: Symbol,
}

impl Error for MacroCutOffAtFileEnd {}
impl Display for MacroCutOffAtFileEnd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Macro {} is assumed to end at file end",
            self.name
        ))
    }
}

impl LintDiagnostic for MacroCutOffAtFileEnd {
    #[inline(always)]
    fn lint(&self) -> Lint {
        builtin::macro_file_cutoff
    }

    fn slices(&self, main_type: AnnotationType) -> Vec<DiagnosticSlice> {
        let slice = DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_type,
                Text::const_str("Macro is assumed to end here"),
                self.span.data(),
            )],
            fold: false,
        };

        vec![slice]
    }

    #[inline(always)]
    fn footer(&self) -> Vec<FooterItem> {
        vec![FooterItem {
            id: None,
            label: Text::const_str("frontend does not allow macro definitions to be split across file borders.\n\
             This is technically not standard compliant but the intended use case most of the time anyway.\n\
             If this is what you meant consider adding a new line so that other compilers also parse your model correctly 
             "),
            annotation_type: AnnotationType::Note,
        }]
    }
}
