/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use openvaf_diagnostics::lints::{builtin, Lint, LintDiagnostic, LintLevel};
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, Text};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::Ident;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct EmptyBuiltinAttribute(pub Ident);

impl Error for EmptyBuiltinAttribute {}

impl Display for EmptyBuiltinAttribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "No value was provided to builtin attribute {}",
            self.0
        ))
    }
}

impl LintDiagnostic for EmptyBuiltinAttribute {
    fn lint(&self) -> Lint {
        builtin::empty_builtin_attribute
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.0.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("This attribute is being ignored"),
                self.0.span.data(),
            )],
            fold: false,
        }]
    }
}

#[derive(Clone, Debug)]
pub struct UnknownLint {
    pub span: Span,
    pub lint: String,
}

impl Error for UnknownLint {}

impl Display for UnknownLint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("No lint with name '{}' was found", self.lint))
    }
}

impl LintDiagnostic for UnknownLint {
    fn lint(&self) -> Lint {
        builtin::unkown_lint
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("Unkown lint"),
                self.span.data(),
            )],
            fold: false,
        }]
    }
}

#[derive(Clone, Debug)]
pub struct LintLevelOverwrite {
    pub span: Span,
    pub lint: String,
    pub old: LintLevel,
    pub new: LintLevel,
}

impl Error for LintLevelOverwrite {}

impl Display for LintLevelOverwrite {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "The lint-level of '{}' was overwritten! It had previously been set to '{:?}'",
            self.lint, self.old
        ))
    }
}

impl LintDiagnostic for LintLevelOverwrite {
    fn lint(&self) -> Lint {
        builtin::lint_level_owerwrite
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::owned(format!("Level of {} is overwritten here", self.lint)),
                self.span.data(),
            )],
            fold: false,
        }]
    }
}
