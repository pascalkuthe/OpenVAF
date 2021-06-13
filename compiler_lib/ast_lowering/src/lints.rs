/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use openvaf_diagnostics::lints::builtin;
use openvaf_diagnostics::lints::{Lint, LintDiagnostic};
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, Text};
use openvaf_session::sourcemap::Span;
use std::error::Error;
use std::fmt::{Display, Formatter};
#[derive(Clone, Debug)]
pub struct IgnoredDisplayTask {
    pub span: Span,
}

impl Error for IgnoredDisplayTask {}

impl Display for IgnoredDisplayTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Display system function calls are currently not fully implemented in frontend. They are simply ignored")
    }
}

impl LintDiagnostic for IgnoredDisplayTask {
    fn lint(&self) -> Lint {
        builtin::ignored_display_task
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("Display system function call is ignored"),
                self.span.data(),
            )],
            fold: false,
        }]
    }
}
