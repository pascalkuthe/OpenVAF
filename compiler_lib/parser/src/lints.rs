/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use core::fmt::Formatter;
use openvaf_diagnostics::lints::{builtin, Lint, LintDiagnostic};
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, Text};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::Symbol;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct AtrributeOverwritten {
    pub old: Span,
    pub new: Span,
    pub name: Symbol,
}

impl Error for AtrributeOverwritten {}
impl Display for AtrributeOverwritten {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Attribute {} was overwritten", self.name))
    }
}

impl LintDiagnostic for AtrributeOverwritten {
    #[inline(always)]
    fn lint(&self) -> Lint {
        builtin::attribute_overwritten
    }

    fn slices(&self, main_type: AnnotationType) -> Vec<DiagnosticSlice> {
        let slice = DiagnosticSlice {
            slice_span: self.old.data().extend(self.new.data()),
            messages: vec![
                (
                    AnnotationType::Info,
                    Text::const_str("First declared here"),
                    self.old.data(),
                ),
                (
                    main_type,
                    Text::const_str("Overwritten here"),
                    self.new.data(),
                ),
            ],
            fold: false,
        };

        vec![slice]
    }
}
