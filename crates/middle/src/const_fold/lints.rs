/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use diagnostics::lints::{builtin, Lint, LintDiagnostic};
use diagnostics::{AnnotationType, DiagnosticSlice, Text};
use session::sourcemap::Span;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct ConstantOverflow(pub Span);

impl Error for ConstantOverflow {}

impl Display for ConstantOverflow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Value overflowed during constant fold")
    }
}

impl LintDiagnostic for ConstantOverflow {
    fn lint(&self) -> Lint {
        builtin::constant_overflow
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.0.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("Value overflowed during constant fold"),
                self.0.data(),
            )],
            fold: false,
        }]
    }
}
