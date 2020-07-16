//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::diagnostic::{DiagnosticSlice, FooterItem, Text};
use crate::lints::{builtin, Lint, LintDiagnostic};
use crate::sourcemap::Span;
use annotate_snippets::snippet::AnnotationType;
use core::fmt::Formatter;
use std::error::Error;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct RoundingDerivativeNotFullyDefined {
    pub span: Span,
}

impl Error for RoundingDerivativeNotFullyDefined {}

impl Display for RoundingDerivativeNotFullyDefined {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Derivative of rounding required")
    }
}

impl LintDiagnostic for RoundingDerivativeNotFullyDefined {
    fn lint(&self) -> Lint {
        builtin::rounding_derivative
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("Required while deriving this"),
                self.span.data(),
            )],
            fold: false,
        }]
    }

    #[inline]
    fn footer(&self) -> Vec<FooterItem> {
        vec![FooterItem{
            id: None,
            label: Text::const_str("Derivative is assumed to be always 0. However technically it is undefined for n/2 (n is a whole number)"),
            annotation_type: AnnotationType::Note
        }]
    }
}

#[derive(Clone, Debug)]
pub struct NoiseDerivative(Span);

impl Error for NoiseDerivative {}

impl Display for NoiseDerivative {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Derivative of rounding required")
    }
}

impl LintDiagnostic for NoiseDerivative {
    fn lint(&self) -> Lint {
        builtin::noise_derivative
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.0.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("Required while deriving this"),
                self.0.data(),
            )],
            fold: false,
        }]
    }

    #[inline]
    fn footer(&self) -> Vec<FooterItem> {
        vec![FooterItem{
            id: None,
            label: Text::const_str("Derivative is assumed to be always 0. However technically it is undefined for n/2 (n is a whole number)"),
            annotation_type: AnnotationType::Note
        }]
    }
}
