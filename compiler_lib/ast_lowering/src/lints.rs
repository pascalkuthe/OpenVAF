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
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, FooterItem, Text};
use openvaf_session::sourcemap::Span;
use std::error::Error;
use std::fmt;
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

#[derive(Clone, Debug, Copy, Eq, PartialEq)]
pub enum NonStandardCodeType {
    DerivativeByTemperatureDerivatives,
    DerivativeByVoltageDifference,
}

impl Display for NonStandardCodeType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::DerivativeByVoltageDifference => {
                write!(f, "Partial derivatives by a potential difference")
            }
            Self::DerivativeByTemperatureDerivatives => {
                write!(f, "Partial derivatives by temperature")
            }
        }
    }
}

impl NonStandardCodeType {
    const fn hint(&self) -> NonStandardCodeHint {
        NonStandardCodeHint::PartialDerivative
    }
}

enum NonStandardCodeHint {
    PartialDerivative,
}

impl Display for NonStandardCodeHint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self{
            NonStandardCodeHint::PartialDerivative => write!(f,"The VerilogAMS standard only allows derivatives by node potential `ddx(foo, pot(node))` and by branch flow `ddx(foo, flow(branch))`")
        }
    }
}

#[derive(Clone, Debug)]
pub struct NonStandardCode {
    pub span: Span,
    pub kind: NonStandardCodeType,
}

impl Error for NonStandardCode {}

impl Display for NonStandardCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("The following code is an extension to VerilogA allowed by this compiler but not compliant with the VerilogAMS standard")
    }
}

impl LintDiagnostic for NonStandardCode {
    fn lint(&self) -> Lint {
        builtin::non_standard_code
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::owned(format!(
                    "{} are not allowed by the VerilogAMS standard",
                    self.kind
                )),
                self.span.data(),
            )],
            fold: false,
        }]
    }

    fn footer(&self) -> Vec<FooterItem> {
        vec![
            FooterItem{
                id: None,
                label: Text::owned(format!("{}", self.kind.hint())),
                annotation_type: AnnotationType::Help
            },
            FooterItem{
                id: None,
                label: Text::const_str("This code will work just fine but other VerilogA compilers will most likely produce an error upon encountering it."),
                annotation_type: AnnotationType::Info
            }
        ]
    }
}
