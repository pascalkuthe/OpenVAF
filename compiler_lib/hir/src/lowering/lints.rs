/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use derive_more::Display;
use openvaf_diagnostics::lints::{builtin, Lint, LintDiagnostic, LintLevel};
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, FooterItem, Text};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::{Ident, Symbol};
use std::error::Error;

#[derive(Clone, Debug, Display)]
#[display(fmt = "no value was provided to builtin attribute {}", "0")]
pub struct EmptyBuiltinAttribute(pub Ident);

impl Error for EmptyBuiltinAttribute {}

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

#[derive(Clone, Debug, Display)]
#[display(fmt = "no lint with name '{}' was found", "lint")]
pub struct UnknownLint {
    pub span: Span,
    pub lint: String,
}

impl Error for UnknownLint {}

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

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "the lint-level of '{}' was overwritten! It had previously been set to '{:?}'",
    "lint",
    "old"
)]
pub struct LintLevelOverwrite {
    pub span: Span,
    pub lint: String,
    pub old: LintLevel,
    pub new: LintLevel,
}

impl Error for LintLevelOverwrite {}
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

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "'{}' does not have any side effects. Therefore calling it without using the return value has no effect!",
    "name"
)]
pub struct UselessFunctionCall {
    pub span: Span,
    pub name: Symbol,
    pub decl: Option<Span>,
}
impl Error for UselessFunctionCall {}

impl LintDiagnostic for UselessFunctionCall {
    fn lint(&self) -> Lint {
        builtin::lint_level_owerwrite
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        let mut messages = vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("function call has no effect"),
                self.span.data(),
            )],
            fold: false,
        }];

        if let Some(decl) = self.decl {
            messages.push(DiagnosticSlice {
                slice_span: decl.data(),
                messages: vec![(
                    main_annotation_type,
                    Text::owned(format!("'{}' is declared here", self.name)),
                    decl.data(),
                )],
                fold: false,
            });
        }

        messages
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "the return value '{}' of is no used", "name")]
pub struct UnusedReturnValue {
    pub span: Span,
    pub name: Symbol,
    pub decl: Option<Span>,
}
impl Error for UnusedReturnValue {}

impl LintDiagnostic for UnusedReturnValue {
    fn lint(&self) -> Lint {
        builtin::lint_level_owerwrite
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        let mut messages = vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::const_str("unused return value"),
                self.span.data(),
            )],
            fold: false,
        }];

        if let Some(decl) = self.decl {
            messages.push(DiagnosticSlice {
                slice_span: decl.data(),
                messages: vec![(
                    main_annotation_type,
                    Text::owned(format!("'{}' is declared here", self.name)),
                    decl.data(),
                )],
                fold: false,
            });
        }

        messages
    }
}

#[derive(Clone, Debug, Copy, Eq, PartialEq, Display)]
pub enum NonStandardCodeType {
    #[display(fmt = "Partial derivatives by a potential difference")]
    DerivativeByTemperatureDerivatives,
    #[display(fmt = "Partial derivatives by temperature")]
    DerivativeByVoltageDifference,
}

impl NonStandardCodeType {
    const fn hint(&self) -> NonStandardCodeHint {
        NonStandardCodeHint::PartialDerivative
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
enum NonStandardCodeHint {
    #[display(
        fmt = "The VerilogAMS standard only allows derivatives by node potential `ddx(foo, pot(node))` and by branch flow `ddx(foo, flow(branch))`"
    )]
    PartialDerivative,
}

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "The following code is an extension to VerilogA allowed by this compiler but not compliant with the VerilogAMS standard"
)]
pub struct NonStandardCode {
    pub span: Span,
    pub kind: NonStandardCodeType,
}

impl Error for NonStandardCode {}

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
