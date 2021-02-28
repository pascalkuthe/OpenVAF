/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use core::fmt::Formatter;
use openvaf_diagnostics::{
    AnnotationType, DiagnosticSlice, FooterItem, LibraryDiagnostic, ListFormatter, Text,
    Unsupported,
};
use openvaf_middle::Type;
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::{Ident, Symbol};
use std::fmt::Display;
use thiserror::Error;


#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ReferenceKind {
    Variable,
    Parameter,
}

impl Display for ReferenceKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable => f.write_str("variable"),
            Self::Parameter => f.write_str("parameter"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TyPrinter(pub Type);

impl From<Type> for TyPrinter {
    fn from(x: Type) -> Self {
        Self(x)
    }
}

impl Display for TyPrinter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.with_info(|x| Display::fmt(x, f))
    }
}

#[derive(Error, Clone, Debug)]
pub enum Error {
    #[error("Expected '{expected_type}' valued expression buf found '{found}'")]
    TypeMissmatch {
        span: Span,
        expected_type: TyPrinter,
        found: TyPrinter,
    },

    #[error(
        "The OpenVAF builtin attribute '{attr}' expects a list of lint names as string literals"
    )]
    ExpectedLintName { attr: Symbol, span: Span },

    #[error("Expected numeric expression buf found '{found}'")]
    ExpectedNumeric { span: Span, found: TyPrinter },

    #[error("Expected {expected_type} {ref_kind} but found {found_type} {ref_kind} {name}")]
    ReferenceTypeMissmatch {
        expected_type: TyPrinter,
        found_type: TyPrinter,
        name: Ident,
        ref_span: Span,
        ref_kind: ReferenceKind,
        decl_span: Span,
    },

    #[error("Expected '{expected_type}' value but found '{found_type}'")]
    DstTypeMissmatch {
        expected_type: TyPrinter,
        found_type: TyPrinter,
        name: Ident,
        decl_span: Span,
        span: Span,
    },

    #[error("Types {types} are not compatible")]
    MultiTypeMissmatch {
        span: Span,
        types: ListFormatter<Vec<TyPrinter>>,
        type_spans: Vec<Span>,
    },

    #[error("'output'/'inout' arguments can only accept variable reference")]
    ExpectedVariableForFunctionOutput(Span),

    #[error("Function argument count mismatch: Expected {expected} found {found}")]
    WrongFunctionArgCount {
        expected: usize,
        found: usize,
        span: Span,
    },

    #[error("{0}")]
    DerivativeError(#[from] openvaf_derivatives::error::Error),

    #[error("frontend does currently not support {0}!")]
    Unsupported(Unsupported, Span),

    #[error("{0} is not a valid finish number (allowed values are 0,1,2)")]
    IllegalFinishNumber(i64, Span),

    #[error("Function {function_name} was called recursively")]
    Recursion {
        function_name: Symbol,
        recursion_span: Span,
        recursion_traceback: Vec<(Symbol, Span)>,
    },
}

struct FunctionContext<'lt>(Option<&'lt (Symbol, Span)>);

impl<'lt> Display for FunctionContext<'lt> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some((name, _)) = self.0 {
            f.write_fmt(format_args!("(in {})", name))
        } else {
            Ok(())
        }
    }
}

impl LibraryDiagnostic for Error {
    #[inline(always)]
    fn annotation_type(&self) -> Option<AnnotationType> {
        Some(AnnotationType::Error)
    }

    fn slices(&self) -> Vec<DiagnosticSlice> {
        match self {
            Self::IllegalFinishNumber(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected 0, 1 or 2"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::TypeMissmatch {
                span,
                expected_type,
                ..
            } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected {}", expected_type)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::ReferenceTypeMissmatch {
                expected_type,
                name,
                ref_span,
                decl_span,
                ..
            } => vec![
                DiagnosticSlice {
                    slice_span: ref_span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(format!("Expected {}", expected_type)),
                        ref_span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: decl_span.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("{} was declared", name)),
                        name.span.data(),
                    )],
                    fold: false,
                },
            ],

            Self::DstTypeMissmatch {
                expected_type,
                name,
                decl_span,
                span: assign_span,
                ..
            } => vec![
                DiagnosticSlice {
                    slice_span: assign_span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(format!("Expected {}", expected_type)),
                        assign_span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: decl_span.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("{} was declared here", name)),
                        name.span.data(),
                    )],
                    fold: false,
                },
            ],

            Self::ExpectedVariableForFunctionOutput(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected variable"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::WrongFunctionArgCount { expected, span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected {} arguments", expected)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::Unsupported(unsupported, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("frontend does currently not allow {}", unsupported)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::Recursion {
                function_name,
                recursion_span,
                recursion_traceback,
            } => {
                let mut res = Vec::with_capacity(recursion_traceback.len() + 1);
                res[0] = DiagnosticSlice {
                    slice_span: recursion_span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(format!(
                            "{} was called recursively here {}",
                            function_name,
                            FunctionContext(recursion_traceback.last())
                        )),
                        recursion_span.data(),
                    )],
                    fold: false,
                };

                let mut iter = recursion_traceback.iter().rev().peekable();

                while let Some((function_name, span)) = iter.next() {
                    let caller = iter.peek().copied();
                    res.push(DiagnosticSlice {
                        slice_span: span.data(),
                        messages: vec![(
                            AnnotationType::Error,
                            Text::owned(format!(
                                "{} was called here {}",
                                function_name,
                                FunctionContext(caller)
                            )),
                            span.data(),
                        )],
                        fold: false,
                    });
                }
                res
            }
            Self::DerivativeError(err) => err.slices(),
            Self::MultiTypeMissmatch {
                span,
                types,
                type_spans,
            } => {
                let mut messages = Vec::with_capacity(types.1.len() + 1);
                messages[0] = (
                    AnnotationType::Error,
                    Text::const_str("Types mismatched"),
                    span.data(),
                );

                for (ty, span) in types.0.iter().zip(type_spans) {
                    messages.push((
                        AnnotationType::Info,
                        Text::owned(format!("This has type '{}'", ty)),
                        span.data(),
                    ))
                }

                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages,
                    fold: true,
                }]
            }
            Self::ExpectedNumeric { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected numeric value "),
                    span.data(),
                )],
                fold: false,
            }],
            Self::ExpectedLintName { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected a string literal"),
                    span.data(),
                )],
                fold: false,
            }],
        }
    }

    #[inline]
    fn footer(&self) -> Vec<FooterItem> {
        if let Self::ExpectedLintName { attr, .. } = self {
            vec![FooterItem {
                id: None,
                label: Text::owned(format!(
                    "Valid examples are {0}=\"lint_a\", {0}='{{\"lint_a\",\"lint_b\",\"lint_c\"}}",
                    attr
                )),
                annotation_type: AnnotationType::Help,
            }]
        } else {
            Vec::new()
        }
    }
}
