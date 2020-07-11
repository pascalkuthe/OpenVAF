/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use annotate_snippets::snippet::AnnotationType;

use crate::derivatives;
use crate::diagnostic::{DiagnosticSlice, LibraryDiagnostic, Text, Unsupported};
use crate::ir::mir::{Parameter, Variable, VariableType};
use crate::ir::AttributeNode;
use crate::mir::ParameterType;
use crate::symbol::{Ident, Symbol};
use crate::Span;
use core::fmt::Formatter;
use std::fmt::Display;
use thiserror::Error;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MockType {
    Real,
    Integer,
    String,
    Numeric,
}

impl Display for MockType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Real => f.write_str("real"),
            Self::Integer => f.write_str("integer"),
            Self::String => f.write_str("string"),
            Self::Numeric => f.write_str("numeric (integer or real)"),
        }
    }
}

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

#[derive(Error, Clone, Debug)]
pub enum Error {
    #[error("Expected {expected_type} valued expression")]
    TypeMissmatch { span: Span, expected_type: MockType },

    #[error("Expected {expected_type} value but found {found_type} {ref_kind} {name}")]
    ReferenceTypeMissmatch {
        expected_type: MockType,
        found_type: MockType,
        name: Ident,
        ref_span: Span,
        ref_kind: ReferenceKind,
        decl_span: Span,
    },

    #[error("Strings cannot be compared to numbers")]
    CannotCompareStringToNumber(Span),

    #[error("Conditional expressions must evaluate to a String or a Number on both sides")]
    CondtionTypeMissmatch {
        string: Span,
        number: Span,
        span: Span,
    },

    // #[error("'output'/'inout' arguments can only accept variables!")]
    // ImplicitSolverDeltaIsNotAValidString(Span),
    #[error("'output'/'inout' arguments can only accept variables")]
    ExpectedVariableForFunctionOutput(Span),

    #[error("Function argument count mismatch: Expected {expected} found {found}")]
    WrongFunctionArgCount {
        expected: u8,
        found: usize,
        span: Span,
    },

    #[error("{0}")]
    DerivativeError(#[from] derivatives::error::Error),

    #[error("frontend does currently not support {0}!")]
    Unsupported(Unsupported, Span),

    #[error("Function {function_name} was called recursively")]
    Recursion {
        function_name: Symbol,
        recursion_span: Span,
        recursion_traceback: Vec<(Symbol, Span)>,
    },
}

impl Error {
    pub fn expected_parameter_type(
        expected: MockType,
        param: &AttributeNode<Parameter>,
        at: Span,
    ) -> Self {
        let found_type = match param.contents.parameter_type {
            ParameterType::Integer { .. } => MockType::Integer,
            ParameterType::Real { .. } => MockType::Real,
            ParameterType::String { .. } => MockType::String,
        };

        Self::ReferenceTypeMissmatch {
            expected_type: expected,
            found_type,
            name: param.contents.ident,
            ref_span: at,
            ref_kind: ReferenceKind::Parameter,
            decl_span: param.span,
        }
    }

    pub fn expected_variable_type(
        expected: MockType,
        var: &AttributeNode<Variable>,
        at: Span,
    ) -> Self {
        let found_type = match var.contents.variable_type {
            VariableType::Integer { .. } => MockType::Integer,
            VariableType::Real { .. } => MockType::Real,
        };

        Self::ReferenceTypeMissmatch {
            expected_type: expected,
            found_type,
            name: var.contents.ident,
            ref_span: at,
            ref_kind: ReferenceKind::Variable,
            decl_span: var.span,
        }
    }
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
            Error::TypeMissmatch {
                span,
                expected_type,
            } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected {}", expected_type)),
                    span.data(),
                )],
                fold: false,
            }],

            Error::ReferenceTypeMissmatch {
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

            Error::CannotCompareStringToNumber(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Illegal comparison"),
                    span.data(),
                )],
                fold: false,
            }],

            Error::CondtionTypeMissmatch {
                string,
                number,
                span,
            } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![
                    (
                        AnnotationType::Error,
                        Text::const_str("This evalulates to a string"),
                        string.data(),
                    ),
                    (
                        AnnotationType::Error,
                        Text::const_str("This evalulates to a number"),
                        number.data(),
                    ),
                ],
                fold: false,
            }],

            Error::ExpectedVariableForFunctionOutput(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected variable"),
                    span.data(),
                )],
                fold: false,
            }],

            Error::WrongFunctionArgCount { expected, span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected {} arguments", expected)),
                    span.data(),
                )],
                fold: false,
            }],

            Error::Unsupported(unsupported, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("frontend does currently not allow {}", unsupported)),
                    span.data(),
                )],
                fold: false,
            }],

            Error::Recursion {
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
            Error::DerivativeError(err) => err.slices(),
        }
    }
}
