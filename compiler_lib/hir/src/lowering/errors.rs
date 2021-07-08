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
use openvaf_diagnostics::{
    AnnotationType, DiagnosticSlice, FooterItem, LibraryDiagnostic, ListFormatter, Text,
    Unsupported, HINT_UNSUPPORTED,
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

#[derive(Copy, Clone, Debug)]
pub struct ArgumentCountMismatch {
    pub at_most: Option<u8>,
    pub at_least: u8,
    pub found: u8,
}

impl Display for ArgumentCountMismatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.at_most == Some(self.at_least) {
            write!(f, "expects {} arguments", self.at_least)?
        } else if self.found < self.at_least {
            write!(f, "requires at least {} arguments", self.at_least)?
        } else {
            write!(f, "accepts at most {} arguments", self.at_most.unwrap())?
        }
        write!(f, "but {} arguments were provided", self.found)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ShortArgCountMissMatch(ArgumentCountMismatch);

impl Display for ShortArgCountMissMatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.at_most == Some(self.0.at_least) {
            write!(f, "expected {} arguments", self.0.at_least)?
        } else if self.0.found < self.0.at_least {
            write!(f, "expected at least {} arguments", self.0.at_least)?
        } else {
            write!(f, "expected at most {} arguments", self.0.at_most.unwrap())?
        }
        write!(f, "but {} arguments were provided", self.0.found)
    }
}

#[derive(Error, Clone, Debug)]
pub enum Error {
    #[error("expected '{expected_type}' valued expression buf found '{found}'")]
    TypeMissmatch {
        span: Span,
        expected_type: TyPrinter,
        found: TyPrinter,
    },

    #[error("expected an expression (a value) but '{function}' does not return anything")]
    ExpectedValue { span: Span, function: Symbol },

    #[error("user defined $limit mut return a real number but '{function}' returns '{ty}'")]
    ExpectedRealFunction {
        span: Span,
        function: Ident,
        ty: Type,
    },

    #[error("user defined $limit function can only have 'input' arguments but '{function}' type but '{arg}' was declared as 'inout'/'output'")]
    UserLimFunctionWithOutputArgs {
        arg: Ident,
        function: Ident,
        arg_decl: Span,
        call: Span,
    },

    #[error(
        "the OpenVAF builtin attribute '{attr}' expects a list of lint names as string literals"
    )]
    ExpectedLintName { attr: Symbol, span: Span },

    #[error("expected numeric expression buf found '{found}'")]
    ExpectedNumeric { span: Span, found: TyPrinter },

    #[error("expected a string or an array buf found '{found}'")]
    ExpectedStringOrArray { span: Span, found: TyPrinter },

    #[error("expected {expected_type} {ref_kind} but found {found_type} {ref_kind} {name}")]
    ReferenceTypeMissmatch {
        expected_type: TyPrinter,
        found_type: TyPrinter,
        name: Ident,
        ref_span: Span,
        ref_kind: ReferenceKind,
        decl_span: Span,
    },

    #[error("expected '{expected_type}' value but found '{found_type}'")]
    DstTypeMissmatch {
        expected_type: TyPrinter,
        found_type: TyPrinter,
        name: Ident,
        decl_span: Span,
        span: Span,
    },

    #[error("types {types} are not compatible")]
    MultiTypeMissmatch {
        span: Span,
        types: ListFormatter<Vec<TyPrinter>>,
        type_spans: Vec<Span>,
    },

    #[error("branch {branch} has both potential (voltage) and flow (current) contributions")]
    SimultaneousVoltageAndCurrentContribute {
        branch: Ident,
        voltage_contribute: Vec<Span>,
        current_contribute: Vec<Span>,
    },

    #[error("both the current and voltage of branch {branch} are read without any contributions")]
    SimultaneousVoltageAndCurrentProbe {
        branch: Ident,
        voltage_probes: Vec<Span>,
        current_probes: Vec<Span>,
    },

    #[error("argument {decl} of the function {fun} is marked as 'output'/'inout' but a value was provided instead of variable reference.")]
    ExpectedVariableForFunctionOutput { span: Span, decl: Ident, fun: Ident },

    #[error("function argument count mismatch: Expected {expected} found {found}")]
    WrongFunctionArgCount {
        expected: usize,
        found: usize,
        span: Span,
    },

    #[error("partial derivatives may only be calculated over potentials between two nodes, branch flows or temperatures")]
    DerivativeNotAllowed(Span),

    #[error("the function unction {name} {missmatch}!")]
    ArgCountMissMatch {
        missmatch: ArgumentCountMismatch,
        span: Span,
        name: Symbol,
        decl: Option<Span>,
    },

    #[error("{name} only accepts a {refkind} for its argument")]
    FunctionExpectedReference {
        span: Span,
        name: Symbol,
        refkind: &'static str,
    },

    #[error("{0}")]
    DerivativeError(#[from] openvaf_middle::derivatives::Error),

    #[error("frontend does currently not support {0}!")]
    Unsupported(Unsupported, Span),

    #[error("{0} is not a valid finish number (allowed values are 0,1,2)")]
    IllegalFinishNumber(i64, Span),

    #[error("function {function_name} was called recursively")]
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
                    Text::const_str("expected 0, 1 or 2"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::DerivativeNotAllowed(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("expected node potential or branch flow"),
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
                    Text::owned(format!("expected {}", expected_type)),
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
                        Text::owned(format!("expected {}", expected_type)),
                        ref_span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: decl_span.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("{} is declared here", name)),
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
                        Text::owned(format!("expected {}", expected_type)),
                        assign_span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: decl_span.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("{} is declared here", name)),
                        name.span.data(),
                    )],
                    fold: false,
                },
            ],

            Self::ExpectedVariableForFunctionOutput { span, decl, fun } => vec![
                DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("expected a variable"),
                        span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: fun.span.data().extend(decl.span.data()),
                    messages: vec![
                        (
                            AnnotationType::Info,
                            Text::owned(format!("{} was declared here...", fun)),
                            fun.span.data(),
                        ),
                        (
                            AnnotationType::Help,
                            Text::owned(format!(
                                "... with argument {} marked as 'inout'/'output",
                                decl
                            )),
                            decl.span.data(),
                        ),
                    ],
                    fold: true,
                },
            ],

            Self::WrongFunctionArgCount { expected, span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("expected {} arguments", expected)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::Unsupported(unsupported, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!(
                        "this compiler does currently not support {}",
                        unsupported
                    )),
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
                let mut messages = Vec::with_capacity(types.list.len() + 1);
                messages[0] = (
                    AnnotationType::Error,
                    Text::const_str("types mismatched"),
                    span.data(),
                );

                for (ty, span) in types.list.iter().zip(type_spans) {
                    messages.push((
                        AnnotationType::Info,
                        Text::owned(format!("this has type '{}'", ty)),
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
                    Text::const_str("expected numeric value"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::ExpectedStringOrArray { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("expected a string or an array"),
                    span.data(),
                )],
                fold: false,
            }],
            Self::ExpectedLintName { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("expected a string literal"),
                    span.data(),
                )],
                fold: false,
            }],
            Self::ArgCountMissMatch {
                missmatch,
                span,
                name,
                decl,
            } => {
                let mut res = vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(ShortArgCountMissMatch(*missmatch).to_string()),
                        span.data(),
                    )],
                    fold: false,
                }];

                if let Some(decl) = decl {
                    res.push(DiagnosticSlice {
                        slice_span: decl.data(),
                        messages: vec![(
                            AnnotationType::Help,
                            Text::owned(format!("'{}' is declared here", name)),
                            decl.data(),
                        )],
                        fold: false,
                    })
                }

                res
            }
            Self::FunctionExpectedReference { span, refkind, .. } => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(format!("expected a {} reference", refkind)),
                        span.data(),
                    )],
                    fold: false,
                }]
            }

            Self::ExpectedValue { span, .. } => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("expected a value"),
                        span.data(),
                    )],
                    fold: false,
                }]
            }
            Self::SimultaneousVoltageAndCurrentContribute {
                branch,
                voltage_contribute,
                current_contribute,
            } => {
                let mut diagnostics = vec![DiagnosticSlice {
                    slice_span: branch.span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("branch received contributions to flow and potential"),
                        branch.span.data(),
                    )],
                    fold: false,
                }];

                diagnostics.extend(voltage_contribute.iter().map(|x| DiagnosticSlice {
                    slice_span: x.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::const_str("contribution to potential here"),
                        x.data(),
                    )],
                    fold: false,
                }));

                diagnostics.extend(current_contribute.iter().map(|x| DiagnosticSlice {
                    slice_span: x.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::const_str("contribution to flow here"),
                        x.data(),
                    )],
                    fold: false,
                }));

                diagnostics
            }
            Self::SimultaneousVoltageAndCurrentProbe {
                branch,
                voltage_probes,
                current_probes,
            } => {
                let mut diagnostics = vec![DiagnosticSlice {
                    slice_span: branch.span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("branch can not be both flow (current) and potential (voltage) probe at the same time"),
                        branch.span.data(),
                    )],
                    fold: false,
                }];

                diagnostics.extend(voltage_probes.iter().map(|x| DiagnosticSlice {
                    slice_span: x.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::const_str("potential is probed here"),
                        x.data(),
                    )],
                    fold: false,
                }));

                diagnostics.extend(current_probes.iter().map(|x| DiagnosticSlice {
                    slice_span: x.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::const_str("flow probed ere"),
                        x.data(),
                    )],
                    fold: false,
                }));

                diagnostics
            }
            Self::ExpectedRealFunction { span, .. } => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("expected a function with real return type"),
                        span.data(),
                    )],
                    fold: false,
                }]
            }
            Self::UserLimFunctionWithOutputArgs {
                arg,
                function,
                arg_decl,
                call,
            } => {
                vec![
                    DiagnosticSlice {
                        slice_span: call.data(),
                        messages: vec![(
                            AnnotationType::Error,
                            Text::const_str("user $limit functions must only have input arguments"),
                            call.data(),
                        )],
                        fold: false,
                    },
                    DiagnosticSlice {
                        slice_span: function.span.data().extend(arg_decl.data()),
                        messages: vec![
                            (
                                AnnotationType::Info,
                                Text::owned(format!("{} was declared here", function)),
                                call.data(),
                            ),
                            (
                                AnnotationType::Error,
                                Text::owned(format!("{} must be an input argument", arg)),
                                call.data(),
                            ),
                        ],
                        fold: true,
                    },
                ]
            }
        }
    }

    #[inline]
    fn footer(&self) -> Vec<FooterItem> {
        match self{
            Self::ExpectedLintName { attr, .. } => vec![FooterItem {
                id: None,
                label: Text::owned(format!(
                    "valid examples are {0}=\"lint_a\" and {0}='{{\"lint_a\",\"lint_b\",\"lint_c\"}}",
                    attr
                )),
                annotation_type: AnnotationType::Help,
            }],
            Self::SimultaneousVoltageAndCurrentContribute { ..} => vec![FooterItem {
                id: None,
                label: Text::const_str(HINT_UNSUPPORTED),
                annotation_type: AnnotationType::Help,
            }],
            Self::SimultaneousVoltageAndCurrentProbe { ..} => vec![FooterItem {
                id: None,
                label: Text::const_str("a simulation where both the potential and the voltage are flow can not converge\nadding a contribute statement fixes either of these variables"),
                annotation_type: AnnotationType::Help,
            }],
            _ => vec![]


        }
    }
}
