/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::fmt::{Display, Formatter};

use crate::Token;
use itertools::Itertools;
use lalrpop_util::{ErrorRecovery, ParseError};
use more_asserts::{assert_le, assert_lt, debug_assert_le, debug_assert_lt};
use openvaf_diagnostics::ListFormatter;
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, FooterItem, LibraryDiagnostic, Text};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::Symbol;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("{name} was already declared in this Scope!")]
    AlreadyDeclaredInThisScope {
        declaration: Span,
        other_declaration: Span,
        name: Symbol,
    },

    #[error("Unexpected Token!")]
    MissingOrUnexpectedToken {
        expected: Token,
        expected_at: Span,
        span: Span,
    },

    #[error("Reached 'endmodule' while stil expecting an 'end' delimiter!")]
    MismatchedDecimeters { start: Span, end: Span },

    #[error("Unexpected EOF! Expected {expected}")]
    UnrecognizedEof {
        expected: ListFormatter<Vec<String>>,
        span: Span,
    },

    #[error("Unexpected Token! Expected {expected} found {found}")]
    UnrecognizedToken {
        expected: ListFormatter<Vec<String>>,
        found: Token,
        span: Span,
        ignored: Option<Span>,
    },

    #[error("Encountered additional unexpected token '{token}'!")]
    ExtraToken { span: Span, token: Token },

    #[error("Unexpected Token!")]
    UnexpectedToken { span: Span, ignored: Option<Span> },

    // for builtin calls only
    #[error("Argument count mismatch: Expected {0}")]
    ArgumentCountMissmatch(ArgumentCountMissmatch, &'static str, Span),
}

#[derive(Copy, Clone, Debug)]
pub struct ArgumentCountMissmatch {
    pub at_most: u8,
    pub at_least: u8,
    pub found: u8,
}
impl ArgumentCountMissmatch {
    fn expectation(&self) -> String {
        debug_assert_le!(self.at_least, self.at_most);

        if self.at_most == self.at_least {
            format!("{} arguments", self.at_least,)
        } else if self.found < self.at_least {
            format!("at least {} arguments", self.at_least,)
        } else {
            debug_assert_lt!(self.at_most, self.found);
            format!("at most {} arguments", self.at_most,)
        }
    }
}
impl Display for ArgumentCountMissmatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} found {}", self.expectation(), self.found))
    }
}

impl LibraryDiagnostic for Error {
    #[inline(always)]
    fn annotation_type(&self) -> Option<AnnotationType> {
        Some(AnnotationType::Error)
    }

    fn footer(&self) -> Vec<FooterItem> {
        match self {
            Self::ArgumentCountMissmatch(missmatch, call, _span) => vec![FooterItem {
                id: None,
                label: Text::owned(format!(
                    "{} accepts {} arguments",
                    call,
                    ListFormatter::new((missmatch.at_least..=missmatch.at_most).collect_vec())
                )),
                annotation_type: AnnotationType::Help,
            }],
            _ => Vec::new(),
        }
    }

    fn slices(&self) -> Vec<DiagnosticSlice> {
        match self {
            Self::MissingOrUnexpectedToken {
                expected,
                expected_at,
                span,
            } => {
                let mut expected_at = expected_at.data();
                expected_at.lo = expected_at.hi;
                vec![DiagnosticSlice {
                    slice_span: expected_at.extend(span.data()),
                    messages: vec![
                        (
                            AnnotationType::Error,
                            Text::const_str("Unexpected Token"),
                            span.data(),
                        ),
                        (
                            AnnotationType::Help,
                            Text::owned(format!("{} might be missing here", expected)),
                            expected_at,
                        ),
                    ],
                    fold: false,
                }]
            }

            Self::AlreadyDeclaredInThisScope {
                declaration,
                other_declaration,
                ..
            } => vec![
                DiagnosticSlice {
                    slice_span: declaration.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Already declared in this scope!"),
                        declaration.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: other_declaration.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::const_str("Previous declaration here"),
                        other_declaration.data(),
                    )],
                    fold: false,
                },
            ],

            Self::UnrecognizedEof { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Unexpected EOF"),
                    span.data(),
                )],
                fold: true,
            }],

            Self::UnexpectedToken { span, ignored, .. }
            | Self::UnrecognizedToken { span, ignored, .. } => {
                if let Some(ignored) = ignored {
                    vec![DiagnosticSlice {
                        slice_span: span.data().extend(ignored.data()),
                        messages: vec![
                            (
                                AnnotationType::Error,
                                Text::const_str("Unexpected Token"),
                                span.data(),
                            ),
                            (
                                AnnotationType::Info,
                                Text::const_str("Skipping to the end of this token"),
                                ignored.data(),
                            ),
                        ],
                        fold: true,
                    }]
                } else {
                    vec![DiagnosticSlice {
                        slice_span: span.data(),
                        messages: vec![(
                            AnnotationType::Error,
                            Text::const_str("Unexpected Token"),
                            span.data(),
                        )],
                        fold: false,
                    }]
                }
            }

            Self::ArgumentCountMissmatch(missmatch, _call, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected {}", missmatch.expectation())),
                    span.data(),
                )],
                fold: false,
            }],

            Self::ExtraToken { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Help,
                    Text::const_str("Remove this token"),
                    span.data(),
                )],
                fold: false,
            }],
            Self::MismatchedDecimeters { start, end  } => vec![DiagnosticSlice {
                slice_span: start.data().extend(end.data()),
                messages: vec![(
                    AnnotationType::Info,
                    Text::const_str("Analog blog starting here..."),
                    start.data(),
                ),
                               (
                                   AnnotationType::Error,
                                   Text::const_str("... has missmatched 'begin'/'end' delimiters (reached 'endmodule' too early)"),
                                   end.data(),
                               )
                ],
                fold: true,
            }],
        }
    }
}
pub struct ModuleEndErrorRecovery(pub ErrorRecovery<Span, Token, ()>, pub Span);

impl From<ModuleEndErrorRecovery> for Error {
    fn from(recovery: ModuleEndErrorRecovery) -> Self {
        match recovery.0.error {
            ParseError::InvalidToken { location } => Self::MismatchedDecimeters {
                start: recovery.1,
                end: location,
            },

            ParseError::UnrecognizedToken {
                token: (span, _, _),
                expected,
            } if expected.len() > 10 => Self::MismatchedDecimeters {
                start: recovery.1,
                end: span,
            },
            _ => recovery.0.into(),
        }
    }
}

pub struct ErrorRecoveryExpecting(pub ErrorRecovery<Span, Token, ()>, pub Span, pub Token);

impl From<ErrorRecoveryExpecting> for Error {
    fn from(recovery: ErrorRecoveryExpecting) -> Self {
        match recovery.0.error {
            ParseError::InvalidToken { location } => Self::MissingOrUnexpectedToken {
                expected: recovery.2,
                expected_at: recovery.1,
                span: location,
            },

            ParseError::UnrecognizedToken {
                token: (span, _, _),
                expected,
            } if expected.len() > 10 => Self::MissingOrUnexpectedToken {
                expected: recovery.2,
                expected_at: recovery.1,
                span,
            },
            _ => recovery.0.into(),
        }
    }
}

impl From<ErrorRecovery<Span, Token, ()>> for Error {
    fn from(recovery: ErrorRecovery<Span, Token, ()>) -> Self {
        match recovery.error {
            ParseError::InvalidToken { location } => Self::UnexpectedToken {
                span: location,
                ignored: recovery.dropped_tokens.last().map(|(span, _, _)| *span),
            },

            ParseError::UnrecognizedToken {
                token: (span, _, _),
                expected,
            } if expected.len() > 10 => Self::UnexpectedToken {
                span,
                ignored: recovery.dropped_tokens.last().map(|(span, _, _)| *span),
            },

            ParseError::UnrecognizedToken {
                token: (span, found, _),
                expected,
            } => Self::UnrecognizedToken {
                span,
                found,
                expected: human_expected(expected),
                ignored: recovery.dropped_tokens.last().map(|(span, _, _)| *span),
            },
            error => error.into(),
        }
    }
}

impl From<ParseError<Span, Token, ()>> for Error {
    fn from(error: ParseError<Span, Token, ()>) -> Self {
        match error {
            ParseError::ExtraToken {
                token: (span, token, _),
            } => Self::ExtraToken { span, token },

            ParseError::InvalidToken { location } => Self::UnexpectedToken {
                span: location,
                ignored: None,
            },

            ParseError::UnrecognizedToken {
                token: (span, _, _),
                expected,
            } if expected.len() > 10 => Self::UnexpectedToken {
                span,
                ignored: None,
            },

            ParseError::UnrecognizedToken {
                token: (span, token, _),
                expected,
            } => Self::UnrecognizedToken {
                expected: human_expected(expected),
                span,
                found: token,
                ignored: None,
            },

            ParseError::UnrecognizedEOF { location, expected } => Self::UnrecognizedEof {
                expected: human_expected(expected),
                span: location,
            },

            ParseError::User { error: _ } => unreachable!("OpenVAF doesn't user user errors"),
        }
    }
}

/// Makes the list of expected tokens more human readable by transforming tokens that can describe multiple inputs to an expression
///
/// # Example
///
/// `"Name" -> an identifier`
fn human_expected(mut expected: Vec<String>) -> ListFormatter<Vec<String>> {
    for expected in &mut expected {
        match expected.as_str() {
            "\"Name\"" | "Name" | "Identifier" => {
                *expected = "an identifer".to_string();
            }

            "\"StringLiteral\"" | "StringLiteral" => {
                *expected = "a string literal".to_string();
            }

            "\"RealLiteral\"" | "RealLiteral" => {
                *expected = "a real number".to_string();
            }

            "\"IntLiteral\"" | "IntLiteral" => {
                *expected = "an integer number".to_string();
            }

            _ => {}
        }
    }
    ListFormatter::new(expected)
}
