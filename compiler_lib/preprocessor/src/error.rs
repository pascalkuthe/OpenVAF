/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::lexer::Token;
use crate::tokenstream::MacroArg;
use openvaf_diagnostics::ListFormatter;
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, LibraryDiagnostic, Text};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::Symbol;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Macro argument count mismatch expected {expected} but found {found}!")]
    MacroArgumentCountMissmatch {
        expected: MacroArg,
        found: usize,
        span: Span,
    },

    #[error("Macro {0} has not been declared")]
    MacroNotFound(Symbol, Span),

    #[error("Macro {0} was called recursively")]
    MacroRecursion(Symbol, Span),

    #[error("IoError occured during file include: {0}")]
    IoError(std::io::Error, Span),

    //General
    #[error("Unexpected EOF! Expected {expected} ")]
    UnexpectedEof {
        expected: ListFormatter<Vec<Token>>,
        span: Span,
    },

    #[error("Unexpected token! Expected {expected}")]
    MissingOrUnexpectedToken {
        expected: Token,
        expected_at: Span,
        span: Span,
    },

    #[error("Unexpected token! Expected {0}")]
    MissingToken(Token, Span),

    #[error("Unexpected EOF! Expected {0}")]
    MissingTokenAtEnd(Token, Span),

    #[error("SyntaxContext (Macro or File) ended while a compiler directive was still unfinished")]
    EndTooEarly(Span),

    #[error("Encountered unexpected token!")]
    UnexpectedToken(Span),
}

impl LibraryDiagnostic for Error {
    #[inline(always)]
    fn annotation_type(&self) -> Option<AnnotationType> {
        Some(AnnotationType::Error)
    }

    fn slices(&self) -> Vec<DiagnosticSlice> {
        match self {
            Self::UnexpectedToken(span) | Self::MissingToken(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Unexpected Token"),
                    span.data(),
                )],
                fold: false,
            }],

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
                            AnnotationType::Info,
                            Text::owned(format!("Expected {}", expected)),
                            expected_at,
                        ),
                    ],
                    fold: false,
                }]
            }

            Self::MacroArgumentCountMissmatch { expected, span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected {} arguments", expected)),
                    span.data(),
                )],
                fold: false,
            }],
            Self::MacroNotFound(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Not found in the current context"),
                    span.data(),
                )],
                fold: false,
            }],
            Self::MacroRecursion(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Recursion occurred here"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::IoError(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("IoError occurred here"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::UnexpectedEof { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Unexpected EOF"),
                    span.data(),
                )],
                fold: true,
            }],
            Self::MissingTokenAtEnd(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Unexpected EOF"),
                    span.data(),
                )],
                fold: false,
            }],
            Self::EndTooEarly(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Unexpected end of SyntaxContext"),
                    span.data(),
                )],
                fold: false,
            }],
        }
    }
}

pub type Result<T = ()> = std::result::Result<T, Error>;