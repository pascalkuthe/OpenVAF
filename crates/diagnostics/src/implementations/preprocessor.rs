/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use annotate_snippets::snippet::AnnotationType;
use basedb::{BaseDB, FileId};
use data_structures::text_size::TextRange;
use syntax::{MacroOverwritten, PreprocessorDiagnostic};

use crate::{
    lints::{builtin, Lint, LintDiagnostic, SyntaxCtx},
    Diagnostic, DiagnosticSlice, Text,
};

impl Diagnostic for PreprocessorDiagnostic {
    #[inline(always)]
    fn annotation_type(&self, db: &dyn BaseDB, root_file: FileId) -> Option<AnnotationType> {
        match self {
            Self::MacroOverwritten(lint) => lint.annotation_type(db, root_file),
            _ => Some(AnnotationType::Error),
        }
    }

    fn slices(&self, db: &dyn BaseDB, root_file: FileId) -> Vec<DiagnosticSlice> {
        match *self {
            PreprocessorDiagnostic::UnexpectedToken(span) => vec![DiagnosticSlice {
                slice_span: span,
                messages: vec![(AnnotationType::Error, Text::const_str("unexpected token"), span)],
                fold: false,
            }],

            PreprocessorDiagnostic::MacroNotFound(_, span) => vec![DiagnosticSlice {
                slice_span: span,
                messages: vec![(AnnotationType::Error, Text::const_str("macro not found"), span)],
                fold: false,
            }],
            PreprocessorDiagnostic::MacroRecursion(_, span) => vec![DiagnosticSlice {
                slice_span: span,
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("recursion occurred here"),
                    span,
                )],
                fold: false,
            }],

            PreprocessorDiagnostic::IoError(span)
            | PreprocessorDiagnostic::InvalidTextFormat(span) => {

                vec![DiagnosticSlice {
                    slice_span: span,
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("failed to read file"),
                        span,
                    )],
                    fold: false,
                }]
            }

            PreprocessorDiagnostic::IoErrorRoot(_)
            | PreprocessorDiagnostic::InvalidTextFormatRoot(_) => {
                vec![]
            }

            PreprocessorDiagnostic::MacroOverwritten(ref lint) => {
                Diagnostic::slices(lint, db, root_file)
            }

            PreprocessorDiagnostic::MissingOrUnexpectedToken { expected, expected_at, span } => {
                let mut expected_at = expected_at;
                expected_at.range = TextRange::at(expected_at.range.start(), 0.into());
                vec![DiagnosticSlice {
                    slice_span: expected_at.extend(span, &db.sourcemap(root_file)),
                    messages: vec![
                        (AnnotationType::Error, Text::const_str("unexpected token"), span),
                        (
                            AnnotationType::Info,
                            Text::owned(format!("expected {}", expected)),
                            expected_at,
                        ),
                    ],
                    fold: false,
                }]
            }

            PreprocessorDiagnostic::MacroArgumentCountMissmatch { expected, span, .. } => {
                vec![DiagnosticSlice {
                    slice_span: span,
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(format!("expected {} arguments", expected)),
                        span,
                    )],
                    fold: false,
                }]
            }
            PreprocessorDiagnostic::UnexpectedEof { expected: _, span } => vec![DiagnosticSlice {
                slice_span: span,
                messages: vec![(AnnotationType::Error, Text::const_str("unexpected EOF"), span)],
                fold: false,
            }],
        }
    }
}

impl LintDiagnostic for MacroOverwritten {
    #[inline(always)]
    fn lint(&self) -> Lint {
        builtin::macro_overwritten
    }

    fn sctx(&self) -> Option<SyntaxCtx> {
        None
    }

    fn slices(&self, main_type: AnnotationType) -> Vec<DiagnosticSlice> {
        let old = DiagnosticSlice {
            slice_span: self.old,
            messages: vec![(
                AnnotationType::Info,
                Text::const_str("First declared here"),
                self.old,
            )],
            fold: false,
        };

        let new = DiagnosticSlice {
            slice_span: self.new,
            messages: vec![(main_type, Text::const_str("Later overwritten here"), self.new)],
            fold: false,
        };

        vec![old, new]
    }
}
