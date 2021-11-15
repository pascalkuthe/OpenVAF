use syntax::PreprocessorDiagnostic;
use vfs::FileId;

use crate::{
    diagnostics::{to_unified_spans, Diagnostic, Label, LabelStyle, Report},
    lints::{self, Lint, LintSrc},
    BaseDB,
};

impl Diagnostic for PreprocessorDiagnostic {
    fn lint(&self) -> Option<(Lint, LintSrc)> {
        if let PreprocessorDiagnostic::MacroOverwritten { .. } = self {
            Some((lints::builtin::macro_overwritten, LintSrc::GLOBAL))
        } else {
            None
        }
    }

    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let sm = db.sourcemap(root_file);
        let report = match *self {
            PreprocessorDiagnostic::MacroArgumentCountMissmatch { expected, span, .. } => {
                let span = span.to_file_span(&sm);

                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id: span.file,
                    range: span.range.into(),
                    message: format!("expected {} arguments", expected),
                }])
            }
            PreprocessorDiagnostic::MacroNotFound { span, .. } => {
                let span = span.to_file_span(&sm);

                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id: span.file,
                    range: span.range.into(),
                    message: "macro not found here".to_owned(),
                }])
            }
            PreprocessorDiagnostic::MacroRecursion { .. } => todo!(),
            PreprocessorDiagnostic::IoError { span, .. } => {
                let labels = if let Some(span) = span {
                    let span = span.to_file_span(&sm);
                    vec![Label {
                        style: LabelStyle::Primary,
                        file_id: span.file,
                        range: span.range.into(),
                        message: "failed to read file".to_owned(),
                    }]
                } else {
                    vec![]
                };
                Report::error().with_labels(labels)
            }
            PreprocessorDiagnostic::FileNotFound { span, .. } => {
                let labels = if let Some(span) = span {
                    let span = span.to_file_span(&sm);
                    vec![Label {
                        style: LabelStyle::Primary,
                        file_id: span.file,
                        range: span.range.into(),
                        message: "file not found".to_owned(),
                    }]
                } else {
                    vec![]
                };
                Report::error().with_labels(labels)
            }
            PreprocessorDiagnostic::InvalidTextFormat { span, .. } => {
                let labels = if let Some(span) = span {
                    let span = span.to_file_span(&sm);
                    vec![Label {
                        style: LabelStyle::Primary,
                        file_id: span.file,
                        range: span.range.into(),
                        message: "failed to read file".to_owned(),
                    }]
                } else {
                    vec![]
                };
                Report::error()
                    .with_labels(labels)
                    .with_notes(vec!["only UTF-8 and UTF-16 files are accepted".to_owned()])
            }
            PreprocessorDiagnostic::UnexpectedEof { expected, span } => {
                let span = span.to_file_span(&sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id: span.file,
                    range: span.range.into(),
                    message: format!("expected {}", expected),
                }])
            }
            PreprocessorDiagnostic::MissingOrUnexpectedToken { expected, expected_at, span } => {
                let (file, [expected_at, span]) = to_unified_spans(&sm, [expected_at, span]);
                Report::error().with_labels(vec![
                    Label {
                        style: LabelStyle::Primary,
                        file_id: file,
                        range: span.into(),
                        message: "unexpected token".to_owned(),
                    },
                    Label {
                        style: LabelStyle::Secondary,
                        file_id: file,
                        range: expected_at.into(),
                        message: format!("expected {}", expected),
                    },
                ])
            }
            PreprocessorDiagnostic::UnexpectedToken(span) => {
                let span = span.to_file_span(&sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id: span.file,
                    range: span.range.into(),
                    message: "unexpected token".to_owned(),
                }])
            }
            PreprocessorDiagnostic::MacroOverwritten { old, new, ref name } => {
                let new = new.to_file_span(&sm);
                let old = old.to_file_span(&sm);
                Report::warning().with_labels(vec![
                    Label {
                        style: LabelStyle::Secondary,
                        file_id: old.file,
                        range: old.range.into(),
                        message: format!("'`{}' was first defined here", name),
                    },
                    Label {
                        style: LabelStyle::Primary,
                        file_id: new.file,
                        range: new.range.into(),
                        message: format!("'`{}' is redefined here", name),
                    },
                ])
            }
        };

        report.with_message(self.to_string())
    }
}

// impl Diagnostic for PreprocessorDiagnostic {
//     #[inline(always)]
//     fn annotation_type(&self, db: &dyn BaseDB, root_file: FileId) -> Option<AnnotationType> {
//         match self {
//             Self::MacroOverwritten(lint) => lint.annotation_type(db, root_file),
//             _ => Some(AnnotationType::Error),
//         }
//     }

//     fn slices(&self, db: &dyn BaseDB, root_file: FileId) -> Vec<DiagnosticSlice> {
//         match *self {
//             PreprocessorDiagnostic::UnexpectedToken(span) => vec![DiagnosticSlice {
//                 slice_span: span,
//                 messages: vec![(AnnotationType::Error, Text::const_str("unexpected token"), span)],
//                 fold: false,
//             }],

//             PreprocessorDiagnostic::MacroNotFound(_, span) => vec![DiagnosticSlice {
//                 slice_span: span,
//                 messages: vec![(AnnotationType::Error, Text::const_str("macro not found"), span)],
//                 fold: false,
//             }],
//             PreprocessorDiagnostic::MacroRecursion(_, span) => vec![DiagnosticSlice {
//                 slice_span: span,
//                 messages: vec![(
//                     AnnotationType::Error,
//                     Text::const_str("recursion occurred here"),
//                     span,
//                 )],
//                 fold: false,
//             }],

//             PreprocessorDiagnostic::IoError(span)
//             | PreprocessorDiagnostic::InvalidTextFormat(span) => {

//                 vec![DiagnosticSlice {
//                     slice_span: span,
//                     messages: vec![(
//                         AnnotationType::Error,
//                         Text::const_str("failed to read file"),
//                         span,
//                     )],
//                     fold: false,
//                 }]
//             }

//             PreprocessorDiagnostic::IoErrorRoot(_)
//             | PreprocessorDiagnostic::InvalidTextFormatRoot(_) => {
//                 vec![]
//             }

//             PreprocessorDiagnostic::MacroOverwritten(ref lint) => {
//                 Diagnostic::slices(lint, db, root_file)
//             }

//             PreprocessorDiagnostic::MissingOrUnexpectedToken { expected, expected_at, span } => {
//                 let mut expected_at = expected_at;
//                 expected_at.range = TextRange::at(expected_at.range.start(), 0.into());
//                 vec![DiagnosticSlice {
//                     slice_span: expected_at.extend(span, &db.sourcemap(root_file)),
//                     messages: vec![
//                         (AnnotationType::Error, Text::const_str("unexpected token"), span),
//                         (
//                             AnnotationType::Info,
//                             Text::owned(format!("expected {}", expected)),
//                             expected_at,
//                         ),
//                     ],
//                     fold: false,
//                 }]
//             }

//             PreprocessorDiagnostic::MacroArgumentCountMissmatch { expected, span, .. } => {
//                 vec![DiagnosticSlice {
//                     slice_span: span,
//                     messages: vec![(
//                         AnnotationType::Error,
//                         Text::owned(format!("expected {} arguments", expected)),
//                         span,
//                     )],
//                     fold: false,
//                 }]
//             }
//             PreprocessorDiagnostic::UnexpectedEof { expected: _, span } => vec![DiagnosticSlice {
//                 slice_span: span,
//                 messages: vec![(AnnotationType::Error, Text::const_str("unexpected EOF"), span)],
//                 fold: false,
//             }],
//         }
//     }
// }

// impl LintDiagnostic for MacroOverwritten {
//     #[inline(always)]
//     fn lint(&self) -> Lint {
//         builtin::macro_overwritten
//     }

//     fn sctx(&self) -> Option<SyntaxCtx> {
//         None
//     }

//     fn slices(&self, main_type: AnnotationType) -> Vec<DiagnosticSlice> {
//         let old = DiagnosticSlice {
//             slice_span: self.old,
//             messages: vec![(
//                 AnnotationType::Info,
//                 Text::const_str("First declared here"),
//                 self.old,
//             )],
//             fold: false,
//         };

//         let new = DiagnosticSlice {
//             slice_span: self.new,
//             messages: vec![(main_type, Text::const_str("Later overwritten here"), self.new)],
//             fold: false,
//         };

//         vec![old, new]
//     }
// }
