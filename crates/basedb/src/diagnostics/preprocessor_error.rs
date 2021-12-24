use syntax::PreprocessorDiagnostic;
use vfs::FileId;

use crate::diagnostics::{to_unified_spans, Diagnostic, Label, LabelStyle, Report};
use crate::lints::{self, Lint, LintSrc};
use crate::BaseDB;

impl Diagnostic for PreprocessorDiagnostic {
    fn lint(&self, _root_file: FileId, _db: &dyn BaseDB) -> Option<(Lint, LintSrc)> {
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
            PreprocessorDiagnostic::FileNotFound { span, .. } => {
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
            PreprocessorDiagnostic::InvalidTextFormat { span, ref file, ref err, .. } => {
                let file = db.vfs().read().file_id(file).unwrap();
                let mut labels: Vec<_> = err
                    .pos
                    .iter()
                    .map(|span| Label {
                        style: LabelStyle::Primary,
                        file_id: file,
                        range: span.clone(),
                        message: "invalid text format!".to_owned(),
                    })
                    .collect();

                if let Some(span) = span {
                    let span = span.to_file_span(&sm);
                    labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: span.file,
                        range: span.range.into(),
                        message: "file was read here".to_owned(),
                    })
                };
                Report::error()
                    .with_labels(labels)
                    .with_notes(vec!["only UTF-8 files are accepted".to_owned()])
                    .with_notes(vec!["help: use --encode-lossy to use the file as-is".to_owned()])
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
