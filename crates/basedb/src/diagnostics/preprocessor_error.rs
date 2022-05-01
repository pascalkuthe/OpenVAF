use ahash::AHashSet;
use stdx::iter::zip;
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
                let text = db.file_text(span.file).unwrap();
                let src = &text[span.range];
                let mut dst = String::with_capacity(src.len());
                let mut lookalike = AHashSet::default();
                let mut has_unicode = false;

                dst.extend(src.chars().map(|c| {
                    let asscii = match c {
                        '\u{02BA}' | '\u{02DD}' | '\u{02EE}' | '\u{02F6}' | '\u{05F2}'
                        | '\u{05F4}' | '\u{1CD3}' | '\u{201C}' | '\u{201D}' | '\u{201F}'
                        | '\u{2033}' | '\u{2036}' | '\u{3003}' | '\u{FF02}' => '"',
                        '\u{0660}' | '\u{06F0}' | '\u{0701}' | '\u{0702}' | '\u{2024}'
                        | '\u{A4F8}' | '\u{A60E}' | '\u{10A50}' | '\u{1D16D}' => '.',
                        '\u{0060}' | '\u{00B4}' | '\u{02B9}' | '\u{02BB}' | '\u{02BC}'
                        | '\u{02BD}' | '\u{02BE}' | '\u{02C8}' | '\u{02CA}' | '\u{02CB}'
                        | '\u{02F4}' | '\u{0374}' | '\u{0384}' | '\u{055A}' | '\u{055D}'
                        | '\u{05D9}' | '\u{05F3}' | '\u{07F4}' | '\u{07F5}' | '\u{144A}'
                        | '\u{16CC}' | '\u{1FBD}' | '\u{1FBF}' | '\u{1FEF}' | '\u{1FFD}'
                        | '\u{1FFE}' | '\u{2018}' | '\u{2019}' | '\u{201B}' | '\u{2032}'
                        | '\u{2035}' | '\u{A78C}' | '\u{16F51}' | '\u{16F52}' | '\u{FF07}'
                        | '\u{FF40}' => '`',
                        '\u{02D7}' | '\u{06D4}' | '\u{2010}' | '\u{2011}' | '\u{2012}'
                        | '\u{2013}' | '\u{2043}' | '\u{2212}' | '\u{2796}' | '\u{2CBA}'
                        | '\u{FE58}' => '-',
                        '\u{2795}' => '+',
                        '\u{204E}' | '\u{2217}' | '\u{1031F}' => '*',
                        '\u{1735}' | '\u{2041}' | '\u{2044}' | '\u{2215}' | '\u{2571}'
                        | '\u{27CB}' | '\u{29F8}' | '\u{2CC6}' | '\u{2F03}' | '\u{3033}'
                        | '\u{30CE}' | '\u{31D3}' | '\u{4E3F}' | '\u{1D23A}' => '/',
                        '\u{2768}' => '(',
                        '\u{2769}' => ')',
                        '\u{00B8}' | '\u{060D}' | '\u{066B}' | '\u{201A}' | '\u{A4F9}' => ',',
                        '\u{037E}' => ';',
                        '\u{A778}' => '&',
                        '\u{2216}' | '\u{27CD}' | '\u{29F5}' | '\u{29F9}' | '\u{2F02}'
                        | '\u{31D4}' | '\u{4E36}' | '\u{1D20F}' | '\u{1D23B}' | '\u{FE68}'
                        | '\u{FF3C}' => '\\',
                        '\u{02C2}' | '\u{1438}' | '\u{16B2}' | '\u{2039}' | '\u{276E}'
                        | '\u{1D236}' => '<',
                        '\u{02C3}' | '\u{1433}' | '\u{203A}' | '\u{276F}' | '\u{16F3F}'
                        | '\u{1D237}' => '>',
                        '\u{2775}' => '}',
                        '\u{2774}' | '\u{1D114}' => '{',
                        '\u{1400}' | '\u{2E40}' | '\u{30A0}' | '\u{A4FF}' => '=',
                        '\u{0430}' | '\u{00e0}' | '\u{00e1}' | '\u{1ea1}' | '\u{0105}' => 'a',
                        '\u{01C3}' | '\u{2D51}' | '\u{FF01}' => '!',

                        '\u{0441}' | '\u{0188}' | '\u{010b}' => 'c',

                        '\u{0501}' | '\u{0257}' => 'd',

                        '\u{0435}' | '\u{1eb9}' | '\u{0117}' | '\u{00e9}' | '\u{00e8}' => 'e',
                        '\u{0121}' => 'g',
                        '\u{04bb}' => 'h',
                        '\u{0456}' | '\u{00ed}' | '\u{00ec}' | '\u{00ef}' => 'i',
                        '\u{03F3}' | '\u{0458}' | '\u{2149}' | '\u{1D423}' | '\u{1D457}'
                        | '\u{1D48B}' | '\u{1D4BF}' | '\u{1D4F3}' | '\u{1D527}' | '\u{1D55B}'
                        | '\u{1D58F}' | '\u{1D5C3}' | '\u{1D5F7}' | '\u{1D62B}' | '\u{1D65F}'
                        | '\u{1D693}' | '\u{FF4A}' => 'j',
                        '\u{04cf}' | '\u{1e37}' => 'l',
                        '\u{0578}' => 'n',
                        '\u{043e}' | '\u{03bf}' | '\u{0585}' | '\u{022f}' | '\u{1ecd}'
                        | '\u{1ecf}' | '\u{01a1}' | '\u{00f6}' | '\u{00f3}' | '\u{00f2}' => 'o',
                        '\u{0440}' => 'p',
                        '\u{0566}' => 'q',
                        '\u{0282}' => 's',
                        '\u{03c5}' | '\u{057d}' | '\u{00fc}' | '\u{00fa}' | '\u{00f9}' => 'u',
                        '\u{03bd}' | '\u{0475}' => 'v',
                        '\u{0445}' | '\u{04b3}' => 'x',
                        '\u{0443}' | '\u{00fd}' => 'y',
                        '\u{0290}' | '\u{017c}' => 'z',
                        _ => {
                            has_unicode |= !c.is_ascii();
                            return c;
                        }
                    };
                    has_unicode = true;
                    lookalike.insert((c, asscii));
                    asscii
                }));

                let mut notes = Vec::new();

                if has_unicode {
                    if !lookalike.is_empty() {
                        let mut help =
                            "help: It looks like you used characters that look similar to ascii: "
                                .to_owned();
                        for (_, (lookalike, ascii)) in zip(0..5, lookalike.iter()) {
                            help.push_str(&format!("\n{lookalike} instead of {ascii}"))
                        }

                        if lookalike.len() > 5 {
                            help.push_str("\n...")
                        }

                        notes.push(help);

                        let replaced = if dst.len() > 10{
                            format!("\n\'{dst}\'")
                        }else{
                            format!(" \'{dst}\'")
                        };
                        let info = format!("info: replacing these lookalikes with ascii yields:{replaced}");
                        notes.push(info);
                    }else{
                        notes = vec!["help: you have used unicode characters but Verilog-A only allows ascii".to_owned()];
                    }
                }

                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: span.file,
                        range: span.range.into(),
                        message: "unexpected token".to_owned(),
                    }])
                    .with_notes(notes)
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
