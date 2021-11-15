use basedb::{
    diagnostics::{text_ranges_to_unified_spans, Diagnostic, Label, LabelStyle, Report},
    lints::{
        builtin::{lint_level_owerwrite, unkown_lint},
        Lint, LintSrc,
    },
    BaseDB, FileId,
};
use stdx::impl_display;
use syntax::{sourcemap::FileSpan, TextRange};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AttrDiagnostic {
    ExpectedArrayOrLiteral { range: TextRange, attr: &'static str },
    ExpectedLiteral { range: TextRange, attr: &'static str },
    UnkownLint { range: TextRange, lint: String },
    LintOverwrite { old: TextRange, new: TextRange, name: String },
}

use AttrDiagnostic::*;

impl_display! {
    match AttrDiagnostic{
        ExpectedArrayOrLiteral{attr,..} => "'{}' attribute exptects a string literal or and array of literals",attr;
        ExpectedLiteral{attr,..} => "'{}' attribute expepects a string literal here", attr;
        UnkownLint{lint,..} => "unkown lint '{}'",lint;
        LintOverwrite{name,..} => "lint level for '{}' was set multiple times",name;
    }
}

impl Diagnostic for AttrDiagnostic {
    fn lint(&self) -> Option<(Lint, LintSrc)> {
        match self {
            UnkownLint { .. } => Some((unkown_lint, LintSrc::GLOBAL)),
            LintOverwrite { .. } => Some((lint_level_owerwrite, LintSrc::GLOBAL)),
            _ => None,
        }
    }

    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let sm = db.sourcemap(root_file);
        let parse = db.parse(root_file);

        match *self {
            ExpectedArrayOrLiteral { range, attr } => {
                let FileSpan { file: file_id, range } = parse.to_file_span(range, &sm);
                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "expected a literal or an array".to_owned(),
                    }])
                    .with_notes(vec![format!(
                        "help: valid examples are {0}=\"foo\" and {0}='{{\"foo\",\"bar\"}}",
                        attr
                    )])
            }
            ExpectedLiteral { range, .. } => {
                let FileSpan { file: file_id, range } = parse.to_file_span(range, &sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message: "expected a string literal".to_owned(),
                }])
            }
            LintOverwrite { old, new, .. } => {
                let (file_id, [new, old]) = text_ranges_to_unified_spans(&sm, &parse, [new, old]);
                Report::warning()
                    .with_labels(vec![
                        Label {
                            style: LabelStyle::Secondary,
                            file_id,
                            range: old.into(),
                            message: "lint lvl was first set here".to_owned(),
                        },
                        Label {
                            style: LabelStyle::Primary,
                            file_id,
                            range: new.into(),
                            message: "lint lvl was overwritten her".to_owned(),
                        },
                    ])
                    .with_notes(vec![
                        "help: the second lint lvl is used; the first attribute has no effect"
                            .to_owned(),
                    ])
            }
            UnkownLint { range, .. } => {
                let FileSpan { file: file_id, range } = parse.to_file_span(range, &sm);
                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "unkown lint".to_owned(),
                    }])
                    .with_notes(vec!["help: this attribute has no effect".to_owned()])
            }
        }
    }
}
