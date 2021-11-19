use basedb::{
    diagnostics::{text_ranges_to_unified_spans, Diagnostic, Label, LabelStyle, Report},
    lints::{
        builtin::{lint_level_owerwrite, lint_not_found},
        ErasedItemTreeId, Lint, LintSrc,
    },
    BaseDB, FileId,
};
use stdx::impl_display;
use syntax::{sourcemap::FileSpan, TextRange};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AttrDiagnostic {
    ExpectedArrayOrLiteral { range: TextRange, attr: &'static str },
    ExpectedLiteral { range: TextRange, attr: &'static str },
    unknownLint { range: TextRange, lint: String, item_tree: ErasedItemTreeId },
    LintOverwrite { old: TextRange, new: TextRange, name: String, item_tree: ErasedItemTreeId },
}

use AttrDiagnostic::*;

impl_display! {
    match AttrDiagnostic{
        ExpectedArrayOrLiteral{attr,..} => "'{}' attribute exptects a string literal or and array of literals",attr;
        ExpectedLiteral{attr,..} => "'{}' attribute expepects a string literal here", attr;
        unknownLint{lint,..} => "unknown lint '{}'",lint;
        LintOverwrite{name,..} => "lint level for '{}' was set multiple times",name;
    }
}

impl Diagnostic for AttrDiagnostic {
    fn lint(&self) -> Option<(Lint, LintSrc)> {
        match self {
            unknownLint { item_tree, .. } => {
                Some((lint_not_found, LintSrc { overwrite: None, item_tree: Some(*item_tree) }))
            }
            LintOverwrite { item_tree, .. } => Some((
                lint_level_owerwrite,
                LintSrc { overwrite: None, item_tree: Some(*item_tree) },
            )),
            _ => None,
        }
    }

    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let sm = db.sourcemap(root_file);
        let parse = db.parse(root_file);

        let report = match *self {
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
            unknownLint { range, .. } => {
                let FileSpan { file: file_id, range } = parse.to_file_span(range, &sm);
                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "unknown lint".to_owned(),
                    }])
                    .with_notes(vec!["help: this attribute has no effect".to_owned()])
            }
        };

        report.with_message(self.to_string())
    }
}
