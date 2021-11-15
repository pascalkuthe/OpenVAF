use basedb::{
    diagnostics::{Diagnostic, Label, LabelStyle, Report},
    lints::{builtin::vams_keyword_compat, ErasedItemTreeId, Lint, LintSrc},
    BaseDB, FileId,
};
use stdx::impl_display;
use syntax::{sourcemap::FileSpan, TextRange};

use crate::Name;

#[derive(Debug, PartialEq, Eq)]
pub enum DefDiagnostic {
    // TODO also warn for attributes?
    ReservedIdentifier { range: TextRange, compat: bool, name: Name, item_tree: ErasedItemTreeId },
}

use DefDiagnostic::*;

impl_display! {
    match DefDiagnostic{
        ReservedIdentifier{name,..}=>"reserved keyword '{}' was used as an identifier",name;
    }
}

impl Diagnostic for DefDiagnostic {
    fn lint(&self) -> Option<(Lint, LintSrc)> {
        match self {
            DefDiagnostic::ReservedIdentifier { compat: true, item_tree, .. } => Some((
                vams_keyword_compat,
                LintSrc { overwrite: None, item_tree: Some(*item_tree) },
            )),
            _ => None,
        }
    }

    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let sm = db.sourcemap(root_file);
        let parse = db.parse(root_file);

        let report = match *self {
            ReservedIdentifier { range, compat, ref name, .. } => {
                let FileSpan { file, range } = parse.to_file_span(range, &sm);

                let report = Report::error().with_code("E001").with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id: file,
                    range: range.into(),
                    message: format!("'{}' is a keyword", name),
                }]);

                // TODO error code (doc)

                if compat {
                    report.with_notes(vec![format!(
                        "'{}' will likely never be used in the implemented language subset so this use is allowed.
To maintain compatibility with the VAMS standard this should be renamed",
                        name
                    )])
                } else {
                    report
                }
            }
        };

        report.with_message(self.to_string())
    }
}
