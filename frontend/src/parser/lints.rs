use crate::diagnostic::{DiagnosticSlice, Text};
use crate::lints::{builtin, Lint, LintDiagnostic};
use crate::symbol::Symbol;
use crate::Span;
use annotate_snippets::snippet::AnnotationType;
use core::fmt::Formatter;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct AtrributeOverwritten {
    pub old: Span,
    pub new: Span,
    pub name: Symbol,
}

impl Error for AtrributeOverwritten {}
impl Display for AtrributeOverwritten {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Attribute {} was overwritten", self.name))
    }
}

impl LintDiagnostic for AtrributeOverwritten {
    #[inline(always)]
    fn lint(&self) -> Lint {
        builtin::attribute_overwritten
    }

    fn slices(&self, main_type: AnnotationType) -> Vec<DiagnosticSlice> {
        let slice = DiagnosticSlice {
            slice_span: self.old.data().extend(self.new.data()),
            messages: vec![
                (
                    AnnotationType::Info,
                    Text::const_str("First declared here"),
                    self.old.data(),
                ),
                (
                    main_type,
                    Text::const_str("Overwritten here"),
                    self.new.data(),
                ),
            ],
            fold: false,
        };

        vec![slice]
    }
}
