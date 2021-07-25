use syntax::{AstPtr, ast};

#[derive(Debug, PartialEq, Eq)]
pub enum DefDiagnosticKind {
    ReservedIdentifier { ast: AstPtr<ast::Name> },

}
