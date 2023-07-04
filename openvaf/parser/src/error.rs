use std::fmt::Debug;

use stdx::pretty;

use crate::Token;

#[derive(Debug, Clone)]
pub enum SyntaxError {
    // #[error("{name} was already declared in this Scope!")]
    // AlreadyDeclaredInThisScope { declaration: Span, other_declaration: Span, name: Box<str> },
    //
    // #[error("Unexpected Token!")]
    // MissingOrUnexpectedToken { expected: Token, expected_at: Span, span: Span },
    //
    // #[error("Reached 'endmodule' while still expecting an 'end' delimiter!")]
    // MismatchedDecimeters { start: Span, end: Span },
    //
    // #[error("Unexpected EOF! Expected {expected}")]
    // UnrecognizedEof { expected: ListFormatter<Vec<String>>, span: Span },
    // #[display(fmt = "unexpected token {}; expected {}", "found", "expected")]
    UnexpectedToken { expected: pretty::List<Vec<Token>>, found: Token },
    // ExtraToken { span: Span, token: Token },
    //
    // #[error("Unexpected Token!")]
    // UnexpectedToken { span: Span, ignored: Option<Span> },
}
