/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

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
    // #[error("Reached 'endmodule' while stil expecting an 'end' delimiter!")]
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
