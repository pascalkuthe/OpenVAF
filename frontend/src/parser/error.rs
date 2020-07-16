//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::fmt::{Display, Formatter};

use annotate_snippets::snippet::AnnotationType;

use crate::diagnostic::{DiagnosticSlice, LibraryDiagnostic, Text, Unsupported};
use crate::parser::Token;
use crate::sourcemap::Span;
use crate::symbol::{Ident, Symbol};
use crate::util::ListFormatter;
use thiserror::Error;

pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Error, Debug, Clone)]
pub enum Error {
    //Parser
    #[error("Module ports declared in Module body when already declared in Module Head")]
    PortRedeclaration {
        module_head: Span,
        body_declaration: Span,
    },

    #[error("{port} has to be listed in the Module head!")]
    PortNotPreDeclaredInModuleHead { port_list: Span, port: Ident },

    #[error("Port {0} was pre declared in module head but not defined in the module body")]
    PortPreDeclaredNotDefined(Ident),

    // Nature/ Discipline attributes
    #[error("Attribute {name} was redefined!")]
    AttributeAlreadyDefined { name: Symbol, old: Span, new: Span },

    #[error("Nature is missing the required attributes {0}")]
    RequiredAttributeNotDefined(ListFormatter<Vec<Symbol>>, Span),

    #[error("Disciplines in the discrete Domain may not define flow/potential natures")]
    DiscreteDisciplineHasNatures {
        span: Span,
        discrete_declaration: Span,
        first_nature: Span,
        second_nature: Option<Span>,
    },

    #[error("{name} was already declared in this Scope!")]
    AlreadyDeclaredInThisScope {
        declaration: Span,
        other_declaration: Span,
        name: Symbol,
    },

    #[error("Unexpected EOF! Expected {expected}")]
    UnexpectedEofExpecting { expected: Token, span: Span },

    #[error("Unexpected EOF!")]
    UnexpectedEof { span: Span },

    #[error("Unexpected Token! Expected {expected}")]
    UnexpectedToken {
        expected: ListFormatter<Vec<Token>>,
        span: Span,
    },

    #[error("Unexpected Token! Expected {expected}")]
    MissingOrUnexpectedToken {
        expected: ListFormatter<Vec<Token>>,
        expected_at: Span,
        span: Span,
    },

    #[error("Unexpected Tokens! Expected {expected}")]
    UnexpectedTokens {
        expected: ListFormatter<Vec<Expected>>,
        span: Span,
    },

    #[error("Multiple default cases declared")]
    MultipleDefaultDeclarations { old: Span, new: Span },

    #[error("frontend does currently not support {0}!")]
    Unsupported(Unsupported, Span),

    #[error("Function {0} is missing a body")]
    FunctionWithoutBody(Ident, Span),

    #[error("{0} is not a valid finish number (allowed values are 0,1,2)")]
    IllegalFinishNumber(u32, Span),

    #[error("Error encountered after parser was already unrecoverable")]
    Unrecoverable,
}

#[derive(Debug, Copy, Clone)]
pub enum Expected {
    Identifier,
    PortDeclaration,
    Port,
    UnaryOperator,
    BinaryOperator,
    Primary,
    Statement,
    Expression,
    BranchAcess,
    ParameterRange,
    StringLiteral,
}

impl Display for Expected {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Identifier => f.write_str("identifier"),
            Self::PortDeclaration => f.write_str("port declaration"),
            Self::Port => f.write_str("port listing"),
            Self::UnaryOperator => f.write_str("unary operator (-,!)"),
            Self::BinaryOperator => f.write_str("binary operator (such as * or +)"),
            Self::Primary => f.write_str("expression primary"),
            Self::Statement => f.write_str("statement"),
            Self::BranchAcess => f.write_str("branch access"),
            Self::ParameterRange => f.write_str("parameter range (such as from [0:inf]"),
            Self::Expression => f.write_str("expression"),
            Self::StringLiteral => f.write_str("string literal"),
        }
    }
}

impl LibraryDiagnostic for Error {
    #[inline(always)]
    fn annotation_type(&self) -> Option<AnnotationType> {
        match self {
            Self::Unrecoverable => None,
            _ => Some(AnnotationType::Error),
        }
    }

    fn slices(&self) -> Vec<DiagnosticSlice> {
        match self {
            Error::MissingOrUnexpectedToken {
                expected,
                expected_at,
                span,
            } => {
                let mut expected_at = expected_at.data();
                expected_at.lo = expected_at.hi;
                vec![DiagnosticSlice {
                    slice_span: expected_at.extend(span.data()),
                    messages: vec![
                        (
                            AnnotationType::Error,
                            Text::const_str("Unexpected Token"),
                            span.data(),
                        ),
                        (
                            AnnotationType::Info,
                            Text::owned(format!("Expected {}", expected)),
                            expected_at,
                        ),
                    ],
                    fold: false,
                }]
            }

            Error::PortRedeclaration {
                module_head,
                body_declaration,
            } => vec![
                DiagnosticSlice {
                    slice_span: module_head.data(),
                    messages: vec![(
                        AnnotationType::Help,
                        Text::const_str("Ports already declared here"),
                        module_head.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: body_declaration.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Ports may not be declared here"),
                        body_declaration.data(),
                    )],
                    fold: false,
                },
            ],

            Error::PortNotPreDeclaredInModuleHead { port_list, port } => vec![
                DiagnosticSlice {
                    slice_span: port.span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Not declared in module head"),
                        port.span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: port_list.data(),
                    messages: vec![(
                        AnnotationType::Help,
                        Text::owned(format!("{} is missing here", port)),
                        port_list.data(),
                    )],
                    fold: false,
                },
            ],

            Error::PortPreDeclaredNotDefined(port) => vec![DiagnosticSlice {
                slice_span: port.span.data(),
                messages: vec![(
                    AnnotationType::Info,
                    Text::const_str("Port is not defined in module body"),
                    port.span.data(),
                )],
                fold: true,
            }],

            Error::AttributeAlreadyDefined { name, old, new } => vec![DiagnosticSlice {
                slice_span: old.data().extend(new.data()),
                messages: vec![
                    (
                        AnnotationType::Info,
                        Text::owned(format!("{} is declared here first", name)),
                        old.data(),
                    ),
                    (
                        AnnotationType::Error,
                        Text::owned(format!("{} is later re declared here", name)),
                        new.data(),
                    ),
                ],
                fold: true,
            }],

            Error::RequiredAttributeNotDefined(required_attr, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("{} are required", required_attr)),
                    span.data(),
                )],
                fold: true,
            }],

            Error::DiscreteDisciplineHasNatures {
                span,
                discrete_declaration,
                first_nature,
                second_nature,
            } => {
                let mut messages = vec![
                    (
                        AnnotationType::Info,
                        Text::const_str("Declared as discrete here"),
                        discrete_declaration.data(),
                    ),
                    (
                        AnnotationType::Error,
                        Text::const_str("Nature declared here"),
                        first_nature.data(),
                    ),
                ];
                if let Some(nature) = second_nature {
                    messages.push((
                        AnnotationType::Error,
                        Text::const_str("Nature declared here"),
                        nature.data(),
                    ))
                }
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages,
                    fold: true,
                }]
            }

            Error::AlreadyDeclaredInThisScope {
                declaration,
                other_declaration,
                ..
            } => vec![
                DiagnosticSlice {
                    slice_span: declaration.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Already declared in this scope!"),
                        declaration.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: other_declaration.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::const_str("Previous declaration here"),
                        other_declaration.data(),
                    )],
                    fold: false,
                },
            ],

            Error::UnexpectedEof { span } | Error::UnexpectedEofExpecting { span, .. } => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Unexpected EOF"),
                        span.data(),
                    )],
                    fold: true,
                }]
            }

            Error::UnexpectedToken { span, .. } | Error::UnexpectedTokens { span, .. } => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Unexpected Token"),
                        span.data(),
                    )],
                    fold: false,
                }]
            }

            Error::Unsupported(unsupported, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("frontend does currently not allow {}", unsupported)),
                    span.data(),
                )],
                fold: false,
            }],

            Error::FunctionWithoutBody(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Function body is missing"),
                    span.data(),
                )],
                fold: false,
            }],

            Error::IllegalFinishNumber(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected 0, 1 or 2"),
                    span.data(),
                )],
                fold: false,
            }],

            Error::Unrecoverable => Vec::new(),
            Error::MultipleDefaultDeclarations { old, new } => vec![
                DiagnosticSlice {
                    slice_span: new.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("default case declared again"),
                        new.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: old.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::const_str("default case already declared here"),
                        old.data(),
                    )],
                    fold: false,
                },
            ],
        }
    }
}
