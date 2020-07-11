/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::fmt::Display;

use annotate_snippets::snippet::AnnotationType;
use core::fmt::Formatter;

use crate::diagnostic::{DiagnosticSlice, LibraryDiagnostic, Text, Unsupported};
use crate::ir::DisciplineId;
use crate::symbol::keywords;
use crate::symbol::{Ident, Symbol};
use crate::util::{format_list, ListFormatter};
use crate::{Hir, Span};
use core::fmt::Debug;
use thiserror::Error;

pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Clone, Debug)]
pub struct NetInfo {
    pub discipline: Symbol,
    pub name: Symbol,
    pub declaration: Span,
}

impl Display for NetInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} (discipline: {})",
            self.name, self.discipline
        ))
    }
}

#[derive(Error, Clone, Debug, Eq, PartialEq, Copy)]
pub enum AllowedNatures {
    None, // Discrete
    Potential(Symbol),
    Flow(Symbol),
    PotentialAndFlow(Symbol, Symbol),
}

impl AllowedNatures {
    pub fn from_discipline(discipline: DisciplineId, hir: &Hir) -> Self {
        let discipline = &hir[discipline].contents;
        match (discipline.potential_nature, discipline.flow_nature) {
            (None, None) => Self::None,
            (Some(pot), None) => Self::Potential(hir[pot].contents.ident.name),
            (None, Some(flow)) => Self::Flow(hir[flow].contents.ident.name),
            (Some(pot), Some(flow)) => {
                Self::PotentialAndFlow(hir[pot].contents.ident.name, hir[flow].contents.ident.name)
            }
        }
    }

    fn to_allowed_list(&self) -> Vec<Symbol> {
        match *self {
            Self::None => vec![],
            Self::Potential(pot) => vec![keywords::potential, pot],
            Self::Flow(flow) => vec![keywords::flow, flow],
            Self::PotentialAndFlow(pot, flow) => {
                vec![keywords::potential, keywords::flow, pot, flow]
            }
        }
    }
}

impl Display for AllowedNatures {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => f.write_fmt(format_args!(
                "Discrete Disciplines can not be accessed using branch Probes"
            )),
            Self::Potential(pot) => f.write_fmt(format_args!(
                "It can only be accessed using its Potential ({})",
                pot
            )),
            Self::Flow(flow) => f.write_fmt(format_args!(
                "It can only be accessed using its Flow ({})",
                flow
            )),
            Self::PotentialAndFlow(pot, flow) => f.write_fmt(format_args!(
                "It can only be accessed using its Potential ({}) or Flow ({})",
                pot, flow
            )),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NonConstantExpression {
    VariableReference,
    BranchAccess,
    FunctionCall,
    AnalogFilter,
}
impl Display for NonConstantExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::VariableReference => f.write_str("Variable references"),
            Self::BranchAccess => f.write_str("Branch probe calls"),
            Self::FunctionCall => f.write_str("Function calls"),
            Self::AnalogFilter => f.write_str("Analog filters"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NotAllowedInFunction {
    BranchAccess,
    AnalogFilters,
    NamedBlocks,
    Contribute,
    NonLocalAccess,
}

impl Display for NotAllowedInFunction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::BranchAccess => f.write_str("Accessing branches"),
            Self::AnalogFilters => f.write_str("Analog filter functions"),
            Self::NamedBlocks => f.write_str("Named blocks"),
            Self::NonLocalAccess => f.write_str("Accessing items outside of the function"),
            Self::Contribute => f.write_str("Contribute statements (<+)"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MockSymbolDeclaration {
    Module,
    Block,
    Variable,
    Branch,
    Net,
    Port,
    Function,
    Discipline,
    Nature,
    Parameter,
}
impl Display for MockSymbolDeclaration {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Module => f.write_str("module"),
            Self::Block => f.write_str("block"),
            Self::Variable => f.write_str("variable"),
            Self::Branch => f.write_str("branch"),
            Self::Net => f.write_str("net"),
            Self::Port => f.write_str("port"),
            Self::Function => f.write_str("function"),
            Self::Discipline => f.write_str("discipline"),
            Self::Nature => f.write_str("nature"),
            Self::Parameter => f.write_str("parameter"),
        }
    }
}

#[derive(Error, Clone, Debug)]
pub enum Error {
    #[error("Function argument {0} is missing separate type declaration")]
    FunctionArgTypeDeclarationMissing(Ident),

    #[error("'{0}' was not found in the current scope!")]
    NotFound(Ident),

    #[error("'{0}' was not found in {1} the current scope!")]
    NotFoundIn(Ident, Symbol),

    #[error("{declaration_name} is not a scope!")]
    NotAScope {
        declaration: Span,
        declaration_name: Symbol,
        span: Span,
    },

    #[error("Expected {expected} but found {found} {name}")]
    DeclarationTypeMismatch {
        expected: ListFormatter<Vec<MockSymbolDeclaration>>,
        found: MockSymbolDeclaration,
        name: Symbol,
        span: Span,
    },

    #[error("Branch probe functions expect at most 2 Arguments")]
    TooManyBranchAccessArgs { nature: Symbol, span: Span },

    #[error("Branch probe functions expect at least 1 Argument")]
    EmptyBranchAccess { nature: Symbol, span: Span },

    #[error("Branch probe function calls only accepts nets and branches as arguments!")]
    UnexpectedToken(Span),

    #[error("This branch can not be accessed by {nature}. {allowed_natures}")]
    NatureNotPotentialOrFlow {
        nature: Ident,
        allowed_natures: AllowedNatures,
    },

    #[error("The disciplines of {0} and {1} are incompatible")]
    DisciplineMismatch(NetInfo, NetInfo, Span),

    #[error("{0} are not allowed in constant expressions!")]
    NotAllowedInConstantContext(NonConstantExpression, Span),

    #[error("{0} is now allowed inside analog functions!")]
    NotAllowedInFunction(NotAllowedInFunction, Span),

    #[error("frontend does currently not support {0}")]
    Unsupported(Unsupported, Span),

    #[error("Partial derivatives may only be calculated over node potentials or branch flows")]
    DerivativeNotAllowed(Span),
}

impl LibraryDiagnostic for Error {
    #[inline(always)]
    fn id(&self) -> Option<&'static str> {
        // TODO error documentation
        None
    }

    fn annotation_type(&self) -> Option<AnnotationType> {
        Some(AnnotationType::Error)
    }

    fn slices(&self) -> Vec<DiagnosticSlice> {
        match self {
            Self::NotAllowedInFunction(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Not allowed inside a function"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::FunctionArgTypeDeclarationMissing(Ident { span, .. }) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Missing seperate type declaration"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NotFound(Ident { span, .. }) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Not found in this scope"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NotFoundIn(Ident { span, .. }, scope) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Not found in {}", scope)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NotAScope {
                declaration,
                declaration_name,
                span,
            } => vec![
                DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Expected a scope"),
                        span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: declaration.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("{} was declared here", declaration_name)),
                        declaration.data(),
                    )],
                    fold: false,
                },
            ],

            Self::DeclarationTypeMismatch { expected, span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected {}", expected)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::TooManyBranchAccessArgs { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Too many arguments"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::EmptyBranchAccess { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected at least 1 Argument!"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::UnexpectedToken(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Unexpected token!"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NatureNotPotentialOrFlow {
                nature: Ident { span, .. },
                allowed_natures,
            } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!(
                        "Expected {}",
                        format_list(allowed_natures.to_allowed_list())
                    )),
                    span.data(),
                )],
                fold: false,
            }],

            Self::DisciplineMismatch(net1, net2, span) => vec![
                DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Disciplines mismatch"),
                        span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: net1.declaration.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("{} was declared here", net1.name)),
                        net1.declaration.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: net2.declaration.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("{} was declared here", net2.name)),
                        net2.declaration.data(),
                    )],
                    fold: false,
                },
            ],

            Self::NotAllowedInConstantContext(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Not allowed in constant expressions"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::Unsupported(unsupported, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("frontend does currently not allow {}", unsupported)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::DerivativeNotAllowed(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected node potential or branch flow"),
                    span.data(),
                )],
                fold: false,
            }],
        }
    }
}
