/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use core::fmt::Debug;
use core::fmt::Formatter;
use std::fmt::Display;

use thiserror::Error;

use diagnostics::{AnnotationType, DiagnosticSlice, FooterItem, LibraryDiagnostic, Text};
use diagnostics::{ListFormatter, HINT_UNSUPPORTED};
use hir::{AllowedOperation, AllowedOperations, Hir};
use ir::ids::DisciplineId;
use session::sourcemap::Span;
use session::symbols::kw;
use session::symbols::{Ident, Symbol};

pub type Result<T = ()> = std::result::Result<T, Error>;
use crate::allowed_operations::AccessLocation;
use data_structures::iter::Itertools;

#[derive(Clone, Debug)]
pub struct NetInfo {
    pub discipline: Symbol,
    pub name: Symbol,
    pub declaration: Span,
}

impl Display for NetInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} (discipline: {})", self.name, self.discipline))
    }
}

#[derive(Error, Clone, Debug, Eq, PartialEq, Copy)]
pub enum AllowedNatures {
    None, // Discrete
    PortPotential,
    Potential(Symbol, Symbol),
    Flow(Symbol, Symbol),
    PotentialAndFlow {
        potential_nature: Symbol,
        potential_nature_access: Symbol,
        flow_nature: Symbol,
        flow_nature_access: Symbol,
    },
}

impl AllowedNatures {
    pub fn from_discipline(discipline: DisciplineId, hir: &Hir) -> Self {
        let discipline = &hir[discipline];
        match (discipline.potential_nature, discipline.flow_nature) {
            (None, None) => Self::None,
            (Some(pot), None) => Self::Potential(hir[pot].ident.name, hir[pot].access.name),
            (None, Some(flow)) => Self::Flow(hir[flow].ident.name, hir[flow].access.name),
            (Some(pot), Some(flow)) => Self::PotentialAndFlow {
                potential_nature: hir[pot].ident.name,
                potential_nature_access: hir[pot].access.name,
                flow_nature: hir[flow].ident.name,
                flow_nature_access: hir[flow].access.name,
            },
        }
    }

    pub fn from_port_discipline(discipline: DisciplineId, hir: &Hir) -> Self {
        let discipline = &hir[discipline];
        if let Some(flow) = discipline.flow_nature {
            Self::Flow(hir[flow].ident.name, hir[flow].access.name)
        } else {
            Self::None
        }
    }

    fn to_allowed_list(self) -> Vec<Symbol> {
        match self {
            Self::None | Self::PortPotential => vec![],
            Self::Potential(_, pot) => vec![kw::potential, pot],
            Self::Flow(_, flow) => vec![kw::flow, flow],
            Self::PotentialAndFlow { flow_nature_access, potential_nature_access, .. } => {
                vec![kw::potential, kw::flow, potential_nature_access, flow_nature_access]
            }
        }
    }
}

impl Display for AllowedNatures {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => f.write_str(
                "Discrete Disciplines can not be accessed using branch probes"
            ),
            Self::PortPotential => f.write_str(
                "Only the flow of port branches can be read but this port is defined for a discipline without flow as such it can't be accessed using branch probes"
            ),
            Self::Potential(pot, pot_access) => f.write_fmt(format_args!(
                "It can only be accessed using its Potential ({}) with '{}'/'pot'",
                pot,
                pot_access,
            )),
            Self::Flow(flow,flow_access) => f.write_fmt(format_args!(
                "It can only be accessed using its Flow ({})  with '{}'/'flow'",
                flow,
                flow_access
            )),
            Self::PotentialAndFlow{ potential_nature, potential_nature_access, flow_nature, flow_nature_access } => f.write_fmt(format_args!(
                "It can only be accessed using its Potential ({}) with '{}'/'flow' or its Flow ({}) with '{}'/'flow'",
                potential_nature, potential_nature_access,
                flow_nature, flow_nature_access,
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MockSymbolDeclaration {
    Module,
    Block,
    Variable,
    Branch,
    PortBranch,
    Node,
    Port,
    Function,
    Discipline,
    Nature,
    NatureAccess,
    Parameter,
}

impl Display for MockSymbolDeclaration {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Module => f.write_str("module"),
            Self::Block => f.write_str("block"),
            Self::Variable => f.write_str("variable"),
            Self::Branch => f.write_str("branch"),
            Self::PortBranch => f.write_str("port branch"),
            Self::Node => f.write_str("net"),
            Self::Port => f.write_str("port"),
            Self::Function => f.write_str("function"),
            Self::Discipline => f.write_str("discipline"),
            Self::Nature => f.write_str("nature"),
            Self::NatureAccess => f.write_str("nature access"),
            Self::Parameter => f.write_str("parameter"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AttributeKind {
    DisciplinePotential(Ident),
    DisciplineFlow(Ident),
    DisciplineDomain(Ident),
    NatureAttribute(Ident, Ident),
}

impl AttributeKind {
    pub fn short(&self) -> Symbol {
        match self {
            Self::DisciplinePotential(_) => kw::potential,
            Self::DisciplineFlow(_) => kw::flow,
            Self::DisciplineDomain(_) => kw::domain,
            Self::NatureAttribute(ident, _) => ident.name,
        }
    }
}

impl Display for AttributeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DisciplineDomain(discipline) => {
                f.write_fmt(format_args!("the potential nature of {}", discipline))
            }

            Self::DisciplinePotential(discipline) => {
                f.write_fmt(format_args!("the flow nature of {}", discipline))
            }

            Self::DisciplineFlow(discipline) => {
                f.write_fmt(format_args!("the domain of {}", discipline))
            }

            Self::NatureAttribute(attr, nature) => {
                f.write_fmt(format_args!("the '{}' attribute of {}", attr, nature))
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum IllegalNatureAttributeOverwriteKind {
    Abstol,
    Access,
}

impl Display for IllegalNatureAttributeOverwriteKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Abstol => f.write_str("'abstol'"),
            Self::Access => f.write_str("'access'"),
        }
    }
}

#[derive(Error, Clone, Debug)]
pub enum Error {
    #[error("overwriting {kind} in derived nature '{nature}' is forbidden! ")]
    IllegalNatureAttributeOverwrite {
        kind: IllegalNatureAttributeOverwriteKind,
        nature: Ident,
        parent: Ident,
        overwrite: Span,
    },

    #[error("the nature {name} is derived from {parent} which is itself derived from {name}")]
    CircularAttributeInheritance { name: Ident, parent: Ident, trace_back: Vec<Ident> },

    #[error("disciplines in the discrete Domain may not define flow/potential natures")]
    DiscreteDisciplineHasNatures {
        span: Span,
        discrete_declaration: Span,
        first_nature: Span,
        second_nature: Option<Span>,
    },

    #[error("{attribute_kind} was defined multiple times")]
    AttributeAlreadyDefined { attribute_kind: AttributeKind, old: Span, new: Span },

    #[error("module ports declared in Module body when already declared in module head")]
    PortRedeclaration { module_head: Span, body_declaration: Span },

    #[error("the port '{port}' has to be listed in the module head!")]
    PortNotPreDeclaredInModuleHead { port_list: Span, port: Ident },

    #[error("port {0} was pre declared in module head but not defined in the module body")]
    PortPreDeclaredNotDefined(Ident),

    #[error("nature {1} is missing the required attributes {0}")]
    RequiredBaseNatureAttributesNotDefined(ListFormatter<Vec<Symbol>>, Ident, Span),

    #[error("case statements may only have one default case")]
    MultipleDefaultDeclarations { old: Span, new: Span },

    #[error("function argument {0} is missing separate type declaration")]
    FunctionArgTypeDeclarationMissing(Ident),

    #[error("'{0}' was not found in the current scope!")]
    NotFound(Ident),

    #[error("'{0}' was not found in '{1}'!")]
    NotFoundIn(Ident, Symbol),

    #[error("'{0}' was declared without a discipline!")]
    NetWithoutDiscipline(Ident),

    #[error("'{net}' was declared with an illegal net type {net_type}!")]
    UnsupportedNetType { net: Ident, net_type: Ident },

    #[error("{declaration_name} is not a scope!")]
    NotAScope { declaration: Span, declaration_name: Symbol, span: Span },

    #[error("expected {expected} but found {found} {name}")]
    DeclarationTypeMismatch {
        expected: ListFormatter<Vec<MockSymbolDeclaration>>,
        found: MockSymbolDeclaration,
        name: Symbol,
        span: Span,
    },

    #[error("branch probe functions expect at most 2 arguments")]
    TooManyBranchAccessArgs { nature: Symbol, span: Span },

    #[error("branch probe functions expect at least 1 argument")]
    EmptyBranchAccess { nature: Symbol, span: Span },

    #[error("{0} only accepts identifiers!")]
    ExpectedIdentifier(&'static str, Span),

    #[error("this branch can not be accessed by {nature}. {allowed_natures}")]
    NatureNotPotentialOrFlow { nature: Ident, allowed_natures: AllowedNatures },

    #[error("the disciplines of {0} and {1} are incompatible")]
    DisciplineMismatch(NetInfo, NetInfo, Span),

    #[error("{op} is not allowed in {loc}!")]
    NotAllowedInContext {
        op: AllowedOperation,
        span: Span,
        loc: AccessLocation,
        allowed: AllowedOperations,
    },

    #[error("access to {kind} '{ident}' defined outside of the function is not allowed")]
    NonLocalAccess { kind: MockSymbolDeclaration, ident: Ident },

    #[error("contribution can only be made to branch probes (for example 'I(net1,net2)', 'potential(net)' or 'flow(branch)')")]
    ContributeToNonBranchProbe(Span),

    #[error("port branches ( like (<PORT>) ) can only be read but not contributed to")]
    ContributeToBranchPortProbe(Span),

    #[error("$limit calls without an explicit function are not supported")]
    LimRequiresFunction(Span),

    #[error("limit functions me be a function Identifier or a string literal")]
    IllegalLimitFn(Span),

    #[error("port flow (like I(< PORT >) ) can not be used in a $lim function")]
    PortFlowCanNotBeANonLinearity(Span),

    #[error("'{ident}' is a reserved keyword of the VerilogAMS language and may not be used as an identifier for a {decl_kind}!")]
    ReservedSymbol { decl_kind: MockSymbolDeclaration, ident: Ident },
}

impl LibraryDiagnostic for Error {
    #[inline(always)]
    fn footer(&self) -> Vec<FooterItem> {
        match self{
            Self::CircularAttributeInheritance { trace_back, .. }  => vec![FooterItem {
                id: None,
                label: Text::owned(format!(
                    "Traceback: {}",
                    ListFormatter::with_final_seperator(trace_back.as_slice(), ", ")
                )),
                annotation_type: AnnotationType::Info,
            }],
            Self::RequiredBaseNatureAttributesNotDefined(_, _, _) => vec![FooterItem {
                id: None,
                label: Text::const_str("every base nature (a nature that has not parent) has to define the 'abstol', 'access' and 'units' attribute"),
                annotation_type: AnnotationType::Help,
            }],

            Self::LimRequiresFunction(_) => vec![
                FooterItem {
                    id: None,
                    label: Text::const_str(HINT_UNSUPPORTED),
                    annotation_type: AnnotationType::Help,
                },
                FooterItem {
                    id: None,
                    label: Text::const_str("to call a simulator limitation function provide the name of the function as string. For example\n\t$limit(V(b,e), \"pnjlimit\", vce, vt) )"),
                    annotation_type: AnnotationType::Help,
                },
                FooterItem {
                    id: None,
                    label: Text::const_str("to use your own limiting strategy create a VerilogA function and provide the name of the function as string. For example\n\t$limit(V(d,f), custom_pnjlimit, extra_arg1, vce, extra_arg2, vt)"),
                    annotation_type: AnnotationType::Help,
                },
            ],
            Self::NetWithoutDiscipline(_) => vec![
                FooterItem {
                    id: None,
                    label: Text::const_str(HINT_UNSUPPORTED),
                    annotation_type: AnnotationType::Help,
                }
            ],

            Self::UnsupportedNetType {..} => vec![

                FooterItem {
                    id: None,
                    label: Text::const_str("only nets without explicit type and with 'ground' type are supported"),
                    annotation_type: AnnotationType::Help,
                }
            ],

            Self::NotAllowedInContext{allowed,loc,..} => {
                let allowed= ListFormatter::with_final_seperator(allowed.iter().collect_vec()," and ");
                vec![
                    FooterItem {
                        id: None,
                        label: Text::owned(format!("only following operations are allowed in {}:\n{}",loc, allowed)),
                        annotation_type: AnnotationType::Help,
                    }
                ]
            },
            _ => Vec::new(),
        }
    }

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
            Self::DiscreteDisciplineHasNatures {
                span,
                discrete_declaration,
                first_nature,
                second_nature,
            } => {
                let mut messages = vec![
                    (
                        AnnotationType::Info,
                        Text::const_str("declared as discrete here"),
                        discrete_declaration.data(),
                    ),
                    (
                        AnnotationType::Error,
                        Text::const_str("nature declared here"),
                        first_nature.data(),
                    ),
                ];
                if let Some(nature) = second_nature {
                    messages.push((
                        AnnotationType::Error,
                        Text::const_str("nature declared here"),
                        nature.data(),
                    ))
                }
                vec![DiagnosticSlice { slice_span: span.data(), messages, fold: true }]
            }

            Self::AttributeAlreadyDefined { attribute_kind, old, new } => vec![DiagnosticSlice {
                slice_span: old.data().extend(new.data()),
                messages: vec![
                    (
                        AnnotationType::Info,
                        Text::owned(format!("{} is declared here first", attribute_kind.short())),
                        old.data(),
                    ),
                    (
                        AnnotationType::Error,
                        Text::owned(format!(
                            "{} is later re declared here",
                            attribute_kind.short()
                        )),
                        new.data(),
                    ),
                ],
                fold: true,
            }],

            Self::PortRedeclaration { module_head, body_declaration } => vec![
                DiagnosticSlice {
                    slice_span: module_head.data(),
                    messages: vec![(
                        AnnotationType::Help,
                        Text::const_str("ports already declared here"),
                        module_head.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: body_declaration.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("ports may not be declared here"),
                        body_declaration.data(),
                    )],
                    fold: false,
                },
            ],

            Self::PortNotPreDeclaredInModuleHead { port_list, port } => vec![
                DiagnosticSlice {
                    slice_span: port.span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("not declared in module head"),
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

            Self::PortPreDeclaredNotDefined(port) => vec![DiagnosticSlice {
                slice_span: port.span.data(),
                messages: vec![(
                    AnnotationType::Info,
                    Text::const_str("port is not defined in module body"),
                    port.span.data(),
                )],
                fold: true,
            }],

            Self::RequiredBaseNatureAttributesNotDefined(required_attr, _, span) => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(format!("{} are required", required_attr)),
                        span.data(),
                    )],
                    fold: true,
                }]
            }

            Self::FunctionArgTypeDeclarationMissing(Ident { span, .. }) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("missing seperate type declaration"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NotFound(Ident { span, .. }) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("not found in this scope"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NotFoundIn(Ident { span, .. }, scope) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("not found in {}", scope)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NotAScope { declaration, declaration_name, span } => vec![
                DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("expected a scope"),
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
                    Text::owned(format!("expected {}", expected)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::TooManyBranchAccessArgs { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("too many arguments"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::EmptyBranchAccess { span, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("expected at least 1 Argument!"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::ExpectedIdentifier(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("unexpected token!"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NatureNotPotentialOrFlow { nature: Ident { span, .. }, allowed_natures } => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::owned(format!(
                            "Expected {}",
                            ListFormatter::new(allowed_natures.to_allowed_list())
                        )),
                        span.data(),
                    )],
                    fold: false,
                }]
            }

            Self::DisciplineMismatch(net1, net2, span) => vec![
                DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("disciplines mismatch"),
                        span.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: net1.declaration.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("'{}' was declared here", net1.name)),
                        net1.declaration.data(),
                    )],
                    fold: false,
                },
                DiagnosticSlice {
                    slice_span: net2.declaration.data(),
                    messages: vec![(
                        AnnotationType::Info,
                        Text::owned(format!("'{}' was declared here", net2.name)),
                        net2.declaration.data(),
                    )],
                    fold: false,
                },
            ],

            Self::NotAllowedInContext { span, loc, .. } => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("not allowed in {}", loc)),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NonLocalAccess { ident, .. } => vec![DiagnosticSlice {
                slice_span: ident.span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("accessing items defined outside of the function is forbidden"),
                    ident.span.data(),
                )],
                fold: false,
            }],

            Self::NetWithoutDiscipline(ident) => vec![DiagnosticSlice {
                slice_span: ident.span.data(),
                messages: vec![(
                    AnnotationType::Help,
                    Text::const_str("add discipline (such as 'electrical'"),
                    ident.span.data(),
                )],
                fold: false,
            }],

            Self::ContributeToNonBranchProbe(span) | Self::ContributeToBranchPortProbe(span) => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("can only contribute to branches"),
                        span.data(),
                    )],
                    fold: false,
                }]
            }

            Self::MultipleDefaultDeclarations { new, old } => vec![DiagnosticSlice {
                slice_span: old.data().extend(new.data()),
                messages: vec![
                    (
                        AnnotationType::Info,
                        Text::const_str("default case declared here"),
                        new.data(),
                    ),
                    (
                        AnnotationType::Error,
                        Text::const_str("second default case is forbidden"),
                        old.data(),
                    ),
                ],
                fold: false,
            }],

            Self::CircularAttributeInheritance { name, parent, .. } => vec![DiagnosticSlice {
                slice_span: name.span.data().extend(parent.span.data()),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("circular derived nature"),
                    parent.span.data(),
                )],
                fold: false,
            }],

            Self::IllegalNatureAttributeOverwrite { kind, nature, overwrite, parent } => {
                vec![DiagnosticSlice {
                    slice_span: nature.span.data().extend(parent.span.data()),
                    messages: vec![
                        (
                            AnnotationType::Error,
                            Text::owned(format!(
                                "overwriting {} in derived natures is forbidden",
                                kind
                            )),
                            overwrite.data(),
                        ),
                        (
                            AnnotationType::Info,
                            Text::owned(format!("{} is derived from {}", nature, parent)),
                            nature.span.data().extend(parent.span.data()),
                        ),
                    ],
                    fold: true,
                }]
            }
            Self::LimRequiresFunction(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("expected at least 2 arguments"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::IllegalLimitFn(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("expected function identifier or string literal"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::PortFlowCanNotBeANonLinearity(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("expected a branch access such as V(b,e)"),
                    span.data(),
                )],
                fold: false,
            }],
            Self::ReservedSymbol { ident, .. } => vec![DiagnosticSlice {
                slice_span: ident.span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("this is a reserved keyword"),
                    ident.span.data(),
                )],
                fold: false,
            }],
            Self::UnsupportedNetType { net, net_type } => vec![DiagnosticSlice {
                slice_span: net.span.data().extend(net_type.span.data()),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("illegal net type"),
                    net_type.span.data(),
                )],
                fold: false,
            }],
        }
    }
}
