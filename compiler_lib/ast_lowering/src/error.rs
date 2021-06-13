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

use openvaf_ast::NatureAttribute;
use openvaf_diagnostics::ListFormatter;
use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, FooterItem, LibraryDiagnostic, Text};
use openvaf_hir::Hir;
use openvaf_ir::ids::DisciplineId;
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::keywords;
use openvaf_session::symbols::{Ident, Symbol};

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

    fn to_allowed_list(&self) -> Vec<Symbol> {
        match *self {
            Self::None | Self::PortPotential => vec![],
            Self::Potential(_, pot) => vec![keywords::potential, pot],
            Self::Flow(_, flow) => vec![keywords::flow, flow],
            Self::PotentialAndFlow {
                flow_nature_access,
                potential_nature_access,
                ..
            } => vec![
                keywords::potential,
                keywords::flow,
                potential_nature_access,
                flow_nature_access,
            ],
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NonConstantExpression {
    VariableReference,
    ParameterReference,
    PortReferences,
    NetReferences,
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
            Self::ParameterReference => f.write_str("Parameter references"),
            Self::PortReferences => f.write_str("Port references"),
            Self::NetReferences => f.write_str("Net references"),
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
    PortBranch,
    Net,
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
            Self::Net => f.write_str("net"),
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
    NatureAttribute(NatureAttribute, Ident),
}

impl AttributeKind {
    pub fn short(&self) -> Symbol {
        match self {
            Self::DisciplinePotential(_) => keywords::potential,
            Self::DisciplineFlow(_) => keywords::flow,
            Self::DisciplineDomain(_) => keywords::domain,
            Self::NatureAttribute(NatureAttribute::Abstol, _) => keywords::abstol,
            Self::NatureAttribute(NatureAttribute::Access, _) => keywords::access,
            Self::NatureAttribute(NatureAttribute::DerivativeNature, _) => keywords::ddt_nature,
            Self::NatureAttribute(NatureAttribute::AntiDerivativeNature, _) => keywords::idt_nature,
            Self::NatureAttribute(NatureAttribute::Units, _) => keywords::units,
            Self::NatureAttribute(NatureAttribute::User(ident), _) => ident.name,
        }
    }
}

impl Display for AttributeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DisciplineDomain(discipline) => {
                f.write_fmt(format_args!("The potential nature of {}", discipline))
            }

            Self::DisciplinePotential(discipline) => {
                f.write_fmt(format_args!("The flow nature of {}", discipline))
            }

            Self::DisciplineFlow(discipline) => {
                f.write_fmt(format_args!("The domain of {}", discipline))
            }

            Self::NatureAttribute(NatureAttribute::Abstol, nature) => {
                f.write_fmt(format_args!("The 'abstol' attribute of {}", nature))
            }

            Self::NatureAttribute(NatureAttribute::Access, nature) => {
                f.write_fmt(format_args!("The 'access' attribute of {}", nature))
            }
            Self::NatureAttribute(NatureAttribute::DerivativeNature, nature) => {
                f.write_fmt(format_args!("The 'ddt_nature' attribute of {}", nature))
            }
            Self::NatureAttribute(NatureAttribute::AntiDerivativeNature, nature) => {
                f.write_fmt(format_args!("The 'idt_nature' attribute of {}", nature))
            }
            Self::NatureAttribute(NatureAttribute::Units, nature) => {
                f.write_fmt(format_args!("The 'units' attribute of {}", nature))
            }
            Self::NatureAttribute(NatureAttribute::User(ident), nature) => {
                f.write_fmt(format_args!("The user attribute '{}' of {}", ident, nature))
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
    #[error("Overwriting {kind} in derived nature '{nature}' is forbidden! ")]
    IllegalNatureAttributeOverwrite {
        kind: IllegalNatureAttributeOverwriteKind,
        nature: Ident,
        parent: Ident,
        overwrite: Span,
    },

    #[error("The nature {name} is derived from {parent} which is itself derived from {name}")]
    CircularAttributeInheritance {
        name: Ident,
        parent: Ident,
        trace_back: Vec<Ident>,
    },

    #[error("Disciplines in the discrete Domain may not define flow/potential natures")]
    DiscreteDisciplineHasNatures {
        span: Span,
        discrete_declaration: Span,
        first_nature: Span,
        second_nature: Option<Span>,
    },

    #[error("{attribute_kind} was defined multiple times")]
    AttributeAlreadyDefined {
        attribute_kind: AttributeKind,
        old: Span,
        new: Span,
    },

    #[error("Module ports declared in Module body when already declared in Module Head")]
    PortRedeclaration {
        module_head: Span,
        body_declaration: Span,
    },

    #[error("{port} has to be listed in the Module head!")]
    PortNotPreDeclaredInModuleHead { port_list: Span, port: Ident },

    #[error("Port {0} was pre declared in module head but not defined in the module body")]
    PortPreDeclaredNotDefined(Ident),

    #[error("Nature {1} is missing the required attributes {0}")]
    RequiredBaseNatureAttributesNotDefined(ListFormatter<Vec<Symbol>>, Ident, Span),

    #[error("Case statements may only have one default case")]
    MultipleDefaultDeclarations { old: Span, new: Span },

    #[error("Function argument {0} is missing separate type declaration")]
    FunctionArgTypeDeclarationMissing(Ident),

    #[error("'{0}' was not found in the current scope!")]
    NotFound(Ident),

    #[error("'{0}' was not found in '{1}'!")]
    NotFoundIn(Ident, Symbol),

    #[error("'{0}' was declared without a discipline! This is not valid in the analog subset of VerilogAMS")]
    NetWithoutDiscipline(Ident),

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

    #[error("{0} only accepts identifiers!")]
    ExpectedIdentifier(&'static str, Span),

    #[error("This branch can not be accessed by {nature}. {allowed_natures}")]
    NatureNotPotentialOrFlow {
        nature: Ident,
        allowed_natures: AllowedNatures,
    },

    #[error("The disciplines of {0} and {1} are incompatible")]
    DisciplineMismatch(NetInfo, NetInfo, Span),

    #[error("{0} are not allowed in this expressions!")]
    NotAllowedInConstantContext(NonConstantExpression, Span),

    #[error("{0} is now allowed inside analog functions!")]
    NotAllowedInFunction(NotAllowedInFunction, Span),

    #[error("Partial derivatives may only be calculated over node potentials or branch flows")]
    DerivativeNotAllowed(Span),

    #[error("Contribution can only be made to branch probes (for example 'I(net1,net2)', 'potential(net)' or 'flow(branch)')")]
    ContributeToNonBranchProbe(Span),

    #[error("Port branches ( like (<PORT>) ) can only be read but not contributed to")]
    ContributeToBranchPortProbe(Span),

    #[error("$limit function calls must have at least 2 arguments found 0")]
    EmptyLimit(Span),

    #[error("OpenVAF does not support $limit calls without an explicitly specifier function")]
    LimRequiresFunction(Span),

    #[error("Limit functions me be a function Identifier or a string literal")]
    IllegalLimitFn(Span),

    #[error("Port flow (like I(< PORT >) ) can not be used in a $lim function")]
    PortFlowCanNotBeANonLinearity(Span),
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
                label: Text::const_str("Every base nature (a nature that has not parent) has to define the 'abstol', 'access' and 'units' attribute"),
                annotation_type: AnnotationType::Help,
            }],

            Self::LimRequiresFunction(_) => vec![
                FooterItem {
                    id: None,
                    label: Text::const_str("A limitation strategy is required because OpenVAF compiles static binaries that can not choose an appropriate strategy at runtime"),
                    annotation_type: AnnotationType::Help,
                },
                FooterItem {
                    id: None,
                    label: Text::const_str("o call a simulator specific function use text. Example\n\t$limit(V(b,e), \"pnjlimit\", vce, $vt) )"),
                    annotation_type: AnnotationType::Help,
                },
                FooterItem {
                    id: None,
                    label: Text::const_str("To call a simulator limitation function provide the name of the function as string. For example\n\t$limit(V(b,e), \"pnjlimit\", vce, $vt) )"),
                    annotation_type: AnnotationType::Help,
                },
                FooterItem {
                    id: None,
                    label: Text::const_str("To use your own limiting strategy create a VerilogA function and provide the name of the function as string. For example\n\t$limit(V(d,f), my_lim, arg1)"),
                    annotation_type: AnnotationType::Help,
                },
            ],
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

            Self::AttributeAlreadyDefined {
                attribute_kind,
                old,
                new,
            } => vec![DiagnosticSlice {
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

            Self::PortRedeclaration {
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

            Self::PortNotPreDeclaredInModuleHead { port_list, port } => vec![
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

            Self::PortPreDeclaredNotDefined(port) => vec![DiagnosticSlice {
                slice_span: port.span.data(),
                messages: vec![(
                    AnnotationType::Info,
                    Text::const_str("Port is not defined in module body"),
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

            Self::ExpectedIdentifier(_, span) => vec![DiagnosticSlice {
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
                        ListFormatter::new(allowed_natures.to_allowed_list())
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

            Self::DerivativeNotAllowed(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Expected node potential or branch flow"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::NetWithoutDiscipline(ident) => vec![DiagnosticSlice {
                slice_span: ident.span.data(),
                messages: vec![(
                    AnnotationType::Help,
                    Text::const_str("Add discipline (such as 'electrical'"),
                    ident.span.data(),
                )],
                fold: false,
            }],

            Self::ContributeToNonBranchProbe(span) | Self::ContributeToBranchPortProbe(span) => {
                vec![DiagnosticSlice {
                    slice_span: span.data(),
                    messages: vec![(
                        AnnotationType::Error,
                        Text::const_str("Can only contribute to branches"),
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
                        Text::const_str("Default case declared here"),
                        new.data(),
                    ),
                    (
                        AnnotationType::Error,
                        Text::const_str("Second default case is forbidden"),
                        old.data(),
                    ),
                ],
                fold: false,
            }],

            Self::CircularAttributeInheritance { name, parent, .. } => vec![DiagnosticSlice {
                slice_span: name.span.data().extend(parent.span.data()),
                messages: vec![(
                    AnnotationType::Error,
                    Text::borrowed("Circular derived nature"),
                    parent.span.data(),
                )],
                fold: false,
            }],

            Self::IllegalNatureAttributeOverwrite {
                kind,
                nature,
                overwrite,
                parent,
            } => vec![DiagnosticSlice {
                slice_span: nature.span.data().extend(parent.span.data()),
                messages: vec![
                    (
                        AnnotationType::Error,
                        Text::owned(format!(
                            "Overwriting {} in derived natures is forbidden",
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
            }],
            Self::LimRequiresFunction(span) | Self::EmptyLimit(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected at least 2 arguments")),
                    span.data(),
                )],
                fold: false,
            }],

            Self::IllegalLimitFn(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("Expected function identifier or string literal")),
                    span.data(),
                )],
                fold: false,
            }],

            Self::PortFlowCanNotBeANonLinearity(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::owned(format!("expected a branch access such as V(b,e)")),
                    span.data(),
                )],
                fold: false,
            }],
        }
    }
}
