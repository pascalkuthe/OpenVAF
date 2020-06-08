/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use std::fmt::Display;

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use core::fmt::Formatter;

use crate::ir::DisciplineId;
use crate::parser::error::{ Unsupported};
use crate::symbol::Symbol;
use crate::symbol_table::SymbolDeclaration;
use crate::util::format_list;
use crate::{parser, Ast, SourceMap, Span};
use beef::lean::Cow;
use core::fmt::Debug;

pub type Error = crate::error::Error<Type>;
//pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<T = ()> = std::result::Result<T, Error>;
#[derive(Clone, Debug)]
pub struct NetInfo {
    pub discipline: DisciplineId,
    pub name: Symbol,
    pub declaration: Span,
}
#[derive(Clone, Debug)]
pub enum Type {
    TypeDeclarationMissing(Symbol),
    NotFound(Symbol),
    NotAScope {
        declaration: Span,
        name: Symbol,
    },
    DeclarationTypeMismatch {
        expected: Vec<MockSymbolDeclaration>,
        found: SymbolDeclaration,
    },
    UnexpectedTokenInBranchAccess,
    EmptyBranchAccess,
    NatureNotPotentialOrFlow(Symbol, DisciplineId),
    DisciplineMismatch(NetInfo, NetInfo),
    NotAllowedInConstantContext(NonConstantExpression),
    NotAllowedInFunction(NotAllowedInFunction),
    Unsupported(Unsupported),
    DerivativeNotAllowed,
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
            Self::NonLocalAccess => f.write_str("Named blocks"),
            Self::Contribute => f.write_str("contribute statements (<+)"),
        }
    }
}

impl Error {
    pub fn print(self, source_map: &SourceMap, ast: &Ast, translate_lines: bool) {
        let (line, line_number, substitution_name, range) =
            source_map.resolve_span_within_line(self.source, translate_lines);
        let (origin, mut footer) = if let Some(substitution_name) = substitution_name {
            (Cow::owned(substitution_name), vec![Annotation {
                id: None,
                label: Some("If macros/files are included inside this macro/file the error output might be hard to understand/display incorrect line numbers (See fully expanded source)"),
                annotation_type: AnnotationType::Note
            }])
        } else {
            (Cow::const_str(source_map.main_file_name), Vec::new())
        };
        let opt = FormatOptions {
            color: true,
            anonymized_line_numbers: false,
        };

        match self.error_type {
            Type::UnexpectedTokenInBranchAccess => {
                let range = (range.start as usize,range.end as usize);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Unexpected Token"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Expected branch reference",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::NotFound(sym) => {
                let range = (range.start as usize,range.end as usize);
                let label = format!("cannot find {} in this scope", sym.as_str());
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(label.as_str()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "not found in this Scope",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::DeclarationTypeMismatch {
                ref expected,
                found,
            } => {
                let range = (range.start as usize,range.end as usize);
                let label = format!(
                    "Expected {} found {} {}",
                    format_list(expected, ""),
                    found.mock(),
                    found.name(&ast).as_str()
                );
                let inline_label = format!("Expected {:?}", expected);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&label),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: &inline_label,
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::NatureNotPotentialOrFlow(name, discipline) => {
                let (msg,expected) = match (ast[discipline].contents.potential_nature,ast[discipline].contents.flow_nature){
                    (None,None)=> (Cow::const_str("Discrete Disciplines can not be accessed using branch Probes"),Cow::const_str("Illegal branch access")),

                    (Some(nature),None) => (Cow::owned(format!(
                        "This branch can only be accessed using its Potential ({})",
                        nature.name,
                    )), Cow::owned(format!("Expected {}",nature.name))
                    ),
                    (None,Some(nature)) => (Cow::owned(format!(
                        "This branch can only be accessed using its Flow ({})",
                        nature.name,
                    )),Cow::owned(format!("Expected {}",nature.name))
                    ),
                    (Some(pot),Some(flow)) => (Cow::owned(format!(
                        "This branch can only be accessed using its Potential ({}) or its Flow ({})",
                        pot.name,
                        flow.name,
                    )),Cow::owned(format!("Expected {} or {}",pot.name,flow.name))
                    ),
                };
                footer.push(Annotation {
                    id: None,
                    label: Some(&*msg),
                    annotation_type: AnnotationType::Info,
                });
                let range = (range.start as usize,range.end as usize);
                let label = format!(
                    "{} can not be accessed by {}",
                    ast[discipline].contents.name,
                    &name.as_str(),
                );
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&*label),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: &*expected,
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::NotAScope { declaration, name } => {
                let (
                    other_declaration_line,
                    other_declaration_line_number,
                    other_declaration_origin,
                    other_declaration_range,
                ) = source_map.resolve_span_within_line(declaration, translate_lines);

                let range = (range.start as usize,range.end as usize);
                let other_declaration_range = (
                    other_declaration_range.start as usize,
                    other_declaration_range.end as usize,
                );

                let other_declaration_origin = if let Some(val) = other_declaration_origin {
                    Cow::owned(val)
                } else {
                    Cow::borrowed(source_map.main_file_name)
                };
                let label = format!("Expected scope found reference to {}", name.as_str());
                let inline_label = format!("{} is declared here", name);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&label),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin: Some(&*origin),
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Expected scope",
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: other_declaration_line,
                            line_start: other_declaration_line_number as usize,
                            origin: Some(&*other_declaration_origin),
                            annotations: vec![SourceAnnotation {
                                range: other_declaration_range,
                                label: &inline_label,
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                    ],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::DisciplineMismatch(net1, net2) => {
                let (
                    declaration_line,
                    declaration_line_number,
                    declaration_origin,
                    declaration_range,
                ) = source_map.resolve_span_within_line(net1.declaration, translate_lines);
                let (
                    other_declaration_line,
                    other_declaration_line_number,
                    other_declaration_origin,
                    other_declaration_range,
                ) = source_map.resolve_span_within_line(net2.declaration, translate_lines);

                let range = (range.start as usize,range.end as usize);
                let declaration_range = (
                    declaration_range.start as usize,
                    declaration_range.end as usize,
                );
                let other_declaration_range = (
                    other_declaration_range.start as usize,
                    other_declaration_range.end as usize,
                );

                let other_declaration_origin = if let Some(val) = other_declaration_origin {
                    Cow::owned(val)
                } else {
                    Cow::borrowed(source_map.main_file_name)
                };

                let declaration_origin = if let Some(val) = declaration_origin {
                    Cow::owned(val)
                } else {
                    Cow::borrowed(source_map.main_file_name)
                };
                let label = format!(
                    "Disciplines {} and {} of nets {} and {} are incompatible",
                    ast[net1.discipline].contents.name.name,
                    ast[net2.discipline].contents.name.name,
                    net1.name,
                    net2.name
                );

                let inline_label = format!("{} is declared here", net1.name);
                let inline_label_2 = format!("{} is declared here", net2.name);

                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&label),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin: Some(&*origin),
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Incompatible disciplines",
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: declaration_line,
                            line_start: declaration_line_number as usize,
                            origin: Some(&*declaration_origin),
                            annotations: vec![SourceAnnotation {
                                range: declaration_range,
                                label: &inline_label,
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: other_declaration_line,
                            line_start: other_declaration_line_number as usize,
                            origin: Some(&*other_declaration_origin),
                            annotations: vec![SourceAnnotation {
                                range: other_declaration_range,
                                label: &inline_label_2,
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                    ],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::NotAllowedInConstantContext(non_constant_expr) => {
                let range = (range.start as usize,range.end as usize);
                let label = format!(
                    "{} are not allowed in a constant context!",
                    non_constant_expr
                );
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(label.as_str()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Not allowed in a constant expression!",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::Unsupported(unsupported) => {
                parser::error::Error {
                    error_type: parser::error::Type::Unsupported(unsupported),
                    source: self.source,
                }
                .print(source_map, translate_lines);
            }
            Type::EmptyBranchAccess => {
                let range = (range.start as usize,range.end as usize);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("This refers to a nature but the brackets afterwards are empty (expected branch probe)"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin:Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Expected a following branch probe",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::DerivativeNotAllowed => {
                let range = (range.start as usize,range.end as usize);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Partial derivatives may only be calculated over node potentials (for example V(node))"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin:Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Illegal derivative",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::TypeDeclarationMissing(name) => {
                let range = (range.start as usize,range.end as usize);
                let label = format!(
                    "Function argument {} is missing separate type declaration",
                    name
                );
                let hint_label = format!(
                    "Add the following to the function declaration: real {}; (or integer {};)",
                    name, name
                );
                footer.push(Annotation {
                    id: None,
                    label: Some(&hint_label),
                    annotation_type: AnnotationType::Help,
                });
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&label),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Type declaration missing",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::NotAllowedInFunction(not_allowed) => {
                let range = (range.start as usize,range.end as usize);
                let label = format!("{} are not allowed inside functions!", not_allowed);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(label.as_str()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Not allowed in a function",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
        };
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
        Debug::fmt(self, f)
    }
}
