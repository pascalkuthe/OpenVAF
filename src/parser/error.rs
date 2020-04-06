//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::fmt::Display;

use annotate_snippets::display_list::DisplayList;
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use intrusive_collections::__core::fmt::Formatter;
use log::{error, warn};

use crate::parser::lexer::Token;
use crate::parser::preprocessor::ArgumentIndex;
use crate::span::Index;
use crate::symbol::{Ident, Symbol};
use crate::{SourceMap, Span};

pub type Error = crate::error::Error<Type>;
pub type Warning = crate::error::Error<WarningType>;
pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Type {
    //Parser
    PortRedeclaration(Span, Span),
    HierarchicalIdNotAllowedAsNature {
        hierarchical_id: Vec<Ident>,
    },
    PortNotPreDeclaredInModuleHead {
        port_list: Span,
    },
    PortPreDeclaredNotDefined,
    EmptyListEntry(List),
    AttributeAlreadyDefined,
    RequiredAttributeNotDefined(Vec<Symbol>),
    DiscreteDisciplineHasNatures,
    StringTooLong(usize),

    //Preprocessor
    MacroArgumentCount {
        expected: ArgumentIndex,
        found: ArgumentIndex,
    },
    ConditionEndWithoutStart,
    MacroEndTooEarly,
    UnclosedConditions(Vec<Span>),
    MacroNotFound,
    MacroRecursion,
    CompilerDirectiveSplit,
    IoErr(String),
    AlreadyDeclaredInThisScope {
        other_declaration: Span,
        name: Symbol,
    },

    //General
    UnexpectedEof {
        expected: Vec<Token>,
    },
    UnexpectedToken {
        expected: Vec<Token>,
    },
    UnexpectedTokens {
        expected: Vec<Expected>,
    },
    Unsupported(Unsupported),
}
impl From<std::io::Error> for Type {
    fn from(io_err: std::io::Error) -> Self {
        Self::IoErr(io_err.to_string())
    }
}
#[derive(Clone, Copy, Debug)]
pub enum Unsupported {
    StringParameters,
    DefaultDiscipline,
    MacroDefinedInMacro,
    NatureInheritance,
}
impl Display for Unsupported {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::StringParameters => f.write_str("String parameters"),
            Self::DefaultDiscipline => f.write_str("Implicit Disciplines"),
            Self::MacroDefinedInMacro => f.write_str("Macros defined inside another Macros"),
            Self::NatureInheritance => f.write_str("Derived Natures"),
        }
    }
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
    FunctionCall,
    BranchAcess,
    Assign,
    ParameterRange,
}
#[derive(Clone, Debug)]
pub enum WarningType {
    MacroOverwritten(Span),
    AttributeOverwrite(Ident, Span),
}

#[derive(Debug, Clone, Copy)]
pub enum List {
    MacroArgument,
    FunctionArgument,
}
impl Error {
    pub fn print(self, source_map: &SourceMap, translate_lines: bool) {
        let (line, line_number, substitution_name, range) =
            source_map.resolve_span_within_line(self.source, translate_lines);
        let (origin, mut footer) = if let Some(substitution_name) = substitution_name {
            (substitution_name,vec![Annotation{
                id: None,
                label: Some("If macros/files are included inside this macro/file the error output might be hard to understand/display incorrect line numbers (See fully expanded source)".to_string()),
                annotation_type: AnnotationType::Note
            }])
        } else {
            (source_map.main_file_name().to_string(), Vec::new())
        };
        let line = line.to_string(); //necessary because annotate snippet cant work with slices yet
        let origin = Some(origin);
        let snippet = match self.error_type {
            Type::UnexpectedToken { ref expected } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Unexpected Token".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: format!("expected {:?}", expected),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::UnexpectedTokens { ref expected } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Unexpected Token".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: format!("expected {:?}", expected),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::PortRedeclaration(error_span, declaration_list_span) => {
                let error_range = translate_to_inner_snippet_range(
                    range.start as Index + error_span.get_start(),
                    range.start as Index + error_span.get_end(),
                    &line,
                );
                let declaration_list_range = translate_to_inner_snippet_range(
                    declaration_list_span.get_start() + range.start as Index,
                    range.start as Index + declaration_list_span.get_end(),
                    &line,
                );
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(
                            "Module ports declared in Module body when already declared in Module Head"
                                .to_string(),
                        ),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![
                            SourceAnnotation {
                                range:declaration_list_range,
                                label: "Ports are declared here".to_string(),
                                annotation_type: AnnotationType::Info,
                            },
                            SourceAnnotation {
                                range: error_range,
                                label: "Port declaration is illegal here".to_string(),
                                annotation_type: AnnotationType::Error,
                            },
                        ],
                        fold: true,
                    }],
                }
            }

            Type::AlreadyDeclaredInThisScope {
                name,
                other_declaration,
            } => {
                let (
                    other_declaration_line,
                    other_declaration_line_number,
                    other_declaration_origin,
                    other_declaration_range,
                ) = source_map.resolve_span_within_line(other_declaration, translate_lines);

                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let other_declaration_range = translate_to_inner_snippet_range(
                    other_declaration_range.start,
                    other_declaration_range.end,
                    &other_declaration_line,
                );

                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!("'{}' has already been declared", name.as_str())),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: other_declaration_line.to_string(),
                            line_start: other_declaration_line_number as usize,
                            origin: other_declaration_origin,
                            annotations: vec![SourceAnnotation {
                                range: other_declaration_range,
                                label: format!("First declaration of '{}' here", name.as_str()),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin,
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Item already declared".to_string(),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                    ],
                }
            }

            Type::PortPreDeclaredNotDefined => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(
                            "Port was pre declared in module head but not defined in module body"
                                .to_string(),
                        ),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Declared here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }

            Type::UnclosedConditions(ref conditions) => {
                let slices = conditions
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(index, condition)| {
                        let (src, line_number, origin, range) =
                            source_map.resolve_span_within_line(condition, translate_lines);
                        let range = translate_to_inner_snippet_range(range.start, range.end, src);
                        Slice {
                            source: src.to_string(),
                            line_start: line_number as usize,
                            origin: origin
                                .or_else(|| Some(source_map.main_file_name().to_string())),
                            annotations: vec![SourceAnnotation {
                                range,
                                label: format!("{}. condition started here", index),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        }
                    })
                    .collect();
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!("{} macro conditions where not closed. Conditions should be closed using `endif",{conditions.len()})),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices,
                }
            }

            Type::MacroNotFound => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro referenced that was not defined before".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Reference occurs here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::HierarchicalIdNotAllowedAsNature { hierarchical_id } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Natures can not be hierarchical ".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: ". in Nature identifier is illegal".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::PortNotPreDeclaredInModuleHead { port_list } => {
                let (
                    other_declaration_line,
                    other_declaration_line_number,
                    other_declaration_origin,
                    other_declaration_range,
                ) = source_map.resolve_span_within_line(port_list, translate_lines);

                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let other_declaration_range = translate_to_inner_snippet_range(
                    other_declaration_range.start,
                    other_declaration_range.end,
                    &other_declaration_line,
                );

                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Ports have to be listed in the Module head fi they are declared in the body".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: other_declaration_line.to_string(),
                            line_start: other_declaration_line_number as usize,
                            origin: other_declaration_origin,
                            annotations: vec![SourceAnnotation {
                                range: other_declaration_range,
                                label: "Port should have been listed here".to_string(),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin,
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Port declaration illegal without listing in module head".to_string(),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                    ],
                }
            }
            Type::EmptyListEntry(_list_type) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Empty macro arguments are not allowed".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Expected argument here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::MacroArgumentCount { expected, found } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "Found wrong number of Macro arguments. Expected {} found {} ",
                            expected, found
                        )),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Unexpected `endif".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::ConditionEndWithoutStart => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(
                            "Unexpected `endif: No condition is currently in Scope ".to_string(),
                        ),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Unexpected `endif".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::MacroEndTooEarly => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro ended too early! One or more compiler directives are still unfinished (macro ends on new line not preceded by \\)".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Recursion detected here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::MacroRecursion => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro recursion detected! Macros may not call themselves (directly or indirectly)".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Recursion detected here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::CompilerDirectiveSplit => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Compiler directives (such as `define x) can't be split across borders of macros or files".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Split detected here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::UnexpectedEof { expected } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!("Unexpected EOF expected {:?}", expected)),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Split detected here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::Unsupported(unsupported) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!("{} are currently not supported", unsupported)),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Unsupported feature was used here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::IoErr(error) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(error),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Io Error occured here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::AttributeAlreadyDefined => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("An attribute was redefined".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Redefined here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }

            Type::RequiredAttributeNotDefined(missing) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "Nature is missing the required attributes  {:?}",
                            missing
                        )),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: format!("Required attributes {:?} are missing", missing),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }

            Type::DiscreteDisciplineHasNatures => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Disciplines in the discrete Domain may not define flow/potential natures".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Natures may not be defined".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::StringTooLong(len) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "String literals longer than {} characters are not supported",
                            std::u16::MAX
                        )),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: format!("String literal with {} characters", len),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
        };
        let display_list = DisplayList::from(snippet);
        let formatter = DisplayListFormatter::new(true, false);
        error!("{}", formatter.format(&display_list));
    }
}
impl Warning {
    pub fn print(&self, source_map: &SourceMap, translate_lines: bool) {
        let (line, line_number, substitution_name, range) =
            source_map.resolve_span_within_line(self.source, translate_lines);
        let (origin, mut footer) = if let Some(substitution_name) = substitution_name {
            (substitution_name,vec![Annotation{
                id: None,
                label: Some("This error occurred inside an expansion of a macro or a file. If additional macros or files are included inside this expansion the line information will be inaccurate and the error output might be hard to track if macros are used extensivly. The fully expanded source could give better insight in that case".to_string()),
                annotation_type: AnnotationType::Note
            }])
        } else {
            (source_map.main_file_name().to_string(), Vec::new())
        };

        let line = line.to_string();
        let snippet = match self.error_type {
            WarningType::MacroOverwritten(first_declaration) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let (original_line, original_line_number, substitution_name, original_range) =
                    source_map.resolve_span_within_line(first_declaration, translate_lines);
                let original_range = translate_to_inner_snippet_range(
                    original_range.start,
                    original_range.end,
                    &original_line,
                );
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro overwritten".to_string()),
                        annotation_type: AnnotationType::Warning,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: original_line.to_string(),
                            line_start: original_line_number as usize,
                            origin: Some(origin.clone()),
                            annotations: vec![SourceAnnotation {
                                range: original_range,
                                label: "First_declared here".to_string(),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin: Some(origin),
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Later overwritten here".to_string(),
                                annotation_type: AnnotationType::Warning,
                            }],
                            fold: false,
                        },
                    ],
                }
            }
            WarningType::AttributeOverwrite(overwritten_ident, overwrite_span) => {
                let overwritten_range = translate_to_inner_snippet_range(
                    range.start,
                    range.start + overwritten_ident.span.get_end() - self.source.get_start(),
                    &line,
                );
                let overwrite_span = overwrite_span.negative_offset(self.source.get_start());
                let overwrite_range = translate_to_inner_snippet_range(
                    range.start + overwrite_span.get_start(),
                    range.start + overwrite_span.get_end(),
                    &line,
                );
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "Attribute {} was declared multiple times. First value is ignored",
                            overwritten_ident.name.as_str()
                        )),
                        annotation_type: AnnotationType::Warning,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(origin),
                        annotations: vec![
                            SourceAnnotation {
                                range: overwrite_range,
                                label: "overwritten here".to_string(),
                                annotation_type: AnnotationType::Warning,
                            },
                            SourceAnnotation {
                                range: overwritten_range,
                                label: "First declared here".to_string(),
                                annotation_type: AnnotationType::Info,
                            },
                        ],
                        fold: false,
                    }],
                }
            }
        };
        let display_list = DisplayList::from(snippet);
        let formatter = DisplayListFormatter::new(true, false);
        warn!("{}", formatter.format(&display_list));
    }
}

pub(crate) fn translate_to_inner_snippet_range(
    start: Index,
    end: Index,
    source: &str,
) -> (usize, usize) {
    let lines = bytecount::count(&source.as_bytes()[..start as usize], b'\n');
    (start as usize + lines, end as usize + lines)
}
