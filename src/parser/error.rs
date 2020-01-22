//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use annotate_snippets::display_list::DisplayList;
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

use crate::parser::lexer::Token;
use crate::parser::preprocessor::ArgumentIndex;
use crate::span::Index;
use crate::symbol::{Ident, Symbol};
use crate::{SourceMap, Span};

pub type Error = crate::error::Error<Type>;
pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<T = ()> = std::result::Result<T, Error>;
pub type MultiResult<T = ()> = std::result::Result<T, MultiError>;
pub struct MultiError(pub Vec<Error>);

impl MultiError {
    pub fn merge(mut self, mut other: Self) -> Self {
        self.0.append(&mut other.0);
        self
    }
    pub fn add(&mut self, err: Error) {
        self.0.push(err)
    }
}
impl From<Error> for MultiError {
    fn from(err: Error) -> Self {
        MultiError(vec![err])
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    PortRedeclaration(Span, Span),
    EmptyListEntry(List),

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
    HierarchicalIdNotAllowedAsNature {
        hierarchical_id: Vec<Ident>,
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
    DefaultDiscipline,
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
}
#[derive(Clone, Debug)]
pub(crate) enum WarningType {
    MacroOverwritten(Span),
}

#[derive(Debug, Clone, Copy)]
pub enum List {
    MacroArgument,
    FunctionArgument,
}
impl Error {
    pub fn print(&self, source_map: &SourceMap) {
        let (line, mut line_number, substitution_name, range) =
            source_map.resolve_span_within_line(self.source);
        let (origin, mut footer) = if let Some(substitution_name) = substitution_name {
            (substitution_name,vec![Annotation{
                id: None,
                label: Some("This error occurred inside an expansion of a macro or a file. If additional macros or files are included inside this expansion the line information will be inaccurate and the error output might be hard to track if macros are used extensivly. The fully expanded source could give better insight in that case".to_string()),
                annotation_type: AnnotationType::Note
            }])
        } else {
            (source_map.main_file_name().to_string(), Vec::new())
        };
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
                    footer: vec![],
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
                    footer: vec![],
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
                    footer: vec![],
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
                ) = source_map.resolve_span_within_line(other_declaration);

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
                    footer: vec![],
                    slices: vec![
                        Slice {
                            source: other_declaration_line,
                            line_start: other_declaration_line_number as usize,
                            origin: origin.clone(),
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
            _ => unimplemented!("{:?}", self.error_type),
        };
        let display_list = DisplayList::from(snippet);
        let formatter = DisplayListFormatter::new(true, false);
        println!("{}", formatter.format(&display_list));
    }
}
impl Warning {
    pub fn print(&self, source_map: &SourceMap) {
        let (line, line_number, substitution_name, range) =
            source_map.resolve_span_within_line(self.source);
        let origin = if let Some(substitution_name) = substitution_name {
            substitution_name.to_string()
        } else {
            source_map.main_file_name().to_string()
        };
        let range = translate_to_inner_snippet_range(range.start, range.end, &line);
        let snippet = match self.error_type {
            WarningType::MacroOverwritten(first_declaration) => {
                let (original_line, original_line_number, substitution_name, original_range) =
                    source_map.resolve_span_within_line(first_declaration);
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
                    footer: vec![],
                    slices: vec![
                        Slice {
                            source: original_line,
                            line_start: original_line_number as usize,
                            origin: None,
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
                            origin: None,
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
        };
        let display_list = DisplayList::from(snippet);
        let formatter = DisplayListFormatter::new(true, false);
        println!("{}", formatter.format(&display_list));
    }
}

fn translate_to_inner_snippet_range(start: Index, end: Index, source: &str) -> (usize, usize) {
    let lines = bytecount::count(&source.as_bytes()[..start as usize], b'\n');
    (start as usize + lines, end as usize + lines)
}

pub fn merge_multi_result<T>(value_res: MultiResult<T>, condition: MultiResult) -> MultiResult<T> {
    match value_res {
        Ok(res) => {
            condition?;
            Ok(res)
        }
        Err(res) => {
            if let Err(err) = condition {
                Err(res.merge(err))
            } else {
                Err(res)
            }
        }
    }
}

pub fn into_multi_res<T>(res: Result<T>) -> MultiResult<T> {
    res.map_err(|err| err.into())
}

pub fn add_error<T>(res: MultiResult<T>, error: Error) -> MultiResult<T> {
    match res {
        Ok(_) => Err(error.into()),
        Err(mut res) => {
            res.add(error);
            Err(res)
        }
    }
}
