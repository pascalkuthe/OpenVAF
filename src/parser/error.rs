//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::fmt::{Display, Formatter};

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

use crate::parser::lexer::Token;
use crate::parser::preprocessor::ArgumentIndex;
use crate::span::Index;
use crate::symbol::{Ident, Symbol};
use crate::util::format_list;
use crate::{SourceMap, Span};
use beef::lean::Cow;

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
    MissingOrUnexpectedToken {
        expected: Vec<Token>,
        expected_at: Index,
    },
    UnexpectedTokens {
        expected: Vec<Expected>,
    },
    TooManyBranchArgs(usize),
    Unsupported(Unsupported),
    FunctionWithoutBody(Ident),
    Unrecoverable,
}

impl From<std::io::Error> for Type {
    fn from(io_err: std::io::Error) -> Self {
        Self::IoErr(io_err.to_string())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Unsupported {
    StringParameters,
    DefaultDiscipline, // TODO remove this. Only part of VerilogAMS not VerilogA
    MacroDefinedInMacro,
    NatureInheritance,
    ConstantFunctionCalls,
    SelfDerivingAssignments,
}

impl Display for Unsupported {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::StringParameters => f.write_str("String parameters"),
            Self::DefaultDiscipline => f.write_str("Implicit Disciplines"),
            Self::MacroDefinedInMacro => f.write_str("Macros defined inside another Macros"),
            Self::NatureInheritance => f.write_str("Derived Natures"),
            Self::ConstantFunctionCalls => {
                f.write_str("Function calls inside constant expressions")
            }
            Self::SelfDerivingAssignments => {
                f.write_str("Assignments of the form 'x = f(ddx(x,..),...)'")
            }
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
    Expression,
    BranchAcess,
    ParameterRange,
}

impl Display for Expected {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        // TODO move exampels to webpage
        match self {
            Expected::Identifier => f.write_str("identifier"),
            Expected::PortDeclaration => f.write_str("port declaration"),
            Expected::Port => f.write_str("port listing"),
            Expected::UnaryOperator => f.write_str("unary operator (+,-,!)"),
            Expected::BinaryOperator => f.write_str("binary operator (such as * or +)"),
            Expected::Primary => f.write_str("expression primary"),
            Expected::Statement => f.write_str("statement"),
            Expected::BranchAcess => f.write_str("branch access"),
            Expected::ParameterRange => f.write_str("parameter range (such as from [0:inf]"),
            Expected::Expression => f.write_str("expression"),
        }
    }
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
        let (mut line, mut line_number, substitution_name, mut range) =
            source_map.resolve_span_within_line(self.source, translate_lines);
        let (origin, mut footer) = if let Some(substitution_name) = substitution_name {
            (Cow::owned(substitution_name),vec![Annotation{
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
            Type::UnexpectedToken { ref expected } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!("expected {}", format_list(expected, "'"));

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
                            label: &label,
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }

            Type::UnexpectedTokens { ref expected } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!("expected {}", format_list(expected, ""));

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
                            label: &label,
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
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
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(
                            "Module ports declared in Module body when already declared in Module Head"
                                ,
                        ),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin:Some(&*origin),
                        annotations: vec![
                            SourceAnnotation {
                                range:declaration_list_range,
                                label: "Ports are declared here",
                                annotation_type: AnnotationType::Info,
                            },
                            SourceAnnotation {
                                range: error_range,
                                label: "Port declaration is illegal here",
                                annotation_type: AnnotationType::Error,
                            },
                        ],
                        fold: true,
                    }],opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
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
                let other_declaration_origin = if let Some(other_declaration_origin) =
                    other_declaration_origin
                {
                    footer.push(Annotation{
                        id: None,
                        label: Some("If macros/files are included inside this macro/file the error output might be hard to understand/display incorrect line numbers (See fully expanded source)"),
                        annotation_type: AnnotationType::Note
                    });
                    Cow::owned(other_declaration_origin)
                } else {
                    Cow::const_str(source_map.main_file_name)
                };
                let label = format!("'{}' has already been declared", name.as_str());
                let inline_label = format!("First declaration of '{}' here", name.as_str());

                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&label),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
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
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin: Some(&*origin),
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Item already declared",
                                annotation_type: AnnotationType::Error,
                            }],

                            fold: false,
                        },
                    ],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }

            Type::PortPreDeclaredNotDefined => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(
                            "Port was pre declared in module head but not defined in module body",
                        ),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Declared here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }

            Type::UnclosedConditions(ref conditions) => {
                let slice_text: Vec<_> = conditions
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(index, condition)| {
                        let (src, line_number, origin, range) =
                            source_map.resolve_span_within_line(condition, translate_lines);
                        let range = translate_to_inner_snippet_range(range.start, range.end, src);
                        (
                            src,
                            line_number,
                            origin.map_or(Cow::const_str(source_map.main_file_name), |val| {
                                Cow::owned(val)
                            }),
                            range,
                            format!("{}. condition started here", index),
                        )
                    })
                    .collect();
                let slices = slice_text
                    .iter()
                    .map(|(src, line_number, origin, range, label)| Slice {
                        source: *src,
                        line_start: *line_number as usize,
                        origin: Some(&**origin),
                        annotations: vec![SourceAnnotation {
                            range: *range,
                            label,
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    })
                    .collect();

                let label = format!("{} macro conditions where not closed. Conditions should be closed using `endif",{conditions.len()});
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&label),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices,
                    opt,
                };

                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }

            Type::MacroNotFound => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro referenced that was not defined before"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Reference occurs here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };

                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::HierarchicalIdNotAllowedAsNature { .. } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);

                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Natures can not be hierarchical "),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: ". in Nature identifier is illegal",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };

                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::PortNotPreDeclaredInModuleHead { port_list } => {
                let (
                    other_declaration_line,
                    other_declaration_line_number,
                    other_declaration_origin,
                    other_declaration_range,
                ) = source_map.resolve_span_within_line(port_list, translate_lines);

                let other_declaration_origin = if let Some(other_declaration_origin) =
                    other_declaration_origin
                {
                    footer.push(Annotation{
                        id: None,
                        label: Some("If macros/files are included inside this macro/file the error output might be hard to understand/display incorrect line numbers (See fully expanded source)"),
                        annotation_type: AnnotationType::Note
                    });
                    Cow::owned(other_declaration_origin)
                } else {
                    Cow::const_str(source_map.main_file_name)
                };

                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let other_declaration_range = translate_to_inner_snippet_range(
                    other_declaration_range.start,
                    other_declaration_range.end,
                    &other_declaration_line,
                );

                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Ports have to be listed in the Module head fi they are declared in the body"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: other_declaration_line,
                            line_start: other_declaration_line_number as usize,
                            origin: Some(&*other_declaration_origin),
                            annotations: vec![SourceAnnotation {
                                range: other_declaration_range,
                                label: "Port should have been listed here",
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin:Some(&*origin),
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Port declaration illegal without listing in module head",
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                    ],
                    opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::EmptyListEntry(_list_type) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Empty macro arguments are not allowed"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Expected argument here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::MacroArgumentCount { expected, found } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!(
                    "Found wrong number of Macro arguments. Expected {} found {} ",
                    expected, found
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
                            label: "Unexpected `endif",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::ConditionEndWithoutStart => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Unexpected `endif: No condition is currently in Scope "),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Unexpected `endif",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::MacroEndTooEarly => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro ended too early! One or more compiler directives are still unfinished (macro ends on new line not preceded by \\)"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin:Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Unexpected newline",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::MacroRecursion => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro recursion detected! Macros may not call themselves (directly or indirectly)"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin:Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Recursion detected here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::CompilerDirectiveSplit => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Compiler directives (such as `define x) can't be split across borders of macros or files"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin:Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Split detected here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::UnexpectedEof { ref expected } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!("Unexpected EOF expected {}", format_list(expected, "'"));

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
                            label: "Unexpected EOF",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: true,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::Unsupported(unsupported) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!("{} are currently not supported", unsupported);

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
                            label: "Unsupported feature was used here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::IoErr(error) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&error),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Io Error occured here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::AttributeAlreadyDefined => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("An attribute was redefined"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Redefined here",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }

            Type::RequiredAttributeNotDefined(ref missing) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!(
                    "Nature is missing the required attributes {}",
                    format_list(missing, "'")
                );

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
                            label: "Required attributes are missing",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }

            Type::DiscreteDisciplineHasNatures => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Disciplines in the discrete Domain may not define flow/potential natures"),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin:Some(&*origin),
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Natures may not be defined",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                    opt
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::StringTooLong(len) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!(
                    "String literals longer than {} characters are not supported",
                    std::u16::MAX
                );
                let inline_label = format!("String literal with {} characters", len);

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
            Type::Unrecoverable => (),
            Type::FunctionWithoutBody(name) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!("Function {} was delcared without body", name);
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
                            label: "Function body is missing",
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: true,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::MissingOrUnexpectedToken {
                ref expected,
                expected_at,
            } => {
                let (range, expected_at) = if expected_at < self.source.get_start() {
                    let res = source_map.resolve_span_within_line(
                        Span::new(expected_at, self.source.get_end()),
                        translate_lines,
                    );
                    line = res.0;
                    range = res.3;
                    line_number = res.1;
                    (
                        (
                            (self.source.get_start() - expected_at + range.start) as usize,
                            range.end as usize,
                        ),
                        range.start as usize,
                    )
                } else {
                    (
                        (range.start as usize, range.end as usize),
                        (expected_at - self.source.get_start() + range.start) as usize,
                    )
                };
                let label = format!("expected {}", format_list(expected, "'"));

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
                        annotations: vec![
                            SourceAnnotation {
                                range,
                                label: "Unexpected Token",
                                annotation_type: AnnotationType::Error,
                            },
                            SourceAnnotation {
                                range: (expected_at, expected_at + 1),
                                label: &label,
                                annotation_type: AnnotationType::Info,
                            },
                        ],
                        fold: false,
                    }],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
            }
            Type::TooManyBranchArgs(count) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let label = format!(
                    "Branch access expected at most 2 arguments but {} were specified",
                    count
                );
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
                            label: "Unexpected arguemts",
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
impl Warning {
    pub fn print(&self, source_map: &SourceMap, translate_lines: bool) {
        let (line, line_number, substitution_name, range) =
            source_map.resolve_span_within_line(self.source, translate_lines);
        let (origin, footer) = if let Some(substitution_name) = substitution_name {
            (Cow::owned(substitution_name),vec![Annotation{
                id: None,
                label: Some("This error occurred inside an expansion of a macro or a file. If additional macros or files are included inside this expansion the line information will be inaccurate and the error output might be hard to track if macros are used extensivly. The fully expanded source could give better insight in that case"),
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
            WarningType::MacroOverwritten(first_declaration) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let (original_line, original_line_number, _, original_range) =
                    source_map.resolve_span_within_line(first_declaration, translate_lines);
                let original_range = translate_to_inner_snippet_range(
                    original_range.start,
                    original_range.end,
                    &original_line,
                );
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Macro overwritten"),
                        annotation_type: AnnotationType::Warning,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: original_line,
                            line_start: original_line_number as usize,
                            origin: Some(&*origin),
                            annotations: vec![SourceAnnotation {
                                range: original_range,
                                label: "First_declared here",
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin: Some(&*origin),
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Later overwritten here",
                                annotation_type: AnnotationType::Warning,
                            }],
                            fold: false,
                        },
                    ],
                    opt,
                };
                let display_list = DisplayList::from(snippet);
                eprintln!("{}", display_list);
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
                let label = format!(
                    "Attribute {} was declared multiple times. First value is ignored",
                    overwritten_ident.name.as_str()
                );
                let snippet = Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(&label),
                        annotation_type: AnnotationType::Warning,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin: Some(&*origin),
                        annotations: vec![
                            SourceAnnotation {
                                range: overwrite_range,
                                label: "overwritten here",
                                annotation_type: AnnotationType::Warning,
                            },
                            SourceAnnotation {
                                range: overwritten_range,
                                label: "First declared here",
                                annotation_type: AnnotationType::Info,
                            },
                        ],
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

// FIXME remove this function. Its just here because i cant be bothered to remove the billion uses of it
pub(crate) fn translate_to_inner_snippet_range(
    start: Index,
    end: Index,
    _source: &str,
) -> (usize, usize) {
    (start as usize, end as usize)
}