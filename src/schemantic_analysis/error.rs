use annotate_snippets::display_list::DisplayList;
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use log::error;

use crate::ir::{ParameterId, VariableId};
use crate::parser::error::translate_to_inner_snippet_range;
use crate::{Hir, SourceMap};

pub type Error<'hir> = crate::error::Error<Type<'hir>>;
//pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<'hir, T = ()> = std::result::Result<T, Error<'hir>>;

#[derive(Clone, Debug)]
pub enum Type<'hir> {
    ExpectedReal,
    ExpectedInteger,
    ExpectedIntegerParameter(ParameterId<'hir>),
    ExpectedIntegerVariable(VariableId<'hir>),
    ExpectedNumericParameter(ParameterId<'hir>),
    ParameterDefinedAfterConstantReference(ParameterId<'hir>),
    InvalidParameterBound,
    ParameterExcludeNotPartOfRange,
}
impl<'tag> Error<'tag> {
    pub fn print(&self, source_map: &SourceMap, hir: &Hir<'tag>, translate_lines: bool) {
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
            Type::ExpectedReal => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Expected real valued expression".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Expected real".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::ExpectedInteger => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Expected integer valued expression".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Expected integer".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::ExpectedIntegerVariable(variable) => {
                let (
                    parameter_line,
                    parameter_line_number,
                    parameter_substitution_name,
                    parameter_range,
                ) = source_map.resolve_span_within_line(hir[variable].source, translate_lines);
                let parameter_origin = if let Some(substitution_name) = parameter_substitution_name
                {
                    Some(substitution_name)
                } else {
                    Some(source_map.main_file_name().to_string())
                };
                let parameter_range = translate_to_inner_snippet_range(
                    parameter_range.start,
                    parameter_range.end,
                    parameter_line,
                );
                let parameter_line = parameter_line.to_string();
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Expected an integer valued variable".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin,
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Expected integer".to_string(),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: parameter_line,
                            line_start: parameter_line_number as usize,
                            origin: parameter_origin,
                            annotations: vec![SourceAnnotation {
                                range: parameter_range,
                                label: format!("{} is declared here", hir[variable].contents.name),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                    ],
                }
            }

            Type::ExpectedIntegerParameter(parameter) => {
                let (
                    parameter_line,
                    parameter_line_number,
                    parameter_substitution_name,
                    parameter_range,
                ) = source_map.resolve_span_within_line(hir[parameter].source, translate_lines);
                let parameter_origin = if let Some(substitution_name) = parameter_substitution_name
                {
                    Some(substitution_name)
                } else {
                    Some(source_map.main_file_name().to_string())
                };
                let parameter_range = translate_to_inner_snippet_range(
                    parameter_range.start,
                    parameter_range.end,
                    parameter_line,
                );
                let parameter_line = parameter_line.to_string();
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some("Expected an integer valued parameter".to_string()),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin,
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Expected integer".to_string(),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: parameter_line,
                            line_start: parameter_line_number as usize,
                            origin: parameter_origin,
                            annotations: vec![SourceAnnotation {
                                range: parameter_range,
                                label: format!("{} is declared here", hir[parameter].contents.name),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                    ],
                }
            }

            Type::ExpectedNumericParameter(parameter) => {
                let (
                    parameter_line,
                    parameter_line_number,
                    parameter_substitution_name,
                    parameter_range,
                ) = source_map.resolve_span_within_line(hir[parameter].source, translate_lines);
                let parameter_origin = if let Some(substitution_name) = parameter_substitution_name
                {
                    Some(substitution_name)
                } else {
                    Some(source_map.main_file_name().to_string())
                };
                let parameter_range = translate_to_inner_snippet_range(
                    parameter_range.start,
                    parameter_range.end,
                    parameter_line,
                );
                let parameter_line = parameter_line.to_string();
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "Expected numeric parameter but {} is a String",
                            hir[parameter].contents.name
                        )),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![
                        Slice {
                            source: line,
                            line_start: line_number as usize,
                            origin,
                            annotations: vec![SourceAnnotation {
                                range,
                                label: "Expected numeric value".to_string(),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: parameter_line,
                            line_start: parameter_line_number as usize,
                            origin: parameter_origin,
                            annotations: vec![SourceAnnotation {
                                range: parameter_range,
                                label: "Parameter declared here".to_string(),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                    ],
                }
            }

            Type::ParameterDefinedAfterConstantReference(parameter) => {
                let (
                    parameter_line,
                    parameter_line_number,
                    parameter_substitution_name,
                    parameter_range,
                ) = source_map.resolve_span_within_line(hir[parameter].source, translate_lines);
                let parameter_origin = if let Some(substitution_name) = parameter_substitution_name
                {
                    Some(substitution_name)
                } else {
                    Some(source_map.main_file_name().to_string())
                };
                let parameter_range = translate_to_inner_snippet_range(
                    parameter_range.start,
                    parameter_range.end,
                    parameter_line,
                );
                let parameter_line = parameter_line.to_string();
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!("Parameter {} was referenced before it was defined in a constant context",hir[parameter].contents.name)),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "Constant reference here".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    },Slice {
                        source: parameter_line,
                        line_start: parameter_line_number as usize,
                        origin: parameter_origin,
                        annotations: vec![SourceAnnotation {
                            range:parameter_range,
                            label: "Parameter declared here".to_string(),
                            annotation_type: AnnotationType::Info,
                        }],
                        fold: false,
                    }
                    ],
                }
            }
            Type::InvalidParameterBound => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(
                            "Invalid parameter range; Lower bound must be smaller than upper bound"
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
                            label: "Lower bound must but smaller than upper bound".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::ParameterExcludeNotPartOfRange => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(
                            "Invalid parameter bound. Can not exclude a value that is not part of the bound to begin with"
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
                            label: "This calue can't be excluded".to_string(),
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
