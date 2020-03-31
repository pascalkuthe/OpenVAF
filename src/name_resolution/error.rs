use std::fmt::Display;

use annotate_snippets::display_list::DisplayList;
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use intrusive_collections::__core::fmt::Formatter;
use log::error;

use crate::ir::DisciplineId;
use crate::parser::error::{translate_to_inner_snippet_range, Unsupported};
use crate::span::Index;
use crate::symbol::Symbol;
use crate::symbol_table::SymbolDeclaration;
use crate::{parser, Ast, SourceMap, Span};

pub type Error<'tag> = crate::error::Error<Type<'tag>>;
//pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<'tag, T = ()> = std::result::Result<T, Error<'tag>>;
#[derive(Clone, Debug)]
pub struct NetInfo<'tag> {
    pub discipline: DisciplineId<'tag>,
    pub name: Symbol,
    pub declaration: Span,
}
#[derive(Clone, Debug)]
pub enum Type<'tag> {
    NotFound(Symbol),
    NotAScope {
        declaration: Span,
        name: Symbol,
    },
    DeclarationTypeMismatch {
        expected: Vec<MockSymbolDeclaration>,
        found: SymbolDeclaration<'tag>,
    },
    UnexpectedTokenInBranchAccess,
    NatureNotPotentialOrFlow(Symbol, DisciplineId<'tag>),
    DisciplineMismatch(NetInfo<'tag>, NetInfo<'tag>),
    NotAllowedInConstantContext(NonConstantExpression),
    Unsupported(Unsupported),
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

impl<'tag> Error<'tag> {
    pub fn print(self, source_map: &SourceMap, ast: &Ast<'tag>, translate_lines: bool) {
        let (line, line_number, substitution_name, range) =
            source_map.resolve_span_within_line(self.source, translate_lines);
        let (origin, mut footer) = if let Some(substitution_name) = substitution_name {
            (substitution_name, vec![Annotation {
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
            Type::UnexpectedTokenInBranchAccess => {
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
                            label: "Expected branch reference".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::NotFound(sym) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!("cannot find {} in this scope", sym.as_str())),
                        annotation_type: AnnotationType::Error,
                    }),
                    footer,
                    slices: vec![Slice {
                        source: line,
                        line_start: line_number as usize,
                        origin,
                        annotations: vec![SourceAnnotation {
                            range,
                            label: "not found in this Scope".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::DeclarationTypeMismatch {
                ref expected,
                found,
            } => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "Expected {:?} found {:?} {}",
                            expected,
                            found.mock(),
                            found.name(&ast).as_str()
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
                            label: format!("Expected {:?}", expected),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::NatureNotPotentialOrFlow(name, discipline) => {
                footer.push(Annotation {
                    id: None,
                    label: Some(format!(
                        "You can only access a branch using its Potential ({}) or Flow ({})",
                        ast[discipline].contents.potential_nature.name,
                        ast[discipline].contents.flow_nature.name
                    )),
                    annotation_type: AnnotationType::Info,
                });
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "{} can not be accessed by {}",
                            ast[discipline].contents.name,
                            &name.as_str(),
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
                            label: format!(
                                "Expected {} or {}",
                                ast[discipline].contents.potential_nature.name,
                                ast[discipline].contents.flow_nature.name
                            ),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::NotAScope { declaration, name } => {
                let (
                    other_declaration_line,
                    other_declaration_line_number,
                    other_declaration_origin,
                    other_declaration_range,
                ) = source_map.resolve_span_within_line(declaration, translate_lines);

                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let other_declaration_range = translate_to_inner_snippet_range(
                    other_declaration_range.start,
                    other_declaration_range.end,
                    &other_declaration_line,
                );

                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "Expected scope found reference to {}",
                            name.as_str()
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
                                label: "Expected scope".to_string(),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: other_declaration_line.to_string(),
                            line_start: other_declaration_line_number as usize,
                            origin: other_declaration_origin,
                            annotations: vec![SourceAnnotation {
                                range: other_declaration_range,
                                label: format!("{} is declared here", name.as_str()),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                    ],
                }
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

                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                let declaration_range = translate_to_inner_snippet_range(
                    declaration_range.start,
                    declaration_range.end,
                    &declaration_line,
                );
                let other_declaration_range = translate_to_inner_snippet_range(
                    other_declaration_range.start,
                    other_declaration_range.end,
                    &other_declaration_line,
                );

                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "Disciplines {} and {} of nets {} and {} are incompatible",
                            ast[net1.discipline].contents.name.name.as_str(),
                            ast[net2.discipline].contents.name.name.as_str(),
                            net1.name.as_str(),
                            net2.name.as_str()
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
                                label: "Incompatible disciplines".to_string(),
                                annotation_type: AnnotationType::Error,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: declaration_line.to_string(),
                            line_start: declaration_line_number as usize,
                            origin: declaration_origin,
                            annotations: vec![SourceAnnotation {
                                range: declaration_range,
                                label: format!("{} is declared here", net1.name.as_str()),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                        Slice {
                            source: other_declaration_line.to_string(),
                            line_start: other_declaration_line_number as usize,
                            origin: other_declaration_origin,
                            annotations: vec![SourceAnnotation {
                                range: other_declaration_range,
                                label: format!("{} is declared here", net2.name.as_str()),
                                annotation_type: AnnotationType::Info,
                            }],
                            fold: false,
                        },
                    ],
                }
            }
            Type::NotAllowedInConstantContext(non_constant_expr) => {
                let range = translate_to_inner_snippet_range(range.start, range.end, &line);
                Snippet {
                    title: Some(Annotation {
                        id: None,
                        label: Some(format!(
                            "{} are not allowed in a constant context!",
                            non_constant_expr
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
                            label: "Not allowed in a constant expression!".to_string(),
                            annotation_type: AnnotationType::Error,
                        }],
                        fold: false,
                    }],
                }
            }
            Type::Unsupported(unsupported) => {
                return parser::error::Error {
                    error_type: parser::error::Type::Unsupported(unsupported),
                    source: self.source,
                }
                .print(source_map, translate_lines)
            }
        };
        let display_list = DisplayList::from(snippet);
        let formatter = DisplayListFormatter::new(true, false);
        error!("{}", formatter.format(&display_list));
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
