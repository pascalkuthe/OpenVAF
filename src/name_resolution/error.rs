use annotate_snippets::display_list::DisplayList;
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use log::error;

use crate::ir::DisciplineId;
use crate::parser::error::translate_to_inner_snippet_range;
use crate::symbol::Symbol;
use crate::symbol_table::SymbolDeclaration;
use crate::{Ast, SourceMap, Span};

pub type Error<'tag> = crate::error::Error<Type<'tag>>;
//pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<'tag, T = ()> = std::result::Result<T, Error<'tag>>;

#[derive(Clone, Debug)]
pub enum Type<'tag> {
    NotFound(Symbol),
    NotAScope {
        declaration: Span,
    },
    DeclarationTypeMismatch {
        expected: Vec<MockSymbolDeclaration>,
        found: SymbolDeclaration<'tag>,
    },
    UnexpectedTokenInBranchAccess,
    ImplicitDisciplinesUnsupported,
    NatureNotPotentialOrFlow(Symbol, DisciplineId<'tag>),
    DisciplineMismatch(DisciplineId<'tag>, DisciplineId<'tag>),
    NotAllowedInConstantContext(NonConstantExpression),
}
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NonConstantExpression {
    VariableReference,
    BranchAccess,
    FunctionCall,
    AnalogFilter,
}

impl<'tag> Error<'tag> {
    pub fn print(&self, source_map: &SourceMap, ast: &Ast<'tag>, translate_lines: bool) {
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
                            found.name(&ast)
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
            _ => unimplemented!("{:?}", self.error_type),
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
