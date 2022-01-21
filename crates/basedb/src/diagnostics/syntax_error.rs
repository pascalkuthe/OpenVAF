use std::iter::once;

use stdx::iter::zip;
use syntax::sourcemap::FileSpan;
use syntax::SyntaxKind::BLOCK_STMT;
use syntax::{AstNode, SyntaxError, TextRange, TextSize};

use crate::diagnostics::{
    text_range_list_to_unified_spans, text_ranges_to_unified_spans, Diagnostic, Label, LabelStyle,
    Report,
};
use crate::lints::builtin::vams_keyword_compat;
use crate::lints::{Lint, LintSrc};
use crate::{BaseDB, FileId};

fn syntax_err_report(missing_delimeter: bool) -> Report {
    if missing_delimeter {
        Report::error().with_notes(vec!["you might be missing a 'begin' delimeter".to_owned()])
    } else {
        Report::error()
    }
}

impl Diagnostic for SyntaxError {
    fn lint(&self, root_file: FileId, db: &dyn BaseDB) -> Option<(Lint, LintSrc)> {
        match self {
            SyntaxError::ReservedIdentifier { compat: true, src, .. } => Some((
                vams_keyword_compat,
                LintSrc {
                    overwrite: None,
                    ast: db.ast_id_map(root_file).nearest_ast_id_to_ptr(*src, db, root_file),
                },
            )),
            _ => None,
        }
    }
    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let sm = db.sourcemap(root_file);
        let parse = db.parse(root_file);

        let report = match *self {
            SyntaxError::UnexpectedToken {
                ref expected,
                span,
                expected_at: Some(expected_at),
                missing_delimeter,
                ..
            } => {
                let (file_id, [expected_at, range]) =
                    text_ranges_to_unified_spans(&sm, &parse, [expected_at, span]);
                syntax_err_report(missing_delimeter).with_labels(vec![
                    Label {
                        style: LabelStyle::Secondary,
                        file_id,
                        range: expected_at.into(),
                        message: format!("expected {}", expected),
                    },
                    Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "unexpected token".to_owned(),
                    },
                ])
            }

            SyntaxError::UnexpectedToken { ref expected, span, missing_delimeter, .. } => {
                let message = if expected.data.len() < 4 {
                    format!("expected {}", expected)
                } else {
                    "unexpected_token".to_owned()
                };
                let FileSpan { file: file_id, range } = parse.to_file_span(span, &sm);

                syntax_err_report(missing_delimeter).with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message,
                }])
            }

            SyntaxError::SurplusToken { found, span } => {
                let FileSpan { file: file_id, range } = parse.to_file_span(span, &sm);

                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: format!("unexpected '{}'", found),
                    }])
                    .with_notes(vec![format!(
                        "the '{}' token is not required here; simply remove it",
                        found
                    )])
            }
            SyntaxError::MissingToken { expected, span, expected_at } => {
                let (file_id, [expected_at, range]) =
                    text_ranges_to_unified_spans(&sm, &parse, [expected_at, span]);
                Report::error().with_labels(vec![
                    Label {
                        style: LabelStyle::Secondary,
                        file_id,
                        range: expected_at.into(),
                        message: format!("'{}' might be missing here", expected),
                    },
                    Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "unexpected token".to_owned(),
                    },
                ])
            }

            SyntaxError::IllegalRootSegment { path_segment, prefix: None } => {
                let FileSpan { file: file_id, range } = parse.to_file_span(path_segment, &sm);
                let end = TextRange::at(range.end() - TextSize::from(1), 1.into());

                Report::error().with_labels(vec![
                    Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "'$root' must be a prefix".to_owned(),
                    },
                    Label {
                        style: LabelStyle::Secondary,
                        file_id,
                        range: end.into(),
                        message: ".<identifier> might be missing here".to_owned(),
                    },
                ])
            }

            SyntaxError::IllegalRootSegment { path_segment, prefix: Some(prefix) } => {
                let (file_id, [prefix, path_segment]) =
                    text_ranges_to_unified_spans(&sm, &parse, [prefix, path_segment]);

                let prefix = TextRange::at(prefix.start() - TextSize::from(1), 1.into());

                Report::error().with_labels(vec![
                    Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: path_segment.into(),
                        message: "$root must be a prefix".to_owned(),
                    },
                    Label {
                        style: LabelStyle::Secondary,
                        file_id,
                        range: prefix.into(),
                        message: "perhaps you meant to place '$root' here".to_owned(),
                    },
                ])
            }
            SyntaxError::BlockItemsAfterStmt { ref items, first_stmt } => {
                let ranges: Vec<_> =
                    once(first_stmt).chain(items.iter().map(|item| item.range())).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &ranges);

                let first_stmt = ranges[0];
                let item_ranges = &ranges[1..];

                let mut labels: Vec<_> = zip(item_ranges, items)
                    .map(|(range, ast_ptr)| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: (*range).into(),
                        message: format!(
                            "{}s are only allowed before the first stmt",
                            ast_ptr.syntax_kind()
                        ),
                    })
                    .collect();

                labels.push(Label {
                    style: LabelStyle::Secondary,
                    file_id,
                    range: first_stmt.into(),
                    message: "help: move all declarations before this statement".to_owned(),
                });

                Report::error().with_labels(labels)
            }
            SyntaxError::BlockItemsWithoutScope { ref items, begin_token } => {
                let ranges: Vec<_> =
                    once(begin_token).chain(items.iter().map(|item| item.range())).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &ranges);

                let begin_token = TextRange::at(ranges[0].end() - TextSize::from(1), 1.into());
                let item_ranges = &ranges[1..];

                let mut labels: Vec<_> = zip(item_ranges, items)
                    .map(|(range, ast_ptr)| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: (*range).into(),
                        message: format!("{}s require a scope", ast_ptr.syntax_kind()),
                    })
                    .collect();

                labels.push(Label {
                    style: LabelStyle::Secondary,
                    file_id,
                    range: begin_token.into(),
                    message: "help: add ':<scope>' here".to_owned(),
                });

                Report::error().with_labels(labels)
            }
            SyntaxError::FunItemsAfterBody { ref items, body } => {
                let ranges: Vec<_> =
                    once(body).chain(items.iter().map(|item| item.range())).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &ranges);

                let body = ranges[0];
                let item_ranges = &ranges[1..];

                let mut labels: Vec<_> = zip(item_ranges, items)
                    .map(|(range, ast_ptr)| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: (*range).into(),
                        message: format!(
                            "{}s are not allowed after the function body",
                            ast_ptr.syntax_kind()
                        ),
                    })
                    .collect();

                labels.push(Label {
                    style: LabelStyle::Secondary,
                    file_id,
                    range: body.into(),
                    message: "help: move all declarations before this statement".to_owned(),
                });

                Report::error().with_labels(labels)
            }
            SyntaxError::MultipleFunBodys { ref additional_bodys, ref body } => {
                let (range, message) = if body.syntax_kind() == BLOCK_STMT {
                    (body.range(), "help: add these statements to this block".to_owned())
                } else {
                    (
                        body.range().cover(*additional_bodys.last().unwrap()),
                        "help: surround with begin ... end to create a single function body"
                            .to_owned(),
                    )
                };

                let ranges: Vec<_> = once(range).chain(additional_bodys.iter().copied()).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &ranges);

                let range = ranges[0];
                let item_ranges = &ranges[1..];

                let mut labels: Vec<_> = item_ranges
                    .iter()
                    .map(|range| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: (*range).into(),
                        message: "only one body per function is ".to_owned(),
                    })
                    .collect();

                labels.push(Label {
                    style: LabelStyle::Secondary,
                    file_id,
                    range: range.into(),
                    message,
                });

                Report::error().with_labels(labels)
            }

            SyntaxError::FunWithoutBody { fun } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(fun, &sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message: "function body is missing".to_owned(),
                }])
            }

            SyntaxError::IllegalBranchNodeCnt { arg_list, .. } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(arg_list, &sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message: "expected 1 or 2 nets".to_owned(),
                }])
            }

            SyntaxError::IllegalBranchNodeExpr { single, ref illegal_nodes } => {
                let (file_id, illegal_nodes) =
                    text_range_list_to_unified_spans(&sm, &parse, illegal_nodes);

                let labels = illegal_nodes
                    .into_iter()
                    .map(|range| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "unexpected expression".to_owned(),
                    })
                    .collect();

                let hint = if single {
                    "help: expected an identifier or a port flow expression (<port>)"
                } else {
                    "help: expected an identifier"
                };

                Report::error().with_labels(labels).with_notes(vec![hint.to_owned()])
            }
            SyntaxError::IllegalInfToken { range } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(range, &sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message: "unexpected token".to_owned(),
                }]).with_notes(vec!["help: 'inf' is only allowed in ranges of parameter declarations (example: [0:inf])".to_owned()])
            }
            SyntaxError::UnitsExpectedStringLiteral { range } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(range, &sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message: "expected string literal".to_owned(),
                }])
            }
            SyntaxError::IllegalDisciplineAttrIdent { range } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(range, &sm);
                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "illegal attribute path".to_owned(),
                    }])
                    .with_notes(vec![
                        "help: expected one of the following".to_owned(),
                        "an identifier: abstol".to_owned(),
                        "an identifier precceded by potential or flow: potential.abstol".to_owned(),
                    ])
            }
            SyntaxError::IllegalNatureIdent { range } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(range, &sm);
                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "illegal nature identifier".to_owned(),
                    }])
                    .with_notes(vec![
                        "help: expected one of the following".to_owned(),
                        "an identifier: voltage".to_owned(),
                        "an identifier followed by potential or flow: electrical.potential"
                            .to_owned(),
                    ])
            }
            SyntaxError::IllegalAttriubte { expected, range, .. } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(range, &sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message: format!("expected {}", expected),
                }])
            }
            SyntaxError::ReservedIdentifier { src, compat, ref name } => {
                let FileSpan { file, range } = parse.to_file_span(src.range(), &sm);

                let report = Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id: file,
                    range: range.into(),
                    message: format!("'{}' is a keyword", name),
                }]);

                // TODO error code (doc)

                if compat {
                    report.with_notes(vec![
                        format!(
                        "'{}' will likely never be used in the implemented language subset so this use is allowed",
                        name
                        ),
                        "to maintain compatibility with the VAMS standard this should be renamed".to_owned()
                    ])
                } else {
                    report
                }
            }
            SyntaxError::DuplicatePort { ref pos, ref name } => {
                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, pos);
                let ranges = &ranges[1..];
                let inital = ranges[0];

                let mut labels: Vec<_> = ranges
                    .iter()
                    .map(|range| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: (*range).into(),
                        message: "..redeclared here".to_owned(),
                    })
                    .collect();

                labels.push(Label {
                    style: LabelStyle::Secondary,
                    file_id,
                    range: inital.into(),
                    message: format!("{} first declared here", name),
                });

                Report::error().with_labels(labels)
            }
            SyntaxError::MixedModuleHead { ref module_ports } => {
                let ports = module_ports.to_node(parse.tree().syntax());

                let name_cnt = ports.names().count();
                let name_ranges: Vec<_> = ports
                    .names()
                    .map(|name| name.syntax().text_range())
                    .chain(ports.declarations().map(|port| port.syntax().text_range()))
                    .collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &name_ranges);
                let names = &ranges[..name_cnt];
                let ports = &ranges[name_cnt..];

                let labels: Vec<_> = names
                    .iter()
                    .map(|range| Label {
                        style: LabelStyle::Secondary,
                        file_id,
                        range: (*range).into(),
                        message: "found reference here".to_owned(),
                    })
                    .chain(ports.iter().map(|range| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: (*range).into(),
                        message: "port declaration not allowed".to_owned(),
                    }))
                    .collect();

                Report::error().with_labels(labels).with_notes(vec![
                    "either declare all ports directly in the header: module example(inout foo, inout bar);".to_owned(),
                    "or only reference ports in the header: module example(foo,bar);".to_owned(),
                ])
            }
            SyntaxError::IllegalBodyPorts { head, ref body_ports } => {
                let mut ranges = vec![head];
                ranges.extend(body_ports);

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &ranges);
                let head = ranges[0];
                let body_ports = &ranges[1..];

                let mut labels: Vec<_> = body_ports
                    .iter()
                    .map(|range| Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: (*range).into(),
                        message: "illegal port declaration".to_owned(),
                    })
                    .collect();

                labels.push(Label {
                    style: LabelStyle::Secondary,
                    file_id,
                    range: head.into(),
                    message: "info: ports already declared in header...".to_owned(),
                });

                Report::error().with_labels(labels).with_notes(vec![
                    "either place all port declaration in the header".to_owned(),
                    "or palce all port declarations in the body ".to_owned(),
                ])
            }
            SyntaxError::IllegalNetType { range, .. } => {
                let FileSpan { range, file: file_id } = parse.to_file_span(range, &sm);
                Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id,
                    range: range.into(),
                    message: "unsupported net type".to_owned(),
                }])
            }
            SyntaxError::RangeConstraintForNonNumericParameter { range, ty, .. } => {
                let (file_id, [range, ty]) = text_ranges_to_unified_spans(&sm, &parse, [range, ty]);
                Report::error().with_labels(vec![
                    Label {
                        style: LabelStyle::Primary,
                        file_id,
                        range: range.into(),
                        message: "illegal range bounds".to_owned(),
                    },
                    Label {
                        style: LabelStyle::Secondary,
                        file_id,
                        range: ty.into(),
                        message: "help: expected real or integer".to_owned(),
                    },
                ])
            }
        };

        report.with_message(self.to_string())
    }
}
