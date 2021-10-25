use std::iter::once;

use data_structures::iter::zip;
use syntax::{sourcemap::FileSpan, SyntaxError, SyntaxKind::BLOCK_STMT, TextRange, TextSize};

use crate::{BaseDB, FileId, diagnostics::{
    text_range_list_to_unified_spans, text_ranges_to_unified_spans, Diagnostic, Label, LabelStyle,
    Report,
}};

fn syntax_err_report(missing_delimeter: bool) -> Report {
    if missing_delimeter {
        Report::error().with_notes(vec!["you might be missing a 'begin' delimeter".to_owned()])
    } else {
        Report::error()
    }
}

impl Diagnostic for SyntaxError {
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
                let end = TextRange::at(range.end(), 1.into());

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
                let mut ranges: Vec<_> =
                    once(first_stmt).chain(items.iter().map(|item| item.range())).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &mut ranges);

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
                let mut ranges: Vec<_> =
                    once(begin_token).chain(items.iter().map(|item| item.range())).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &mut ranges);

                let begin_token = TextRange::at(ranges[0].end() + TextSize::from(1), 1.into());
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
                let mut ranges: Vec<_> =
                    once(body).chain(items.iter().map(|item| item.range())).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &mut ranges);

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

                let mut ranges: Vec<_> =
                    once(range).chain(additional_bodys.iter().copied()).collect();

                let (file_id, ranges) = text_range_list_to_unified_spans(&sm, &parse, &mut ranges);

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
        };

        report.with_message(self.to_string())
    }
}
