use std::borrow::Cow;
use std::ops::Deref;

use basedb::diagnostics::{
    to_unified_span_list, to_unified_spans, Diagnostic, Label, LabelStyle, Report,
};
use basedb::lints::builtin::non_standard_code;
use basedb::lints::{Lint, LintSrc};
use basedb::{BaseDB, FileId};
use hir_def::body::BodySourceMap;

use hir_def::{ExprId, FunctionId, Lookup, Type};
use stdx::iter::zip;
use stdx::{impl_display, pretty};
use syntax::ast::{self, AssignOp};
use syntax::sourcemap::{FileSpan, SourceMap};
use syntax::{Parse, SourceFile};
use typed_index_collections::TiSlice;

use crate::db::HirTyDB;
use crate::inference::InferenceDiagnostic;
use crate::types::{Signature, SignatureData, Ty, TyRequirement};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeMissmatch {
    pub expected: Cow<'static, [TyRequirement]>,
    pub found_ty: Ty,
    pub expr: ExprId,
}

impl_display! {
    match TypeMissmatch{
        TypeMissmatch{expected, ..} => "expected {}" , pretty::List::new(expected.deref());
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayTypeMissmatch {
    pub expected: Type,
    pub found_ty: Type,
    pub found_expr: ExprId,
    pub expected_expr: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureMissmatch {
    pub type_missmatches: Box<[TypeMissmatch]>,
    pub signatures: Cow<'static, TiSlice<Signature, SignatureData>>,
    pub src: Option<FunctionId>,
    pub found: Box<[Ty]>,
}

pub struct InferenceDiagnosticWrapped<'a> {
    pub body_sm: &'a BodySourceMap,
    pub diag: &'a InferenceDiagnostic,
    pub parse: &'a Parse<SourceFile>,
    pub db: &'a dyn HirTyDB,
    pub sm: &'a SourceMap,
}

impl Diagnostic for InferenceDiagnosticWrapped<'_> {
    fn build_report(&self, _root_file: FileId, _db: &dyn BaseDB) -> Report {
        match *self.diag {
            InferenceDiagnostic::InvalidAssignDst {
                e,
                maybe_different_operand,
                assigment_kind,
            } => {
                let src = self
                    .parse
                    .to_file_span(self.body_sm.expr_map_back[e].as_ref().unwrap().range(), self.sm);

                let res = Report::error().with_labels(vec![Label {
                    style: LabelStyle::Primary,
                    file_id: src.file,
                    range: src.range.into(),
                    message: "invalid destination".to_owned(),
                }]);

                let res = match assigment_kind {
                    AssignOp::Contribute => res
                        .with_message("invalid destination for branch contribution")
                        .with_notes(vec![
                            "help: expected nature access such as V(foo) or I(foo)".to_owned()
                        ]),
                    AssignOp::Assign => res
                        .with_message("invalid destination for assginment")
                        .with_notes(vec!["help: expected a variable".to_owned()]),
                };

                match maybe_different_operand {
                    Some(ast::AssignOp::Contribute) => res.with_notes(vec![
                        "help: found a branch access\nperhaps you mean to contribute (<+)"
                            .to_owned(),
                    ]),
                    Some(ast::AssignOp::Assign) => res.with_notes(vec![
                        "help: found a variable\nperhaps you meant to assign (=) a value"
                            .to_owned(),
                    ]),
                    None => res,
                }
            }
            InferenceDiagnostic::PathResolveError { ref err, expr } => {
                let src = self.parse.to_file_span(
                    self.body_sm.expr_map_back[expr].as_ref().unwrap().range(),
                    self.sm,
                );

                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: src.file,
                        range: src.range.into(),
                        message: err.message(),
                    }])
                    .with_message(err.to_string())
            }
            InferenceDiagnostic::ArgCntMissmatch { expected, found, expr, exact } => {
                let src = self.parse.to_file_span(
                    self.body_sm.expr_map_back[expr].as_ref().unwrap().range(),
                    self.sm,
                );

                let message = match (expected < found, exact) {
                    (_, true) => format!("expected {} arguments", expected),
                    (true, false) => format!("expected at most {} arguments", expected),
                    (false, false) => format!("expected at least {} arguments", expected),
                };

                Report::error()
                    .with_message(&format!(
                        "invalid argument count: {} but found {}",
                        &message, found
                    ))
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: src.file,
                        range: src.range.into(),
                        message,
                    }])
            }
            InferenceDiagnostic::TypeMissmatch(ref err) => {
                let src = self.parse.to_file_span(
                    self.body_sm.expr_map_back[err.expr].as_ref().unwrap().range(),
                    self.sm,
                );

                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: src.file,
                        range: src.range.into(),
                        message: err.to_string(),
                    }])
                    .with_message(format!("type missmatch: {} but found {}", &err, err.found_ty))
            }
            InferenceDiagnostic::SignatureMissmatch(ref err) => {
                let mut res = if let [ref ty_err] = *err.type_missmatches {
                    let FileSpan { file, range } = self.parse.to_file_span(
                        self.body_sm.expr_map_back[ty_err.expr].as_ref().unwrap().range(),
                        self.sm,
                    );

                    Report::error()
                        .with_labels(vec![])
                        .with_message(format!(
                            "type missmatch: {} but found {}",
                            &ty_err, ty_err.found_ty
                        ))
                        .with_labels(vec![Label {
                            style: LabelStyle::Primary,
                            file_id: file,
                            range: range.into(),
                            message: ty_err.to_string(),
                        }])
                } else {
                    let mut spans: Vec<_> = err
                        .type_missmatches
                        .iter()
                        .map(|it| {
                            self.parse.to_ctx_span(
                                self.body_sm.expr_map_back[it.expr].as_ref().unwrap().range(),
                                self.sm,
                            )
                        })
                        .collect();

                    let (file, ranges) = to_unified_span_list(self.sm, &mut spans);
                    let labels = zip(ranges, &*err.type_missmatches)
                        .map(|(range, ty_err)| Label {
                            style: LabelStyle::Primary,
                            file_id: file,
                            range: range.into(),
                            message: ty_err.to_string(),
                        })
                        .collect();

                    let mut notes = vec![format!(
                        "help: found ({})",
                        pretty::List::with_final_seperator(&*err.found, ", ")
                    )];

                    notes.extend(err.signatures.iter().map(|sig| format!("expected {}", sig)));

                    Report::error()
                        .with_message("typed missmatch invalid function arguments".to_owned())
                        .with_labels(labels)
                        .with_notes(notes)
                };

                if let Some(src) = err.src {
                    let fun = src.lookup(self.db.upcast());
                    let name = fun.item_tree(self.db.upcast())[fun.id].name.clone();
                    let range = fun.ast_ptr(self.db.upcast()).range();
                    let span = self.parse.to_file_span(range, self.sm);
                    res.labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: span.file,
                        range: span.range.into(),
                        message: format!("info: '{}' was declared here", name),
                    })
                }

                res
            }
            InferenceDiagnostic::ArrayTypeMissmatch(ArrayTypeMissmatch {
                ref expected,
                ref found_ty,
                found_expr,
                expected_expr,
            }) => {
                let found_range = self.body_sm.expr_map_back[found_expr].as_ref().unwrap().range();
                let expected_range =
                    self.body_sm.expr_map_back[expected_expr].as_ref().unwrap().range();

                let expected_span = self.parse.to_ctx_span(expected_range, self.sm);
                let found_span = self.parse.to_ctx_span(found_range, self.sm);

                let (file, [found_range, expected_range]) =
                    to_unified_spans(self.sm, [found_span, expected_span]);

                Report::error()
                    .with_labels(vec![
                        Label {
                            style: LabelStyle::Primary,
                            file_id: file,
                            range: found_range.into(),
                            message: format!("expected {}", expected),
                        },
                        Label {
                            style: LabelStyle::Secondary,
                            file_id: file,
                            range: expected_range.into(),
                            message: format!("expected because this is {}", found_ty),
                        },
                    ])
                    .with_message(format!("type missmatch: {} but found {}", expected, found_ty))
                    .with_notes(vec!["help: all array elements must have the same type".to_owned()])
            }
            InferenceDiagnostic::InvalidUnkown { e } => {
                let src = self
                    .parse
                    .to_file_span(self.body_sm.expr_map_back[e].as_ref().unwrap().range(), self.sm);

                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: src.file,
                        range: src.range.into(),
                        message: "invalid ddx unkown".to_owned(),
                    }])
                    .with_message("invalid unkown was supplied to the ddx operator")
                    .with_notes(vec![
                        "help: expected one of the following\nbranch current acces: I(branch), I(a,b)\nnode voltage: V(x)\nexplicit voltage: V(x,y)\ntemperature: $temperature".to_owned(),
                    ])
            }
            InferenceDiagnostic::NonStandardUnkown { e, .. } => {
                let src = self
                    .parse
                    .to_file_span(self.body_sm.expr_map_back[e].as_ref().unwrap().range(), self.sm);

                Report::warning()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: src.file,
                        range: src.range.into(),
                        message: "unkown is not standard compliant".to_owned(),
                    }])
                    .with_message("unkown supplied to the ddx operator is not standard compilant")
                    .with_notes(vec![
                        "note: this functionality is fully suported by openvaf\nbut other Verilog-A compilers might not support it".to_owned(),
                        "help: expected one of the following\nbranch current acces: I(branch), I(a,b)\nnode voltage: V(x)".to_owned(),
                    ])
            }
            InferenceDiagnostic::ExpectedProbe { e } => {
                let src = self
                    .parse
                    .to_file_span(self.body_sm.expr_map_back[e].as_ref().unwrap().range(), self.sm);

                Report::error()
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: src.file,
                        range: src.range.into(),
                        message: "expected a branch probe".to_owned(),
                    }])
                    .with_message("'$limit' expected a branch probe as the first argument")
                    .with_notes(vec![
                        "help: expected nature access such as V(foo) or I(foo)".to_owned()
                    ])
            }
            InferenceDiagnostic::InvalidLimitFunction {
                expr,
                func,
                invalid_arg0,
                invalid_arg1,
                invalid_ret,
                ref output_args,
            } => {
                let src = self.parse.to_file_span(
                    self.body_sm.expr_map_back[expr].as_ref().unwrap().range(),
                    self.sm,
                );

                let func = func.lookup(self.db.upcast());
                let name = func.name(self.db.upcast());
                let tree = func.item_tree(self.db.upcast());
                let mut labels = Vec::new();
                let id_map = self.db.ast_id_map(func.scope.root_file);

                let decl = tree[func.id].ast_id;
                let decl = id_map.get_syntax(decl.into()).range();
                let decl = self.parse.to_file_span(decl, self.sm);
                labels.push(Label {
                    style: LabelStyle::Primary,
                    file_id: decl.file,
                    range: decl.range.into(),
                    message: "invalid $limit function".to_owned(),
                });

                labels.push(Label {
                    style: LabelStyle::Secondary,
                    file_id: src.file,
                    range: src.range.into(),
                    message: format!("info: {name} is used in $limit here"),
                });

                if invalid_arg0 {
                    let decl = tree[tree[func.id].args.raw[0].declarations[0]].ast_id;
                    let decl = id_map.get_syntax(decl.into()).range();
                    let decl = self.parse.to_file_span(decl, self.sm);
                    labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: decl.file,
                        range: decl.range.into(),
                        message: "help: first argument must have 'real' type".to_owned(),
                    })
                }

                if invalid_arg1 {
                    let decl = tree[tree[func.id].args.raw[1].declarations[0]].ast_id;
                    let decl = id_map.get_syntax(decl.into()).range();
                    let decl = self.parse.to_file_span(decl, self.sm);
                    labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: decl.file,
                        range: decl.range.into(),
                        message: "help: second argument must have 'real' type".to_owned(),
                    })
                }

                for arg in output_args {
                    let decl = tree[func.id].args[*arg].ast_ids[0];
                    let decl = id_map.get_syntax(decl.into()).range();
                    let decl = self.parse.to_file_span(decl, self.sm);
                    labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: decl.file,
                        range: decl.range.into(),
                        message: "help: argument must have direction 'input'".to_owned(),
                    })
                }

                let mut notes = Vec::new();

                if invalid_ret {
                    notes.push("help: return type must be real".to_owned());
                }

                Report::error()
                    .with_labels(labels)
                    .with_message(format!("{name} is not a valid function for use with $limit"))
                    .with_notes(notes)
            }
        }
    }

    fn lint(&self, _root_file: FileId, _db: &dyn BaseDB) -> Option<(Lint, LintSrc)> {
        if let InferenceDiagnostic::NonStandardUnkown { stmt, .. } = *self.diag {
            Some((non_standard_code, self.body_sm.lint_src(stmt, non_standard_code)))
        } else {
            None
        }
    }
}
