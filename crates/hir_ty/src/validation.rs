mod body;
mod types;

use basedb::diagnostics::{Diagnostic, Label, LabelStyle, Report};
pub use body::BodyValidationDiagnostic;
use hir_def::body::BodySourceMap;
use hir_def::{ExprId, ItemLoc, ItemTreeNode, Lookup, NodeTypeDecl};
use syntax::name::Name;
use syntax::sourcemap::{FileSpan, SourceMap};
use syntax::{Parse, SourceFile};
pub use types::TypeValidationDiagnostic;

use crate::db::HirTyDB;
use crate::validation::body::{BodyCtx, IllegalCtxAccess, IllegalCtxAccessKind};

pub struct BodyValidationDiagnosticWrapped<'a> {
    pub body_sm: &'a BodySourceMap,
    pub diag: &'a BodyValidationDiagnostic,
    pub parse: &'a Parse<SourceFile>,
    pub db: &'a dyn HirTyDB,
    pub sm: &'a SourceMap,
}

impl BodyValidationDiagnosticWrapped<'_> {
    fn expr_src(&self, expr: ExprId) -> FileSpan {
        self.parse.to_file_span(self.body_sm.expr_map_back[expr].as_ref().unwrap().range(), self.sm)
    }

    fn lookup<I, T>(&self, id: I) -> (Name, FileSpan)
    where
        I: Lookup<Data = ItemLoc<T>>,
        T: ItemTreeNode,
    {
        let loc = id.lookup(self.db.upcast());
        let src = loc.ast_ptr(self.db.upcast()).range();
        (loc.name(self.db.upcast()), self.parse.to_file_span(src, self.sm))
    }
}

impl Diagnostic for BodyValidationDiagnosticWrapped<'_> {
    fn build_report(&self, root_file: basedb::FileId, db: &dyn basedb::BaseDB) -> Report {
        match self.diag {
            BodyValidationDiagnostic::ExpectedPort { expr, node } => {
                let FileSpan { range, file } = self.expr_src(*expr);
                let node = node.lookup(self.db.upcast());
                let module = node.module.lookup(self.db.upcast());
                let tree = module.item_tree(self.db.upcast());
                let node = &tree[module.id].nodes[node.id];
                let name = &node.name;

                let mut labels = vec![Label {
                    style: LabelStyle::Primary,
                    file_id: file,
                    range: range.into(),
                    message: "expected port".to_owned(),
                }];

                labels.extend(node.decls.iter().map(|decl| {
                    let net = match decl {
                        NodeTypeDecl::Net(net) => *net,
                        NodeTypeDecl::Port(_) => unreachable!(),
                    };

                    let range = db.ast_id_map(root_file).get(tree[net].ast_id).range();
                    let FileSpan { range, file } = self.parse.to_file_span(range, self.sm);
                    Label {
                        style: LabelStyle::Secondary,
                        file_id: file,
                        range: range.into(),
                        message: format!("info: '{}' was declared here", name),
                    }
                }));

                Report::error()
                    .with_message(format!(
                        "expected a port reference but no direction was declared for net '{}'",
                        name
                    ))
                    .with_labels(labels)
                    .with_notes(vec![
                        "help: prefix one of the declarations with inout, input or output"
                            .to_owned(),
                    ])
            }
            BodyValidationDiagnostic::PotentialOfPortFlow { expr, branch } => {
                let FileSpan { range, file } = self.expr_src(*expr);

                let mut labels = vec![Label {
                    style: LabelStyle::Primary,
                    file_id: file,
                    range: range.into(),
                    message: "invalid potential access".to_owned(),
                }];

                if let Some(branch) = branch {
                    let (name, FileSpan { range, file }) = self.lookup(*branch);

                    labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: file,
                        range: range.into(),
                        message: format!("info: '{}' was declared here", name),
                    });
                }

                Report::error()
                    .with_message("access of port-branch potential")
                    .with_labels(labels)
                    .with_notes(vec![
                        "help: only the flow of port branches like <foo> can be accessed"
                            .to_owned(),
                    ])
            }
            BodyValidationDiagnostic::IllegalContribute { stmt, ctx } => {
                let FileSpan { range, file } = self.parse.to_file_span(
                    self.body_sm.stmt_map_back[*stmt].as_ref().unwrap().range(),
                    self.sm,
                );

                Report::error()
                    .with_message(format!("branch contributions are not allowed in {}", ctx))
                    .with_labels(vec![Label {
                        style: LabelStyle::Secondary,
                        file_id: file,
                        range: range.into(),
                        message: "not allowed here".to_owned(),
                    }])
                    .with_notes(vec![
                        "help: branch contributions are only allowed in module-level analog blocks"
                            .to_owned(),
                    ])
            }
            BodyValidationDiagnostic::InvalidNodeDirectionForAccess {
                expr,
                nodes,
                branch,
                write,
            } => {
                let FileSpan { range, file } = self.expr_src(*expr);

                let (mut labels, notes): (Vec<_>, Vec<_>) = nodes
                    .iter()
                    .map(|node| {
                        let node = node.lookup(self.db.upcast());
                        let module = node.module.lookup(self.db.upcast());
                        let tree = module.item_tree(self.db.upcast());
                        let node = &tree[module.id].nodes[node.id];
                        let name = &node.name;

                        let port = node
                            .decls
                            .iter()
                            .find_map(|it| {
                                if let NodeTypeDecl::Port(port) = it {
                                    Some(*port)
                                } else {
                                    None
                                }
                            })
                            .unwrap();

                        let range = db.ast_id_map(root_file).get(tree[port].ast_id).range();
                        let FileSpan { range, file } = self.parse.to_file_span(range, self.sm);

                        (
                            Label {
                                style: LabelStyle::Secondary,
                                file_id: file,
                                range: range.into(),
                                message: format!(
                                    "info: '{}' was declared {} here",
                                    name,
                                    if tree[port].is_output { "output" } else { "input" }
                                ),
                            },
                            format!("help: change direction of '{}' to inout", name),
                        )
                    })
                    .unzip();

                let (message, label) = if *write {
                    ("contribute to branch with input ports", "illegal contribution")
                } else {
                    ("nature access of branch with output ports", "illegal branch access")
                };

                labels.push(Label {
                    style: LabelStyle::Primary,
                    file_id: file,
                    range: range.into(),
                    message: label.to_owned(),
                });

                if let Some(branch) = branch {
                    let (name, FileSpan { range, file }) = self.lookup(*branch);

                    labels.push(Label {
                        style: LabelStyle::Secondary,
                        file_id: file,
                        range: range.into(),
                        message: format!("info: '{}' was declared here", name),
                    });
                }

                Report::error()
                    .with_message(message.to_owned())
                    .with_labels(labels)
                    .with_notes(notes)
            }
            BodyValidationDiagnostic::WriteToInputArg { expr, arg } => {
                let FileSpan { range, file } = self.expr_src(*expr);
                let arg_name = arg.name(self.db.upcast());
                let arg_src = arg.ast_ptr(self.db.upcast()).range();
                let arg_src = self.parse.to_file_span(arg_src, self.sm);

                Report::error()
                    .with_message(format!("write to input function argument '{}'", arg_name))
                    .with_labels(vec![Label {
                        style: LabelStyle::Secondary,
                        file_id: arg_src.file,
                        range: arg_src.range.into(),
                        message: format!("help: '{}' is defined here", arg_name),
                    }])
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: file,
                        range: range.into(),
                        message: "write to input argument".to_owned(),
                    }])
                    .with_notes(vec![format!("help: change direction of '{}' to inout", arg_name)])
            }
            BodyValidationDiagnostic::IllegalParamAccess { def, expr, param } => {
                let FileSpan { range, file } = self.expr_src(*expr);
                let (def_name, def_src) = self.lookup(*def);
                let (ref_name, ref_src) = self.lookup(*param);

                Report::error()
                    .with_message(format!(
                        "defintion of '{}' references parameter '{}' defined afterwards",
                        def_name, ref_name
                    ))
                    .with_labels(vec![Label {
                        style: LabelStyle::Secondary,
                        file_id: def_src.file,
                        range: def_src.range.into(),
                        message: format!("help: '{}' is defined here", def_name),
                    }])
                    .with_labels(vec![Label {
                        style: LabelStyle::Primary,
                        file_id: file,
                        range: range.into(),
                        message: "illegal reference".to_owned(),
                    }])
                    .with_labels(vec![Label {
                        style: LabelStyle::Secondary,
                        file_id: ref_src.file,
                        range: ref_src.range.into(),
                        message: format!(".. to parameter '{}' defined here", ref_name),
                    }])
                    .with_notes(vec![
                        "help: parameters may only refere to parameters (textually) defined before them"
                            .to_owned(),
                    ])
            }
            BodyValidationDiagnostic::IllegalCtxAccess(IllegalCtxAccess { kind, ctx, expr }) => {
                let FileSpan { range, file } = self.expr_src(*expr);

                let mut res = Report::error().with_labels(vec![Label {
                    style: LabelStyle::Secondary,
                    file_id: file,
                    range: range.into(),
                    message: "not allowed here".to_owned(),
                }]);

                match kind {
                    IllegalCtxAccessKind::NatureAccess => res
                        .with_message(format!("nature access is not allowed in {}", ctx))
                        .with_notes(vec![
                            "help: nature access is only allowed in module-level analog blocks"
                                .to_owned(),
                        ]),
                    IllegalCtxAccessKind::AnalogOperator {
                        name,
                        is_standard: _, // TODO add a note?
                        non_const_dominator,
                    } => {
                        let notes = if *ctx == BodyCtx::Conditional {
                            vec![
                                "help: analog operators are only allowed in non-conditional behaviour".to_owned(),
                                "help: only constant and analysis functions are allowed in conditions".to_owned()
                            ]
                        } else {
                            vec!["help: analog operators are only allowed in the main-analog block"
                                .to_owned()]
                        };

                        res.labels.extend(non_const_dominator.iter().map(|expr| {
                            let FileSpan { range, file } = self.expr_src(*expr);
                            Label {
                                style: LabelStyle::Secondary,
                                file_id: file,
                                range: range.into(),
                                message: "help: this condition is not a constant".to_owned(),
                            }
                        }));

                        res.with_message(format!(
                            "analog operator '{}' is not allowed in {}",
                            name, ctx
                        ))
                        .with_notes(notes)
                    }
                    IllegalCtxAccessKind::AnalysisFun { name } => res.with_message(format!(
                        "analysis function '{}' is not allowed in constants",
                        name
                    )),
                    IllegalCtxAccessKind::Var(var) => {
                        let name = var.lookup(self.db.upcast()).name(self.db.upcast());
                        let def = var.lookup(self.db.upcast()).ast_ptr(self.db.upcast()).range();
                        let FileSpan { range, file } = self.parse.to_file_span(def, self.sm);
                        res.labels.push(Label {
                            style: LabelStyle::Secondary,
                            file_id: file,
                            range: range.into(),
                            message: format!("help: '{}' was declard here", name),
                        });
                        res.with_message(
                            "constant expressions must not contain variable references".to_owned(),
                        )
                    }
                }
            }
        }
    }
}


pub struct TypeValidationDiagnosticWrapped<'a> {
    pub diag: &'a TypeValidationDiagnostic,
    pub parse: &'a Parse<SourceFile>,
    pub db: &'a dyn HirTyDB,
    pub sm: &'a SourceMap,
}

impl Diagnostic for TypeValidationDiagnosticWrapped<'_>{
    fn build_report(&self, root_file: basedb::FileId, db: &dyn basedb::BaseDB) -> Report {
        todo!()
        // match self.diag {
        //     TypeValidationDiagnostic::PathError { err, src } => todo!(),
        //     TypeValidationDiagnostic::DuplicateDisciplineAttr(_) => todo!(),
        //     TypeValidationDiagnostic::DuplicateNatureAttr(_) => todo!(),
        //     TypeValidationDiagnostic::MultipleDirections(_) => todo!(),
        //     TypeValidationDiagnostic::MultipleDisciplines(_) => todo!(),
        //     TypeValidationDiagnostic::MultipleGnds(_) => todo!(),
        //     TypeValidationDiagnostic::PortWithoutDirection { decl } => {
        //         Report::error()
        //     }
        // }
    }
}


