use std::mem::replace;

use arrayvec::ArrayVec;
use hir_def::body::Body;
use hir_def::{
    BranchId, BuiltIn, DefWithBodyId, Expr, ExprId, Lookup, NodeId, ParamId, Path, Stmt, StmtId,
    VarId,
};
use syntax::ast::AssignOp;
use syntax::name::{AsIdent, Name};

use crate::builtin::{
    ABSDELAY_MAX, DDT_TOL, IDT_IC_ASSERT_TOL, NATURE_ACCESS_BRANCH, NATURE_ACCESS_NODES,
    NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW, TRANSITION_DELAY_RISET_FALLT_TOL,
};
use crate::db::HirTyDB;
use crate::inference::{InferenceResult, ResolvedFun};
use crate::lower::BranchKind;
use crate::types::{Signature, Ty};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum IllegalCtxAccessKind {
    NatureAccess,
    AnalogOperator { name: Name, is_standard: bool, non_const_dominator: Box<[IllegalCtxAccess]> },
    AnalysisFun { name: Name },
    Var(VarId),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct IllegalCtxAccess {
    pub kind: IllegalCtxAccessKind,
    pub ctx: BodyCtx,
    pub expr: ExprId,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BodyValidationDiagnostic {
    ExpectedPort {
        expr: ExprId,
        node: NodeId,
    },
    PotentialOfPortFlow {
        expr: ExprId,
        branch: Option<BranchId>,
    },
    // IncompatibleNodes {
    //     expr: ExprId,
    //     err: IncompatibleNodes,
    // },
    IllegalNatureAccess {
        expr: ExprId,
        ctx: BodyCtx,
    },
    IllegalAnalogFilter {
        expr: ExprId,
        name: Path,
        ctx: BodyCtx,
    },
    IllegalContribute {
        stmt: StmtId,
        ctx: BodyCtx,
    },
    InvalidNodeDirectionForAccess {
        expr: ExprId,
        nodes: ArrayVec<NodeId, 2>,
        branch: Option<BranchId>,
        write: bool,
    },

    InvalidArgDirectionForAccess {
        expr: ExprId,
        write: bool,
    },

    IllegalParamAccess {
        def: ParamId,
        expr: ExprId,
        param: ParamId,
    },

    IllegalCtxAccess(IllegalCtxAccess),
}

impl BodyValidationDiagnostic {
    pub fn collect(db: &dyn HirTyDB, def: DefWithBodyId) -> Vec<BodyValidationDiagnostic> {
        let body = db.body(def);
        let infere = db.inference_result(def);

        let ctx = match def {
            DefWithBodyId::ModuleId(_) => BodyCtx::AnalogBlock,
            DefWithBodyId::FunctionId(_) => BodyCtx::Function,
            _ => BodyCtx::Const,
        };

        let mut validator = BodyValidator {
            db,
            owner: def,
            body: &body,
            infer: &infere,
            diagnostics: Vec::new(),
            ctx,
            non_const_dominator: vec![].into_boxed_slice(),
        };

        for stmt in &*body.entry_stmts {
            validator.validate_stmt(*stmt)
        }

        validator.diagnostics
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BodyCtx {
    AnalogBlock,
    Conditional,
    EventControl,
    Function,
    ConstOrAnalysis,
    Const,
}

impl BodyCtx {
    fn allow_nature_access(self) -> bool {
        matches!(self, Self::AnalogBlock | Self::Conditional | Self::EventControl)
    }

    fn allow_contribute(self) -> bool {
        matches!(self, Self::AnalogBlock | Self::Conditional)
    }

    fn allow_analog_operator(self) -> bool {
        matches!(self, Self::AnalogBlock)
    }

    fn allow_analysis_fun(self) -> bool {
        !matches!(self, Self::Const)
    }

    fn allow_var_ref(self) -> bool {
        !matches!(self, Self::Const | Self::ConstOrAnalysis)
    }
}

struct BodyValidator<'a> {
    db: &'a dyn HirTyDB,
    owner: DefWithBodyId,
    body: &'a Body,
    infer: &'a InferenceResult,
    diagnostics: Vec<BodyValidationDiagnostic>,
    ctx: BodyCtx,
    non_const_dominator: Box<[IllegalCtxAccess]>,
}

impl BodyValidator<'_> {
    fn validate_stmt(&mut self, stmt: StmtId) {
        let cond = match self.body.stmts[stmt] {
            Stmt::Assigment { dst, val, assignment_kind } => {
                self.validate_expr(val);

                if assignment_kind == AssignOp::Contribute && !self.ctx.allow_contribute() {
                    self.diagnostics
                        .push(BodyValidationDiagnostic::IllegalContribute { stmt, ctx: self.ctx })
                }
                // avoid duplicate errors
                else if self.infer.assigment_destination.contains_key(&stmt) {
                    self.validate_assigment_dst(dst);
                }

                return;
            }
            Stmt::EventControl { body, .. } => {
                let old = replace(&mut self.ctx, BodyCtx::EventControl);
                self.validate_stmt(body);
                self.ctx = old;
                return;
            }
            Stmt::Block { ref body } => {
                body.iter().for_each(|stmt| self.validate_stmt(*stmt));
                return;
            }

            Stmt::Missing | Stmt::Empty => return,

            Stmt::Expr(e) => {
                self.validate_expr(e);
                return;
            }

            Stmt::If { cond, .. }
            | Stmt::ForLoop { cond, .. }
            | Stmt::WhileLoop { cond, .. }
            | Stmt::Case { discr: cond, .. } => cond,
        };

        self.validate_condition(cond, |s| {
            s.body.stmts[stmt].walk_child_stmts(|stmt| s.validate_stmt(stmt))
        });
    }

    fn validate_condition(
        &mut self,
        cond: ExprId,
        f: impl FnOnce(&mut Self),
    ) -> Option<Box<[IllegalCtxAccess]>> {
        if self.ctx == BodyCtx::AnalogBlock || self.ctx == BodyCtx::Conditional {
            let mut non_const_access = Vec::new();
            ExprValidator {
                parent: self,
                cond_diagnostic_sink: Some(&mut non_const_access),
                write: false,
            }
            .validate_expr(cond);

            if !non_const_access.is_empty() {
                let non_const_dominator =
                    replace(&mut self.non_const_dominator, non_const_access.into_boxed_slice());
                let ctx = replace(&mut self.ctx, BodyCtx::Conditional);
                f(self);
                self.ctx = ctx;
                return Some(replace(&mut self.non_const_dominator, non_const_dominator));
            }
        } else {
            self.validate_expr(cond);
        }

        f(self);
        None
    }

    fn validate_expr(&mut self, expr: ExprId) {
        ExprValidator { parent: self, cond_diagnostic_sink: None, write: false }.validate_expr(expr)
    }

    fn validate_assigment_dst(&mut self, expr: ExprId) {
        ExprValidator { parent: self, cond_diagnostic_sink: None, write: true }.validate_expr(expr)
    }
}

struct ExprValidator<'a, 'b> {
    parent: &'a mut BodyValidator<'b>,
    cond_diagnostic_sink: Option<&'a mut Vec<IllegalCtxAccess>>,
    write: bool,
}

impl ExprValidator<'_, '_> {
    fn report_illegal_access(&mut self, kind: IllegalCtxAccessKind, expr: ExprId) {
        let err = IllegalCtxAccess { kind, ctx: self.parent.ctx, expr };
        match &mut self.cond_diagnostic_sink {
            Some(sink) => sink.push(err),
            None => self.parent.diagnostics.push(BodyValidationDiagnostic::IllegalCtxAccess(err)),
        }
    }

    fn report(&mut self, diagnostic: BodyValidationDiagnostic) {
        self.parent.diagnostics.push(diagnostic)
    }

    fn validate_expr(&mut self, expr: ExprId) {
        match self.parent.body.exprs[expr] {
            Expr::Call { ref fun, ref args, .. } => {
                if let Some(ResolvedFun::BuiltIn(builtin)) =
                    self.parent.infer.resolved_calls.get(&expr)
                {
                    let signature = self.parent.infer.resolved_signatures.get(&expr);
                    self.validate_builtin(fun, expr, args, *builtin, signature.cloned());
                    return;
                }
            }

            Expr::Select { cond, then_val, else_val } => {
                // false positive see https://github.com/rust-lang/rust-clippy/issues/8047
                #[allow(clippy::needless_option_as_deref)]
                if let Some(non_const_dominators) = self.parent.validate_condition(cond, |s| {
                    let mut validator = ExprValidator {
                        parent: s,
                        cond_diagnostic_sink: self.cond_diagnostic_sink.as_deref_mut(),
                        write: false,
                    };
                    validator.validate_expr(then_val);
                    validator.validate_expr(else_val);
                }) {
                    if let Some(sink) = &mut self.cond_diagnostic_sink {
                        sink.extend(non_const_dominators.to_vec())
                    }
                }
            }

            Expr::Path { port: false, .. } => {
                match self.parent.infer.expr_types[expr] {
                    Ty::FuntionVar { arg: Some(arg), fun, .. } => {
                        let arg = &self.parent.db.function_data(fun).args[arg];
                        if self.write && !arg.is_output {
                            self.parent.diagnostics.push(
                                BodyValidationDiagnostic::InvalidArgDirectionForAccess {
                                    expr,
                                    write: self.write,
                                },
                            )
                        }
                    }

                    Ty::Var(_, var) if !self.parent.ctx.allow_var_ref() => {
                        self.report_illegal_access(IllegalCtxAccessKind::Var(var), expr);
                    }
                    Ty::Param(_, param) => {
                        if let DefWithBodyId::ParamId(def) = self.parent.owner {
                            if def.lookup(self.parent.db.upcast()).id
                                < param.lookup(self.parent.db.upcast()).id
                            {
                                self.parent.diagnostics.push(
                                    BodyValidationDiagnostic::IllegalParamAccess {
                                        def,
                                        expr,
                                        param,
                                    },
                                )
                            }
                        }
                    }
                    _ => (),
                };
                return;
            }

            _ => (),
        }

        self.parent.body.exprs[expr].walk_child_exprs(|child| self.validate_expr(child))
    }

    fn validate_builtin(
        &mut self,
        name: &Option<Path>,
        expr: ExprId,
        mut args: &[ExprId],
        call: BuiltIn,
        signature: Option<Signature>,
    ) {
        match (call, signature) {
            _ if (call.is_analog_operator() || call.is_analog_operator_sysfun())
                && !self.parent.ctx.allow_analog_operator() =>
            {
                let non_const_dominator = if self.cond_diagnostic_sink.is_none() {
                    self.parent.non_const_dominator.clone()
                } else {
                    vec![].into_boxed_slice()
                };

                self.report_illegal_access(
                    IllegalCtxAccessKind::AnalogOperator {
                        name: name.as_ref().and_then(|p| p.as_ident()).unwrap(),
                        is_standard: call.is_analog_operator(),
                        non_const_dominator,
                    },
                    expr,
                )
            }

            _ if call.is_analysis_var() && !self.parent.ctx.allow_analysis_fun() => self
                .report_illegal_access(
                    IllegalCtxAccessKind::AnalysisFun {
                        name: name.as_ref().and_then(|p| p.as_ident()).unwrap(),
                    },
                    expr,
                ),

            (BuiltIn::potential | BuiltIn::flow, Some(_))
                if !self.parent.ctx.allow_nature_access() =>
            {
                self.report_illegal_access(IllegalCtxAccessKind::NatureAccess, expr)
            }

            (BuiltIn::potential | BuiltIn::flow, Some(NATURE_ACCESS_NODES)) => {
                let hi = self.parent.infer.expr_types[args[0]].unwrap_node();
                let lo = self.parent.infer.expr_types[args[1]].unwrap_node();
                self.validate_node_direction_for_access(expr, hi, Some(lo), None);
            }

            (BuiltIn::potential | BuiltIn::flow, Some(NATURE_ACCESS_NODE_GND)) => {
                let node = self.parent.infer.expr_types[args[0]].unwrap_node();
                self.validate_node_direction_for_access(expr, node, None, None);
            }

            (BuiltIn::flow, Some(NATURE_ACCESS_PORT_FLOW)) => {
                let node = self.parent.infer.expr_types[args[0]].unwrap_port_flow();
                let node_data = self.parent.db.node_data(node);
                if !(node_data.is_input | node_data.is_output) {
                    self.report(BodyValidationDiagnostic::ExpectedPort { node, expr })
                }
            }

            (BuiltIn::port_connected, _) => {
                let node = self.parent.infer.expr_types[args[0]].unwrap_node();
                let node_data = self.parent.db.node_data(node);
                if !(node_data.is_input | node_data.is_output) {
                    self.report(BodyValidationDiagnostic::ExpectedPort { node, expr })
                }
            }

            (BuiltIn::potential, Some(NATURE_ACCESS_PORT_FLOW)) => {
                self.report(BodyValidationDiagnostic::PotentialOfPortFlow { expr, branch: None })
            }

            (BuiltIn::potential | BuiltIn::flow, Some(NATURE_ACCESS_BRANCH)) => {
                let branch = self.parent.infer.expr_types[args[0]].unwrap_branch();

                if let Some(branch_info) = self.parent.db.branch_info(branch) {
                    match branch_info.kind {
                        BranchKind::PortFlow(_) if call == BuiltIn::potential => {
                            self.report(BodyValidationDiagnostic::PotentialOfPortFlow {
                                expr,
                                branch: Some(branch),
                            })
                        }
                        BranchKind::PortFlow(node) if !self.write => {
                            self.validate_node_direction_for_access(expr, node, None, Some(branch))
                        }

                        BranchKind::PortFlow(_) => (),
                        BranchKind::NodeGnd(node) => {
                            self.validate_node_direction_for_access(expr, node, None, Some(branch));
                        }
                        BranchKind::Nodes(hi, lo) => {
                            self.validate_node_direction_for_access(
                                expr,
                                hi,
                                Some(lo),
                                Some(branch),
                            );
                        }
                    }
                }
            }
            (BuiltIn::absdelay, Some(ABSDELAY_MAX))
            | (BuiltIn::transition, Some(TRANSITION_DELAY_RISET_FALLT_TOL))
            | (BuiltIn::ddt, Some(DDT_TOL))
            | (BuiltIn::idt | BuiltIn::idtmod, Some(IDT_IC_ASSERT_TOL)) => {
                if let [other_args @ .., const_expr] = args {
                    // Do not type check const expr twice
                    args = other_args;
                    self.validate_const_expr(*const_expr);
                };
            }

            (
                BuiltIn::laplace_nd
                | BuiltIn::laplace_np
                | BuiltIn::laplace_zp
                | BuiltIn::laplace_zd
                | BuiltIn::zi_nd
                | BuiltIn::zi_np
                | BuiltIn::zi_zd
                | BuiltIn::zi_zp,
                Some(_),
            ) => {
                if let [_expr, const_args @ ..] = args {
                    args = &args[..1];
                    for arg in const_args {
                        self.validate_const_expr(*arg)
                    }
                }
            }

            _ => (),
        }

        for arg in args {
            self.validate_expr(*arg)
        }
    }

    fn validate_const_expr(&mut self, expr: ExprId) {
        let old = replace(&mut self.parent.ctx, BodyCtx::Const);
        let sink = replace(&mut self.cond_diagnostic_sink, None);
        self.validate_expr(expr);
        self.cond_diagnostic_sink = sink;
        self.parent.ctx = old;
    }

    fn validate_node_direction_for_access(
        &mut self,
        expr: ExprId,
        hi: NodeId,
        lo: Option<NodeId>,
        branch: Option<BranchId>,
    ) {
        let mut nodes = ArrayVec::new();
        if self.is_invalid_node_direction_for_access(hi) {
            nodes.push(hi)
        }
        if let Some(lo) = lo {
            if self.is_invalid_node_direction_for_access(lo) {
                nodes.push(lo)
            }
        }

        if !nodes.is_empty() {
            self.report(BodyValidationDiagnostic::InvalidNodeDirectionForAccess {
                nodes,
                branch,
                write: self.write,
                expr,
            })
        }
    }

    fn is_invalid_node_direction_for_access(&self, node: NodeId) -> bool {
        let node = self.parent.db.node_data(node);
        self.write && node.read_only() || !self.write && node.write_only()
    }
}
