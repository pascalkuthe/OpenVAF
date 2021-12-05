use std::mem::replace;

use arrayvec::ArrayVec;
use hir_def::{body::Body, DefWithBodyId};
use hir_def::{BranchId, BuiltIn, Expr, ExprId, NodeId, Path, Stmt, StmtId};
use syntax::ast::AssignOp;

use crate::builtin::{NATURE_ACCESS_BRANCH, NATURE_ACCESS_NODES, NATURE_ACCESS_PORT_FLOW};
use crate::db::HirTyDB;
use crate::inference::{AssignDst, BranchWrite, InferenceResult, ResolvedFun};
use crate::lower::BranchKind;
use crate::types::{Signature, Ty};
use crate::validation::types::IncompatibleNodes;

pub enum BodyValidationDiagnostic {
    PotentialOfPortFlow {
        expr: ExprId,
        branch: Option<BranchId>,
    },
    IncompatibleNodes {
        expr: ExprId,
        err: IncompatibleNodes,
    },
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
}

impl BodyValidationDiagnostic {
    pub fn collect(db: &dyn HirTyDB, def: DefWithBodyId) -> Vec<BodyValidationDiagnostic> {
        // let body= db.body
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum BodyCtx {
    AnalogBlock,
    Conditional,
    EventControl,
    Function,
    Const,
}

impl BodyCtx {
    pub fn allow_nature_access(&self) -> bool {
        matches!(self, Self::AnalogBlock | Self::Conditional | Self::EventControl)
    }

    pub fn allow_contribute(&self) -> bool {
        matches!(self, Self::AnalogBlock | Self::Conditional)
    }
}

struct BodyValidator<'a> {
    db: &'a dyn HirTyDB,
    owner: DefWithBodyId,
    body: &'a Body,
    infer: &'a InferenceResult,
    diagnostics: Vec<BodyValidationDiagnostic>,
    ctx: BodyCtx,
}

impl BodyValidator<'_> {
    fn validate_stmt_with_ctx(&mut self, stmt: StmtId, ctx: BodyCtx) {
        let old = replace(&mut self.ctx, ctx);
        self.validate_stmt(stmt);
        self.ctx = old
    }

    fn validate_conditional_stmt(&mut self, stmt: StmtId) {
        if self.ctx == BodyCtx::AnalogBlock {
            self.ctx = BodyCtx::Conditional;
            self.validate_stmt(stmt);
            self.ctx = BodyCtx::AnalogBlock;
        } else {
            self.validate_stmt(stmt)
        }
    }

    fn validate_stmt(&mut self, stmt: StmtId) {
        match self.body.stmts[stmt] {
            Stmt::Assigment { dst, val, assignment_kind } => {
                self.validate_expr(val, false);

                if assignment_kind == AssignOp::Contribute && (self.ctx.allow_contribute()) {
                    self.diagnostics
                        .push(BodyValidationDiagnostic::IllegalContribute { stmt, ctx: self.ctx })
                }
                // avoid duplicate errors
                else if self.infer.assigment_destination.contains_key(&stmt) {
                    self.validate_expr(dst, true);
                }
            }
            Stmt::EventControl { body, .. } => {
                self.validate_stmt_with_ctx(body, BodyCtx::EventControl);
            }
            Stmt::Block { ref body } => body.iter().for_each(|stmt| self.validate_stmt(*stmt)),

            _ => {
                self.body.stmts[stmt].walk_child_exprs(|e| self.validate_expr(e, false));
                self.body.stmts[stmt].walk_child_stmts(|s| self.validate_conditional_stmt(s));
            }
        }
    }

    fn validate_expr(&mut self, expr: ExprId, write: bool) {
        match self.body.exprs[expr] {
            Expr::Call { ref args, .. } => {
                if let (Some(call), Some(signature)) = (
                    self.infer.resolved_calls.get(&expr),
                    self.infer.resolved_signatures.get(&expr),
                ) {
                    match call {
                        ResolvedFun::BuiltIn(builtin) => {
                            self.validate_builtin(expr, args, *builtin, *signature, write)
                        }
                        ResolvedFun::User(fun) => {
                            // TODO anything to validate here?
                        }
                    }
                }
            }

            Expr::Path { path, port: false } => {
                if let Ty::FuntionVar { arg: Some(arg), fun, .. } = self.infer.expr_types[expr] {
                    let arg = self.db.function_data(fun).args[arg];
                    if write && !arg.is_output || !write && !arg.is_input {
                        self.diagnostics.push(
                            BodyValidationDiagnostic::InvalidArgDirectionForAccess { expr, write },
                        )
                    }
                }
            }

            _ => (),
        }

        self.body.exprs[expr].walk_child_exprs(|child| self.validate_expr(child, false))
    }

    fn validate_builtin(
        &mut self,
        expr: ExprId,
        args: &[ExprId],
        call: BuiltIn,
        signature: Signature,
        write: bool,
    ) {
        match (call, signature) {
            (BuiltIn::potential | BuiltIn::flow, _) if self.ctx.allow_nature_access() => self
                .diagnostics
                .push(BodyValidationDiagnostic::IllegalNatureAccess { expr, ctx: self.ctx }),

            (BuiltIn::potential | BuiltIn::flow, NATURE_ACCESS_NODES) => {
                let hi = self.infer.expr_types[args[0]].unwrap_node();
                let lo = self.infer.expr_types[args[1]].unwrap_node();
                self.validate_node_direction_for_access(expr, hi, Some(lo), write, None);
                todo!("check net compatability")
            }

            (BuiltIn::potential | BuiltIn::flow, NATURE_ACCESS_NODES) => {
                let hi = self.infer.expr_types[args[0]].unwrap_node();
                let lo = self.infer.expr_types[args[1]].unwrap_node();
                self.validate_node_direction_for_access(expr, hi, Some(lo), write, None);
                todo!("check net compatability")
            }

            (BuiltIn::potential, NATURE_ACCESS_PORT_FLOW) => self
                .diagnostics
                .push(BodyValidationDiagnostic::PotentialOfPortFlow { expr, branch: None }),

            (BuiltIn::potential | BuiltIn::flow, NATURE_ACCESS_BRANCH) => {
                let branch = self.infer.expr_types[args[0]].unwrap_branch();

                if let Some(branch_info) = self.db.branch_info(branch) {
                    match branch_info.kind {
                        BranchKind::PortFlow(_) if call == BuiltIn::potential => {
                            self.diagnostics.push(BodyValidationDiagnostic::PotentialOfPortFlow {
                                expr,
                                branch: Some(branch),
                            })
                        }
                        BranchKind::PortFlow(node) if !write => self
                            .validate_node_direction_for_access(
                                expr,
                                node,
                                None,
                                write,
                                Some(branch),
                            ),

                        BranchKind::PortFlow(_) => (),
                        BranchKind::NodeGnd(node) => {
                            self.validate_node_direction_for_access(
                                expr,
                                node,
                                None,
                                write,
                                Some(branch),
                            );
                        }
                        BranchKind::Nodes(hi, lo) => {
                            self.validate_node_direction_for_access(
                                expr,
                                hi,
                                Some(lo),
                                write,
                                Some(branch),
                            );
                        }

                        _ => (),
                    }
                }
            }
            // TODO anything else?
            _ => (),
        }
    }

    fn validate_node_direction_for_access(
        &mut self,
        expr: ExprId,
        hi: NodeId,
        lo: Option<NodeId>,
        write: bool,
        branch: Option<BranchId>,
    ) {
        let mut nodes = ArrayVec::new();
        if self.is_invalid_node_direction_for_access(hi, write) {
            nodes.push(hi)
        }
        if let Some(lo) = lo {
            if self.is_invalid_node_direction_for_access(lo, write) {
                nodes.push(lo)
            }
        }

        self.diagnostics.push(BodyValidationDiagnostic::InvalidNodeDirectionForAccess {
            nodes,
            branch,
            write,
            expr,
        })
    }

    fn is_invalid_node_direction_for_access(&self, node: NodeId, write: bool) -> bool {
        let node = self.db.node_data(node);
        write && node.read_only() || !write && node.write_only()
    }
}
