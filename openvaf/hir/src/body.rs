use std::sync::Arc;

use hir_def::db::HirDefDB;
use hir_def::DefWithBodyId;
use hir_ty::db::HirTyDB;
use hir_ty::inference;
use hir_ty::types::{Signature, Ty};

pub use hir_def::expr::Event;
pub use hir_def::{expr::CaseCond, BuiltIn, Case, ExprId, Literal, ParamSysFun, StmtId, Type};
pub use syntax::ast::{BinaryOp, UnaryOp};

use crate::{Branch, CompilationDB, Node};
use crate::{BranchWrite, Function, FunctionArg, NatureAttribute, Parameter, Variable};

#[derive(Debug, Clone)]
pub struct Body {
    body: Arc<hir_def::body::Body>,
    infere: Arc<inference::InferenceResult>,
}
impl Body {
    pub(crate) fn new(id: DefWithBodyId, db: &CompilationDB) -> Body {
        Body { body: db.body(id), infere: db.inference_result(id) }
    }

    pub fn borrow(&self) -> BodyRef<'_> {
        BodyRef { body: &self.body, infere: &self.infere }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BodyRef<'a> {
    body: &'a hir_def::body::Body,
    infere: &'a inference::InferenceResult,
}

impl<'a> BodyRef<'a> {
    pub fn entry(&self) -> &'a [StmtId] {
        &self.body.entry_stmts
    }

    /// Returns the type that was inferred for this expression
    pub fn expr_type(&self, expr: ExprId) -> Type {
        self.infere.expr_types[expr].to_value().unwrap()
    }

    /// Returns whether the result of an expression
    /// needs to be cast to a different type before use.
    pub fn needs_cast(&self, expr: ExprId) -> Option<(Type, &'a Type)> {
        let dst = self.infere.casts.get(&expr)?;
        let src = self.expr_type(expr);
        debug_assert_ne!(&src, dst, "cast types must be different");
        Some((src, dst))
    }

    fn resolve_path(&self, expr: ExprId) -> Ref {
        match self.infere.expr_types[expr] {
            Ty::Var(_, id) => Ref::Variable(Variable { id }),
            Ty::Param(_, id) => Ref::Parameter(Parameter { id }),
            Ty::FunctionVar { fun, arg: Some(arg), .. } => {
                Ref::FunctionArg(FunctionArg { fun_id: fun, arg_id: arg })
            }
            Ty::FunctionVar { fun, .. } => Ref::FunctionReturn(Function { id: fun }),
            Ty::NatureAttr(_, id) => Ref::NatureAttr(NatureAttribute { id }),

            ref it => {
                if let Some(&inference::ResolvedFun::Param(param)) =
                    self.infere.resolved_calls.get(&expr)
                {
                    return Ref::ParamSysFun(param);
                }
                panic!("invalid HIR: path {:?} was not resolved {:?}", self.body.exprs[expr], it)
            }
        }
    }

    pub fn get_call_signature(&self, expr: ExprId) -> Signature {
        self.infere.resolved_signatures.get(&expr).copied().unwrap_or(Signature(u32::MAX))
    }

    pub fn as_literal(&self, expr: ExprId) -> Option<&'a Literal> {
        match &self.body.exprs[expr] {
            hir_def::Expr::Literal(lit) => Some(lit),
            _ => None,
        }
    }

    pub fn into_node(&self, expr: ExprId) -> Node {
        let id = self.infere.expr_types[expr].unwrap_node();
        Node { id }
    }

    pub fn into_port_flow(&self, expr: ExprId) -> Node {
        let id = self.infere.expr_types[expr].unwrap_port_flow();
        Node { id }
    }

    pub fn into_parameter(&self, expr: ExprId) -> Parameter {
        let id = self.infere.expr_types[expr].unwrap_param();
        Parameter { id }
    }

    pub fn into_branch(&self, expr: ExprId) -> Branch {
        let id = self.infere.expr_types[expr].unwrap_branch();
        Branch { id }
    }

    pub fn get_expr(&self, expr: ExprId) -> Expr<'a> {
        match self.body.exprs[expr] {
            hir_def::Expr::Path { .. } => Expr::Read(self.resolve_path(expr)),
            hir_def::Expr::BinaryOp { lhs, rhs, op: Some(op) } => Expr::BinaryOp { lhs, rhs, op },
            hir_def::Expr::UnaryOp { expr, op } => Expr::UnaryOp { expr, op },
            hir_def::Expr::Select { cond, then_val, else_val } => {
                Expr::Select { cond, then_val, else_val }
            }
            hir_def::Expr::Call { ref args, .. } => {
                let fun = match self.infere.resolved_calls[&expr] {
                    inference::ResolvedFun::User { func, limit } => {
                        ResolvedFun::User { func: Function { id: func }, limit }
                    }
                    inference::ResolvedFun::BuiltIn(builtin) => ResolvedFun::BuiltIn(builtin),
                    // this is a special case, the VAMS standard allows these parameters
                    // to be called like functions (but its the same as direct access)
                    // we hide that detail from downstream users here
                    inference::ResolvedFun::Param(param) => {
                        return Expr::Read(Ref::ParamSysFun(param))
                    }
                    inference::ResolvedFun::InvalidNatureAccess(_) => {
                        panic!("invalid HIR: invalid nature access {:?}", self.body.exprs[expr])
                    }
                };
                Expr::Call { fun, args }
            }
            hir_def::Expr::Array(ref args) => Expr::Array(args),
            hir_def::Expr::Literal(ref literal) => Expr::Literal(literal),
            _ => panic!("invalid HIR: {:?}", self.body.exprs[expr]),
        }
    }

    pub fn get_entry_stmt(&self, i: usize) -> Option<Stmt<'a>> {
        self.get_stmt(self.entry()[i])
    }

    pub fn get_entry_expr(&self, i: usize) -> ExprId {
        self.get_stmt(self.entry()[i]).unwrap().unwrap_expr()
    }

    pub fn get_stmt(&self, stmnt: StmtId) -> Option<Stmt<'a>> {
        match self.body.stmts[stmnt] {
            hir_def::Stmt::Empty | hir_def::Stmt::Missing => None,
            hir_def::Stmt::Expr(e) => Some(Stmt::Expr(e)),
            hir_def::Stmt::EventControl { ref event, body } => {
                Some(Stmt::EventControl { event, body })
            }
            hir_def::Stmt::Assignment { val, .. } => {
                let stmt = match self.infere.assignment_destination[&stmnt] {
                    inference::AssignDst::Var(id) => {
                        Stmt::Assignment { lhs: AssignmentLhs::Variable(Variable { id }), rhs: val }
                    }
                    inference::AssignDst::FunVar { fun, arg: None } => Stmt::Assignment {
                        lhs: AssignmentLhs::FunctionReturn(Function { id: fun }),
                        rhs: val,
                    },
                    inference::AssignDst::FunVar { fun, arg: Some(arg) } => Stmt::Assignment {
                        lhs: AssignmentLhs::FunctionArg(FunctionArg { fun_id: fun, arg_id: arg }),
                        rhs: val,
                    },
                    inference::AssignDst::Flow(branch) => Stmt::Contribute {
                        kind: ContributeKind::Flow,
                        branch: branch.into(),
                        rhs: val,
                    },
                    inference::AssignDst::Potential(branch) => Stmt::Contribute {
                        kind: ContributeKind::Potential,
                        branch: branch.into(),
                        rhs: val,
                    },
                };
                Some(stmt)
            }
            hir_def::Stmt::Block { ref body } => Some(Stmt::Block { body }),
            hir_def::Stmt::If { cond, then_branch, else_branch } => {
                Some(Stmt::If { cond, then_branch, else_branch })
            }
            hir_def::Stmt::ForLoop { init, cond, incr, body } => {
                Some(Stmt::ForLoop { init, cond, incr, body })
            }
            hir_def::Stmt::WhileLoop { cond, body } => Some(Stmt::WhileLoop { cond, body }),
            hir_def::Stmt::Case { discr, ref case_arms } => Some(Stmt::Case { discr, case_arms }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum AssignmentLhs {
    Variable(Variable),
    FunctionReturn(Function),
    FunctionArg(FunctionArg),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ContributeKind {
    Flow,
    Potential,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt<'a> {
    Expr(ExprId),
    EventControl { event: &'a Event, body: StmtId },
    Contribute { kind: ContributeKind, branch: BranchWrite, rhs: ExprId },
    Assignment { lhs: AssignmentLhs, rhs: ExprId },
    Block { body: &'a [StmtId] },
    If { cond: ExprId, then_branch: StmtId, else_branch: StmtId },
    ForLoop { init: StmtId, cond: ExprId, incr: StmtId, body: StmtId },
    WhileLoop { cond: ExprId, body: StmtId },
    Case { discr: ExprId, case_arms: &'a [Case] }, // TODO lint on unreachable
}
impl Stmt<'_> {
    #[inline]
    pub fn unwrap_expr(&self) -> ExprId {
        if let Stmt::Expr(e) = self {
            *e
        } else {
            unreachable!("Called unwrap_expr on {:?}", self)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr<'a> {
    Read(Ref),
    BinaryOp { lhs: ExprId, rhs: ExprId, op: BinaryOp },
    UnaryOp { expr: ExprId, op: UnaryOp },
    Select { cond: ExprId, then_val: ExprId, else_val: ExprId },
    Call { fun: ResolvedFun, args: &'a [ExprId] },
    Array(&'a [ExprId]),
    Literal(&'a Literal),
}
impl Expr<'_> {
    pub fn is_zero(&self) -> bool {
        if let Expr::Literal(lit) = self {
            lit.is_zero()
        } else {
            false
        }
    }

    pub fn as_assignment_lhs(&self) -> AssignmentLhs {
        match *self {
            Expr::Read(Ref::Variable(var)) => AssignmentLhs::Variable(var),
            Expr::Read(Ref::FunctionArg(arg)) => AssignmentLhs::FunctionArg(arg),
            Expr::Read(Ref::FunctionReturn(fun)) => AssignmentLhs::FunctionReturn(fun),
            _ => panic!("{self:?} is not a lhs reference"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ref {
    Variable(Variable),
    Parameter(Parameter),
    FunctionArg(FunctionArg),
    FunctionReturn(Function),
    NatureAttr(NatureAttribute),
    ParamSysFun(ParamSysFun),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ResolvedFun {
    User { func: Function, limit: bool },
    BuiltIn(BuiltIn),
}
