use std::borrow::Cow;
use std::mem;
use std::sync::Arc;

use ahash::AHashMap;
use arena::ArenaMap;
use hir_def::body::Body;
use hir_def::db::HirDefDB;
use hir_def::expr::{CaseCond, Literal};
use hir_def::nameres::diagnostics::PathResolveError;
use hir_def::nameres::{NatureAccess, ResolvedPath, ScopeDefItem, ScopeDefItemKind};
use hir_def::{
    BranchId, BuiltIn, DefWithBodyId, Expr, ExprId, FunctionArgLoc, FunctionId, LocalFunctionArgId,
    Lookup, NatureId, NodeId, ParamSysFun, Path, Stmt, StmtId, Type, VarId,
};
use stdx::impl_from;
use stdx::iter::zip;
use syntax::ast::{self, BinaryOp, UnaryOp};
use syntax::{TextRange, TextSize};
use typed_index_collections::{TiSlice, TiVec};

use crate::builtin::{
    DDX_FLOW, DDX_POT, DDX_POT_DIFF, DDX_TEMP, LIMIT_BUILTIN_FUNCTION, LIMIT_USER_FUNCTION,
    NATURE_ACCESS_BRANCH, NATURE_ACCESS_NODES, NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW,
};
use crate::db::{Alias, HirTyDB};
use crate::diagnostics::{ArrayTypeMismatch, SignatureMismatch, TypeMismatch};
use crate::inference::fmt_parser::parse_real_fmt_spec;
use crate::lower::{BranchKind, BranchTy, DisciplineAccess};
use crate::types::{default_return_ty, BuiltinInfo, Signature, SignatureData, Ty, TyRequirement};

mod fmt_parser;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ResolvedFun {
    User { func: FunctionId, limit: bool },
    BuiltIn(BuiltIn),
    Param(ParamSysFun),
    InvalidNatureAccess(NatureId),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum AssignDst {
    Var(VarId),
    FunVar { fun: FunctionId, arg: Option<LocalFunctionArgId> },
    Flow(BranchWrite),
    Potential(BranchWrite),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum BranchWrite {
    Named(BranchId),
    Unnamed { hi: NodeId, lo: Option<NodeId> },
}

impl BranchWrite {
    pub fn nodes(self, db: &dyn HirTyDB) -> (NodeId, Option<NodeId>) {
        match self {
            BranchWrite::Named(branch) => match db.branch_info(branch).unwrap().kind {
                BranchKind::Nodes(hi, lo) => (hi, Some(lo)),
                BranchKind::NodeGnd(hi) => (hi, None),
                BranchKind::PortFlow(_) => unreachable!(),
            },
            BranchWrite::Unnamed { hi, lo } => (hi, lo),
        }
    }
}

impl AssignDst {
    pub fn ty(&self, db: &dyn HirDefDB) -> Type {
        if let AssignDst::Var(var) = *self {
            let var = var.lookup(db);
            let tree = var.item_tree(db);
            tree[var.id].ty.clone()
        } else {
            Type::Real
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct InferenceResult {
    pub expr_types: ArenaMap<Expr, Ty>,
    pub resolved_calls: AHashMap<ExprId, ResolvedFun>,
    pub resolved_signatures: AHashMap<ExprId, Signature>,
    pub assignment_destination: AHashMap<StmtId, AssignDst>,
    pub casts: AHashMap<ExprId, Type>,
    pub diagnostics: Vec<InferenceDiagnostic>,
}

impl InferenceResult {
    pub fn infere_body_query(db: &dyn HirTyDB, id: DefWithBodyId) -> Arc<InferenceResult> {
        let body = db.body(id);
        let result = InferenceResult {
            expr_types: ArenaMap::from(vec![Ty::Val(Type::Err); body.exprs.len()]),
            ..Default::default()
        };

        let mut ctx = Ctx { result, body: &body, db, expr_stmt_ty: None };
        ctx.expr_stmt_ty = match id {
            DefWithBodyId::ParamId(param) => match &db.param_data(param).ty {
                Some(ty) => Some(ty.clone()),
                // parameter type is inferred if omitted
                None => ctx
                    .infere_expr(body.entry_stmts[0], db.param_exprs(param).default)
                    .and_then(|ty| ty.to_value()),
            },
            DefWithBodyId::VarId(var) => Some(db.var_data(var).ty.clone()),
            _ => None,
        };

        for stmt in &*body.entry_stmts {
            ctx.infere_stmt(*stmt);
        }

        Arc::new(ctx.result)
    }
}

struct Ctx<'a> {
    result: InferenceResult,
    body: &'a Body,
    db: &'a dyn HirTyDB,
    /// A Body that only represent expressions have expr stmts as entry_stmts.
    /// These need to be type checked properly.
    /// For behavioural (anlog body and function) and untype (nature attr)
    /// bodys this is simply none
    expr_stmt_ty: Option<Type>,
}

impl Ctx<'_> {
    pub fn infere_stmt(&mut self, stmt: StmtId) {
        match self.body.stmts[stmt] {
            Stmt::Expr(expr) => {
                // TODO lint for side effect free expressions
                self.infere_assignment(stmt, expr, self.expr_stmt_ty.clone());
            }
            Stmt::Assignment { dst, val, assignment_kind } => {
                let dst_ty = self.infere_assignment_dst(stmt, dst, assignment_kind);
                self.infere_assignment(stmt, val, dst_ty);
            }
            Stmt::ForLoop { cond, .. } | Stmt::If { cond, .. } | Stmt::WhileLoop { cond, .. } => {
                self.infere_cond(stmt, cond)
            }

            Stmt::Case { discr, ref case_arms } => {
                if let Some(ty) = self.infere_expr(stmt, discr) {
                    let req = ty.to_value().map_or(TyRequirement::AnyVal, TyRequirement::Val);
                    for case in case_arms {
                        if let CaseCond::Vals(vals) = &case.cond {
                            for val in vals {
                                if let Some(val_ty) = self.infere_expr(stmt, *val) {
                                    self.expect::<false>(
                                        *val,
                                        None,
                                        val_ty,
                                        Cow::Owned(vec![req.clone()]),
                                    );
                                }
                            }
                        }
                    }
                }
            }
            _ => (),
        };

        self.body.stmts[stmt].walk_child_stmts(|stmt| self.infere_stmt(stmt));
    }

    fn infere_assignment(&mut self, stmt: StmtId, val: ExprId, dst_ty: Option<Type>) {
        if let Some(val_ty) = self.infere_expr(stmt, val) {
            if let Some(value_ty) = val_ty.to_value() {
                if let Some(dst_ty) = dst_ty {
                    if dst_ty.is_assignable_to(&value_ty) {
                        if dst_ty != value_ty {
                            self.result.casts.insert(val, dst_ty);
                        }
                    } else {
                        self.result.diagnostics.push(
                            TypeMismatch {
                                expected: Cow::Owned(vec![TyRequirement::Val(dst_ty)]),
                                found_ty: val_ty,
                                expr: val,
                            }
                            .into(),
                        );
                    }
                }
            } else {
                let expected = dst_ty.map_or(TyRequirement::AnyVal, TyRequirement::Val);
                self.result.diagnostics.push(
                    TypeMismatch {
                        expected: Cow::Owned(vec![expected]),
                        found_ty: val_ty,
                        expr: val,
                    }
                    .into(),
                );
            }
        }
    }

    pub fn infere_assignment_dst(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        assignment_kind: ast::AssignOp,
    ) -> Option<Type> {
        let e = self.infere_expr(stmt, expr);

        let (dst, ty) = match e? {
            Ty::Var(ty, var) => (AssignDst::Var(var), ty),
            Ty::FunctionVar { fun, ty, arg } => (AssignDst::FunVar { fun, arg }, ty),
            Ty::Val(Type::Real)
                if matches!(
                    self.result.resolved_calls.get(&expr),
                    Some(ResolvedFun::BuiltIn(BuiltIn::potential | BuiltIn::flow))
                ) =>
            {
                let mut args = Vec::new();
                self.body.exprs[expr].walk_child_exprs(|e| args.push(&self.result.expr_types[e]));
                let kind = match *self.result.resolved_signatures.get(&expr)? {
                    NATURE_ACCESS_BRANCH => BranchWrite::Named(args[0].unwrap_branch()),
                    NATURE_ACCESS_NODES => BranchWrite::Unnamed {
                        hi: args[0].unwrap_node(),
                        lo: Some(args[1].unwrap_node()),
                    },

                    NATURE_ACCESS_NODE_GND => {
                        BranchWrite::Unnamed { hi: args[0].unwrap_node(), lo: None }
                    }

                    NATURE_ACCESS_PORT_FLOW => {
                        self.result.diagnostics.push(InferenceDiagnostic::InvalidAssignDst {
                            e: expr,
                            maybe_different_operand: None,
                            assignment_kind,
                        });
                        return None;
                    }
                    _ => unreachable!(),
                };

                let dst = match self.result.resolved_calls[&expr] {
                    ResolvedFun::BuiltIn(BuiltIn::potential) => AssignDst::Potential(kind),
                    ResolvedFun::BuiltIn(BuiltIn::flow) => AssignDst::Flow(kind),
                    _ => unreachable!(),
                };
                (dst, Type::Real)
            }
            _ => {
                self.result.diagnostics.push(InferenceDiagnostic::InvalidAssignDst {
                    e: expr,
                    maybe_different_operand: None,
                    assignment_kind,
                });
                return None;
            }
        };

        // check that the correct operator is used
        match (&dst, assignment_kind) {
            (AssignDst::Var(_) | AssignDst::FunVar { .. }, ast::AssignOp::Contribute) => {
                self.result.diagnostics.push(InferenceDiagnostic::InvalidAssignDst {
                    e: expr,
                    maybe_different_operand: Some(ast::AssignOp::Assign),
                    assignment_kind,
                });
            }
            (AssignDst::Flow(_) | AssignDst::Potential(_), ast::AssignOp::Assign) => {
                self.result.diagnostics.push(InferenceDiagnostic::InvalidAssignDst {
                    e: expr,
                    maybe_different_operand: Some(ast::AssignOp::Contribute),
                    assignment_kind,
                });
            }
            _ => {
                self.result.assignment_destination.insert(stmt, dst);
            }
        }
        Some(ty)
    }

    fn infere_cond(&mut self, stmt: StmtId, expr: ExprId) {
        if let Some(ty) = self.infere_expr(stmt, expr) {
            self.expect::<false>(expr, None, ty, Cow::Borrowed(&[TyRequirement::Condition]));
        }
    }

    fn infere_expr(&mut self, stmt: StmtId, expr: ExprId) -> Option<Ty> {
        let ty = match self.body.exprs[expr] {
            Expr::Missing => return None,
            Expr::Path { ref path, port: true } => {
                let port = self.resolve_item_path(stmt, expr, path)?;
                Ty::PortFlow(port)
            }

            Expr::Path { ref path, port: false } => match self.resolve_path(stmt, expr, path)? {
                ScopeDefItem::BlockId(_) | ScopeDefItem::ModuleId(_) => Ty::Scope,
                ScopeDefItem::NatureId(nature) => Ty::Nature(nature),
                ScopeDefItem::DisciplineId(discipline) => Ty::Discipline(discipline),
                ScopeDefItem::NodeId(node) => Ty::Node(node),
                ScopeDefItem::VarId(var) => Ty::Var(self.db.var_data(var).ty.clone(), var),
                ScopeDefItem::ParamId(param) => Ty::Param(self.db.param_ty(param), param),
                ScopeDefItem::AliasParamId(param) => match self.db.resolve_alias(param)? {
                    Alias::Cycel => return None,
                    Alias::Param(param) => Ty::Param(self.db.param_ty(param), param),
                    Alias::ParamSysFun(param) => {
                        self.result.resolved_calls.insert(expr, ResolvedFun::Param(param));
                        Ty::Val(Type::Real)
                    }
                },
                ScopeDefItem::BranchId(branch) => Ty::Branch(branch),
                ScopeDefItem::BuiltIn(_) | ScopeDefItem::NatureAccess(_) => Ty::BuiltInFunction,

                ScopeDefItem::FunctionId(fun) => Ty::UserFunction(fun),
                ScopeDefItem::FunctionReturn(fun) => Ty::FunctionVar {
                    fun,
                    ty: self.db.function_data(fun).return_ty.clone(),
                    arg: None,
                },
                ScopeDefItem::FunctionArgId(arg) => {
                    let FunctionArgLoc { fun, id } = arg.lookup(self.db.upcast());
                    Ty::FunctionVar {
                        fun,
                        ty: self.db.function_data(fun).args[id].ty.clone(),
                        arg: Some(id),
                    }
                }
                ScopeDefItem::NatureAttrId(attr) => {
                    Ty::NatureAttr(self.db.nature_attr_ty(attr)?, attr)
                }
                ScopeDefItem::ParamSysFun(_) => Ty::Val(Type::Real),
            },

            Expr::BinaryOp { op: None, lhs, rhs } => {
                self.infere_expr(stmt, lhs);
                self.infere_expr(stmt, rhs);
                return None;
            }

            Expr::BinaryOp { lhs, rhs, op: Some(op) } => {
                self.infere_bin_op(stmt, expr, lhs, rhs, op)?
            }

            Expr::UnaryOp { expr: arg, op: UnaryOp::Identity } => self.infere_expr(stmt, arg)?,
            Expr::UnaryOp { expr: arg, op: UnaryOp::Neg } => {
                let ty = self.infere_expr(stmt, arg)?;
                let variant = self.expect::<false>(
                    arg,
                    Some(expr),
                    ty,
                    Cow::Borrowed(&[
                        TyRequirement::Val(Type::Integer),
                        TyRequirement::Val(Type::Real),
                    ]),
                )?;
                let ty = match variant {
                    0 => Type::Integer,
                    1 => Type::Real,
                    _ => unreachable!(),
                };
                Ty::Val(ty)
            }

            Expr::UnaryOp { expr: arg, op: UnaryOp::BitNegate } => {
                let ty = self.infere_expr(stmt, arg)?;
                // TODO bool
                self.expect::<false>(
                    arg,
                    Some(expr),
                    ty,
                    Cow::Borrowed(&[TyRequirement::Val(Type::Integer)]),
                );
                Ty::Val(Type::Integer)
            }

            Expr::UnaryOp { expr: arg, op: UnaryOp::Not } => {
                let ty = self.infere_expr(stmt, arg)?;
                self.expect::<false>(
                    arg,
                    Some(expr),
                    ty,
                    Cow::Borrowed(&[TyRequirement::Condition]),
                );
                Ty::Val(Type::Bool)
            }

            Expr::Select { cond, then_val, else_val } => {
                self.infere_cond(stmt, cond);
                self.resolve_function_args(
                    stmt,
                    expr,
                    &[then_val, else_val],
                    Cow::Borrowed(TiSlice::from_ref(SignatureData::SELECT)),
                    None,
                )
                .0?
            }

            Expr::Call { ref fun, ref args } => {
                self.infere_fun_call(stmt, expr, fun.as_ref()?, args)?
            }
            Expr::Array(ref args) if args.is_empty() => Ty::Val(Type::EmptyArray),
            Expr::Array(ref args) => self.infere_array(stmt, args)?,
            Expr::Literal(Literal::Float(_)) => Ty::Literal(Type::Real),
            Expr::Literal(Literal::Int(_)) => Ty::Literal(Type::Integer),
            // +/- inf can only appear in param bounds.
            // This is checked during ast validation and when it appears it is always correct
            Expr::Literal(Literal::Inf) => {
                if let Some(ty) = &self.expr_stmt_ty {
                    self.result.expr_types[expr] = Ty::Val(ty.clone());
                }
                return None;
            }
            Expr::Literal(Literal::String(_)) => Ty::Literal(Type::String),
        };

        self.result.expr_types[expr] = ty.clone();

        Some(ty)
    }

    fn infere_fun_call(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        fun: &Path,
        args: &[ExprId],
    ) -> Option<Ty> {
        let def = self.resolve_path(stmt, expr, fun)?;
        match def {
            ScopeDefItem::NatureAccess(access) => {
                self.infere_nature_access(stmt, expr, access, args);
                Some(Ty::Val(Type::Real))
            }
            ScopeDefItem::FunctionId(fun) => self.infere_user_fun_call(stmt, expr, fun, args),
            ScopeDefItem::BuiltIn(builtin) => {
                self.result.resolved_calls.insert(expr, ResolvedFun::BuiltIn(builtin));
                self.infere_builtin(stmt, expr, builtin, args).0
            }
            ScopeDefItem::ParamSysFun(param) => {
                self.result.resolved_calls.insert(expr, ResolvedFun::Param(param));
                if !args.is_empty() {
                    let err = InferenceDiagnostic::ArgCntMismatch {
                        expected: 0,
                        found: args.len(),
                        expr,
                        exact: true,
                    };
                    self.result.diagnostics.push(err);
                }
                Some(Ty::Val(Type::Real))
            }
            found => {
                self.result.diagnostics.push(InferenceDiagnostic::PathResolveError {
                    err: PathResolveError::ExpectedItemKind {
                        expected: "a function",
                        found: ResolvedPath::ScopeDefItem(found),
                        name: fun.segments.last().unwrap().to_owned(),
                    },
                    expr,
                });
                None
            }
        }
    }

    fn infere_user_fun_call(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        func: FunctionId,
        args: &[ExprId],
    ) -> Option<Ty> {
        self.result.resolved_calls.insert(expr, ResolvedFun::User { func, limit: false });
        let fun_info = self.db.function_data(func);
        if fun_info.args.len() != args.len() {
            self.result.diagnostics.push(InferenceDiagnostic::ArgCntMismatch {
                expected: fun_info.args.len(),
                found: args.len(),
                expr,
                exact: true,
            });
            return Some(Ty::Val(fun_info.return_ty.clone()));
        }

        let signature = fun_info
            .args
            .iter()
            .map(|arg| {
                if arg.is_output {
                    // Output arguments must be variables
                    TyRequirement::Var(arg.ty.clone())
                } else {
                    TyRequirement::Val(arg.ty.clone())
                }
            })
            .collect();

        self.resolve_function_args(
            stmt,
            expr,
            args,
            Cow::Owned(TiVec::from(vec![SignatureData {
                args: Cow::Owned(signature),
                return_ty: fun_info.return_ty.clone(),
            }])),
            Some(func),
        )
        .0
    }

    fn infere_nature_access(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        access: NatureAccess,
        args: &[ExprId],
    ) {
        // resolve as flow first because we don't yet know if this is a flow or pot access
        // This choice is arbitrary (but must be consistent with the code below
        if !self.infere_builtin(stmt, expr, BuiltIn::flow, args).1 {
            return;
        }

        let nature = access.0.lookup(self.db.upcast()).nature;
        // Now that we know that the arguments are valid actually resolve whether this is flow or
        // pot access
        let access = self.infere_access_kind(nature, expr, args[0]);

        // update resolved_calls in case this is actually a pot and not a flow access
        match access {
            Some(DisciplineAccess::Potential) => {
                self.result.resolved_calls.insert(expr, ResolvedFun::BuiltIn(BuiltIn::potential));
            }

            Some(DisciplineAccess::Flow) => {
                self.result.resolved_calls.insert(expr, ResolvedFun::BuiltIn(BuiltIn::flow));
            }

            None => {
                self.result.resolved_calls.insert(expr, ResolvedFun::InvalidNatureAccess(nature));
            }
        }
    }

    fn infere_access_kind(
        &self,
        nature: NatureId,
        expr: ExprId,
        arg: ExprId,
    ) -> Option<DisciplineAccess> {
        let signature = self.result.resolved_signatures.get(&expr);
        let node = match *signature? {
            NATURE_ACCESS_BRANCH => {
                let branch = self.result.expr_types[arg].unwrap_branch();
                let branch_info = self.db.branch_info(branch)?;
                return branch_info.access(nature, self.db);
            }

            NATURE_ACCESS_NODES | NATURE_ACCESS_NODE_GND => {
                self.result.expr_types[arg].unwrap_node()
            }

            NATURE_ACCESS_PORT_FLOW => self.result.expr_types[arg].unwrap_port_flow(),
            var => unreachable!("{:?}", var),
        };

        let discipline = self.db.node_discipline(node)?;
        self.db.discipline_info(discipline).access(nature, self.db)
    }

    fn infere_builtin(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        builtin: BuiltIn,
        args: &[ExprId],
    ) -> (Option<Ty>, bool) {
        let info: BuiltinInfo = builtin.into();

        let exact = Some(info.min_args) == info.max_args;
        if args.len() < info.min_args {
            let err = InferenceDiagnostic::ArgCntMismatch {
                expected: info.min_args,
                found: args.len(),
                expr,
                exact,
            };
            self.result.diagnostics.push(err);
            return (default_return_ty(info.signatures), false);
        }

        if info.max_args.map_or(false, |max_args| max_args < args.len()) {
            self.result.diagnostics.push(InferenceDiagnostic::ArgCntMismatch {
                expected: info.min_args,
                found: args.len(),
                expr,
                exact,
            });
            return (default_return_ty(info.signatures), false);
        }

        let mut infere_args = args;
        let signatures = match builtin {
            BuiltIn::ddx => {
                self.infere_ddx(stmt, expr, args[0], args[1]);
                return (Some(Ty::Val(Type::Real)), true);
            }

            BuiltIn::limit => {
                infere_args = &args[0..2];
                Cow::Borrowed(TiSlice::from_ref(info.signatures))
            }

            _ if info.max_args.is_none() => {
                let mut signatures = Vec::from(info.signatures);
                for sig in &mut signatures {
                    sig.args.to_mut().resize(args.len(), TyRequirement::AnyVal)
                }
                Cow::Owned(TiVec::from(signatures))
            }
            _ => Cow::Borrowed(TiSlice::from_ref(info.signatures)),
        };

        debug_assert_ne!(&signatures.raw, &[]);

        let (ty, valid) = if let (Some(ty), valid) =
            self.resolve_function_args(stmt, expr, infere_args, signatures, None)
        {
            (ty, valid)
        } else {
            return (default_return_ty(info.signatures), false);
        };

        match builtin {
            BuiltIn::limit => self.infere_limit(stmt, expr, args),
            BuiltIn::write
            | BuiltIn::display
            | BuiltIn::strobe
            | BuiltIn::monitor
            | BuiltIn::debug
            | BuiltIn::warning
            | BuiltIn::error
            | BuiltIn::info
            | BuiltIn::fatal => self.infere_display(stmt, args),

            _ => (),
        }

        (Some(ty), valid)
    }

    fn check_display_dynamic_arg(&mut self, fmt_expr: ExprId, arg: Option<ExprId>, off: TextSize) {
        let arg = if let Some(arg) = arg {
            arg
        } else {
            self.result.diagnostics.push(InferenceDiagnostic::MissingFmtArg {
                fmt_lit: fmt_expr,
                lit_range: TextRange::at(off, 1u32.into()),
            });

            return;
        };
        match self.result.expr_types[arg].to_value() {
            Some(Type::Integer) => (),

            Some(ty) if ty.is_convertible_to(&Type::Integer) => {
                self.result.casts.insert(arg, Type::Integer);
            }
            _ => self.result.diagnostics.push(InferenceDiagnostic::DisplayTypeMismatch {
                err: TypeMismatch {
                    expected: Cow::Borrowed(&[TyRequirement::Val(Type::Integer)]),
                    found_ty: self.result.expr_types[arg].clone(),
                    expr: arg,
                },
                fmt_lit: fmt_expr,
                lit_range: TextRange::at(off, 1u32.into()),
                lint_ctx: None,
            }),
        }
    }

    fn check_display_arg_val(
        &mut self,
        stmt: StmtId,
        fmt_expr: ExprId,
        arg: Option<ExprId>,
        lit_range: TextRange,
        ty: Type,
    ) {
        let arg = if let Some(arg) = arg {
            arg
        } else {
            self.result
                .diagnostics
                .push(InferenceDiagnostic::MissingFmtArg { fmt_lit: fmt_expr, lit_range });

            return;
        };
        match self.result.expr_types[arg].to_value() {
            Some(ty_) if ty_ == ty => (),

            Some(ty_) if ty_.is_convertible_to(&ty) => {
                self.result.casts.insert(arg, ty);
            }

            Some(ty_) if ty_.is_assignable_to(&ty) => {
                self.result.casts.insert(arg, ty.clone());
                self.result.diagnostics.push(InferenceDiagnostic::DisplayTypeMismatch {
                    err: TypeMismatch {
                        expected: Cow::Owned(vec![TyRequirement::Val(ty)]),
                        found_ty: self.result.expr_types[arg].clone(),
                        expr: arg,
                    },
                    fmt_lit: fmt_expr,
                    lit_range,
                    lint_ctx: Some(stmt),
                })
            }
            _ => self.result.diagnostics.push(InferenceDiagnostic::DisplayTypeMismatch {
                err: TypeMismatch {
                    expected: Cow::Owned(vec![TyRequirement::Val(ty)]),
                    found_ty: self.result.expr_types[arg].clone(),
                    expr: arg,
                },
                fmt_lit: fmt_expr,
                lit_range,
                lint_ctx: None,
            }),
        }
    }

    fn infere_display(&mut self, stmt: StmtId, args: &[ExprId]) {
        let mut i = 0;
        while let Some(fmt_expr) = args.get(i) {
            i += 1;
            if let Expr::Literal(Literal::String(ref lit)) = self.body.exprs[*fmt_expr] {
                let mut chars = lit.char_indices();
                while let Some((start, c)) = chars.next() {
                    if c == '%' {
                        let pos = chars.next();
                        let mut end: TextSize = (start + 2).try_into().unwrap();
                        let ty = match pos.map(|(_, c)| c) {
                            Some('%' | 'm' | 'M' | 'l' | 'L') => continue, // escape sequences, always correct
                            Some('d' | 'D' | 'h' | 'H' | 'o' | 'O' | 'b' | 'B' | 'c' | 'C') => {
                                Type::Integer
                            }
                            Some('s' | 'S') => Type::String,
                            _ => {
                                let res =
                                    parse_real_fmt_spec(start as u32, *fmt_expr, pos, &mut chars);
                                if let Some(err) = res.err {
                                    self.result.diagnostics.push(err);
                                    i += 1 + res.dynamic_args.len();
                                    continue;
                                }

                                for pos in res.dynamic_args {
                                    self.check_display_dynamic_arg(
                                        *fmt_expr,
                                        args.get(i).copied(),
                                        pos,
                                    );
                                    i += 1;
                                }

                                end = res.end;
                                Type::Real
                            }
                        };

                        let arg = args.get(i).copied();
                        let range = TextRange::new(start.try_into().unwrap(), end);
                        self.check_display_arg_val(stmt, *fmt_expr, arg, range, ty);

                        i += 1;
                    }
                }
            }
        }
    }
    fn infere_limit(&mut self, stmt: StmtId, expr: ExprId, args: &[ExprId]) {
        let sig = if let Some(sig) = self.result.resolved_signatures.get(&expr) {
            *sig
        } else {
            // already reported an error no need to repeat
            return;
        };

        let probe = args[0];
        if self.result.expr_types[probe] != Ty::Val(Type::Err)
            && !matches!(
                self.result.resolved_calls.get(&probe),
                Some(ResolvedFun::BuiltIn(BuiltIn::potential | BuiltIn::flow))
            )
        {
            self.result.diagnostics.push(InferenceDiagnostic::ExpectedProbe { e: probe })
        }

        if let Some(Ty::UserFunction(func)) = self.result.expr_types.get(args[1]).cloned() {
            debug_assert_eq!(sig, LIMIT_USER_FUNCTION);
            let fun_info = self.db.function_data(func);

            // user-function needs two extra arguments but $limit also accepts two accepts that are
            // not passed directly to the function so these must just be equal
            if fun_info.args.len() != args.len() {
                self.result.diagnostics.push(InferenceDiagnostic::ArgCntMismatch {
                    expected: fun_info.args.len(),
                    found: args.len(),
                    expr,
                    exact: true,
                });
                return;
            }

            let output_args: Vec<_> = fun_info
                .args
                .iter_enumerated()
                .filter_map(|(id, info)| info.is_output.then_some(id))
                .collect();

            let invalid_ret = !matches!(fun_info.return_ty, Type::Real | Type::Err);
            let invalid_arg0 = !matches!(fun_info.args.raw[0].ty, Type::Real | Type::Err);
            let invalid_arg1 = !matches!(fun_info.args.raw[1].ty, Type::Real | Type::Err);

            if invalid_ret || invalid_arg0 || invalid_arg1 || !output_args.is_empty() {
                self.result.diagnostics.push(InferenceDiagnostic::InvalidLimitFunction {
                    expr,
                    func,
                    invalid_arg0,
                    invalid_arg1,
                    invalid_ret,
                    output_args,
                })
            }

            let signature = fun_info.args.raw[2..]
                .iter()
                .map(|arg| TyRequirement::Val(arg.ty.clone()))
                .collect();

            self.resolve_function_args(
                stmt,
                expr,
                &args[2..],
                Cow::Owned(TiVec::from(vec![SignatureData {
                    args: Cow::Owned(signature),
                    return_ty: fun_info.return_ty.clone(),
                }])),
                Some(func),
            );

            self.result.resolved_calls.insert(expr, ResolvedFun::User { func, limit: true });
        } else if sig == LIMIT_BUILTIN_FUNCTION {
            self.resolve_function_args(
                stmt,
                expr,
                &args[2..],
                Cow::Owned(TiVec::from(vec![SignatureData {
                    args: Cow::Owned(vec![TyRequirement::Val(Type::Real); args.len() - 2]),
                    return_ty: Type::Real,
                }])),
                None,
            );
        }
    }

    fn infere_ddx(&mut self, stmt: StmtId, expr: ExprId, val: ExprId, unknown: ExprId) {
        if let Some(ty) = self.infere_expr(stmt, val) {
            self.expect::<false>(expr, None, ty, Cow::Borrowed(&[TyRequirement::Val(Type::Real)]));
        }

        let ty = self.infere_expr(stmt, unknown);
        if ty.is_some() {
            let (call, signature) = if let (Some(ResolvedFun::BuiltIn(fun)), Some(signature)) = (
                self.result.resolved_calls.get(&unknown),
                self.result.resolved_signatures.get(&unknown),
            ) {
                (*fun, *signature)
            } else {
                if !matches!(&self.body.exprs[expr], Expr::Call { .. }) {
                    self.result.diagnostics.push(InferenceDiagnostic::InvalidUnknown { e: unknown });
                }
                return;
            };

            let signature = match (call, signature) {
                (BuiltIn::potential, NATURE_ACCESS_NODES) => {
                    self.result
                        .diagnostics
                        .push(InferenceDiagnostic::NonStandardUnknown { e: unknown, stmt });
                    DDX_POT_DIFF
                }
                (BuiltIn::potential, NATURE_ACCESS_NODE_GND) => DDX_POT,
                (BuiltIn::flow, NATURE_ACCESS_BRANCH) => DDX_FLOW,
                (BuiltIn::temperature, _) => {
                    self.result
                        .diagnostics
                        .push(InferenceDiagnostic::NonStandardUnknown { e: unknown, stmt });
                    DDX_TEMP
                }
                _ => {
                    self.result.diagnostics.push(InferenceDiagnostic::InvalidUnknown { e: unknown });
                    return;
                }
            };

            self.result.resolved_signatures.insert(expr, signature);
        }
    }

    fn infere_array(&mut self, stmt: StmtId, args: &[ExprId]) -> Option<Ty> {
        let infere_value_ty = |sel: &mut Self, arg| -> Option<Type> {
            sel.infere_expr(stmt, arg).and_then(|ty| {
                let res = ty.to_value();
                if res.is_none() {
                    sel.result.diagnostics.push(
                        TypeMismatch {
                            expected: Cow::Borrowed(&[TyRequirement::AnyVal]),
                            found_ty: ty,
                            expr: arg,
                        }
                        .into(),
                    )
                }
                res
            })
        };

        let mut iter = args.iter();
        let (ty, first_expr) = loop {
            let arg = match iter.next() {
                Some(arg) => arg,
                None => return None,
            };

            if let Some(ty) = infere_value_ty(self, *arg) {
                break (ty, *arg);
            }
        };

        let ty = iter.fold(ty, |ty, arg| {
            let arg_ty = match infere_value_ty(self, *arg) {
                Some(arg_ty) => arg_ty,
                None => return ty,
            };
            match ty.union(&arg_ty) {
                Some(ty) => ty,
                None => {
                    self.result.diagnostics.push(
                        ArrayTypeMismatch {
                            expected: ty.clone(),
                            found_ty: arg_ty,
                            found_expr: *arg,
                            expected_expr: first_expr,
                        }
                        .into(),
                    );
                    ty
                }
            }
        });

        for arg in args {
            if let Some(arg_ty) = self.result.expr_types[*arg].to_value() {
                if arg_ty != ty {
                    self.result.casts.insert(*arg, ty.clone());
                }
            }
        }

        Some(Ty::Val(ty))
    }

    fn infere_bin_op(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        lhs: ExprId,
        rhs: ExprId,
        op: BinaryOp,
    ) -> Option<Ty> {
        let signatures = match op {
            BinaryOp::BooleanOr | BinaryOp::BooleanAnd => &[SignatureData::CONDITIONAL_BIN_OP],

            BinaryOp::LesserEqualTest
            | BinaryOp::GreaterEqualTest
            | BinaryOp::LesserTest
            | BinaryOp::GreaterTest => SignatureData::NUMERIC_COMPARISON,

            BinaryOp::Addition
            | BinaryOp::Multiplication
            | BinaryOp::Subtraction
            | BinaryOp::Division
            | BinaryOp::Remainder => SignatureData::NUMERIC_BIN_OP,

            BinaryOp::LeftShift
            | BinaryOp::RightShift
            | BinaryOp::BitwiseXor
            | BinaryOp::BitwiseEq
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseAnd => &[SignatureData::INT_BIN_OP],
            BinaryOp::Power => &[SignatureData::REAL_BIN_OP],
            BinaryOp::EqualityTest | BinaryOp::NegatedEqualityTest => SignatureData::ANY_COMPARISON,
        };
        let signatures = Cow::Borrowed(TiSlice::from_ref(signatures));

        self.resolve_function_args(stmt, expr, &[lhs, rhs], signatures, None).0
    }

    /// Resolves the arguments of a function Call
    /// **NOTE** The argument count needs to be check otherwise useless error messages will be
    /// produced
    fn resolve_function_args(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        args: &[ExprId],
        signatures: Cow<'static, TiSlice<Signature, SignatureData>>,
        src: Option<FunctionId>,
    ) -> (Option<Ty>, bool) {
        debug_assert!(signatures.iter().any(|sig| sig.args.len() == args.len()));
        let arg_types: Vec<_> = args.iter().map(|arg| self.infere_expr(stmt, *arg)).collect();
        let mut valid_args = true;

        let mut candidates: Vec<_> = signatures.keys().collect();
        let mut new_candidates = Vec::new();
        let mut errors = Vec::new();
        for (i, (arg, ty)) in zip(args, &arg_types).enumerate() {
            if let Some(ty) = ty {
                new_candidates.clone_from(&candidates);
                new_candidates.retain(|candidate| {
                    signatures[*candidate]
                        .args
                        .get(i)
                        .map_or(false, |req| ty.satisfies_with_conversion(req))
                });
                if new_candidates.is_empty() {
                    let candidate_types: Vec<TyRequirement> = candidates
                        .iter()
                        .filter_map(|candidate| signatures[*candidate].args.get(i).cloned())
                        .collect();
                    debug_assert_ne!(&candidate_types, &[]);
                    errors.push(TypeMismatch {
                        expected: Cow::from(candidate_types),
                        found_ty: ty.clone(),
                        expr: *arg,
                    });
                } else {
                    mem::swap(&mut new_candidates, &mut candidates)
                }
            } else {
                valid_args = false;
            }
        }

        candidates.retain(|sig| signatures[*sig].args.len() == args.len());

        if !errors.is_empty() || candidates.is_empty() {
            self.result.diagnostics.push(
                SignatureMismatch {
                    type_mismatches: errors.into_boxed_slice(),
                    signatures: signatures.clone(),
                    src,
                    found: arg_types
                        .iter()
                        .map(|it| it.clone().unwrap_or(Ty::Val(Type::Err)))
                        .collect(),
                }
                .into(),
            );
            return (default_return_ty(&signatures.raw), false);
        }

        let res = match candidates.as_slice() {
            [] => {
                unreachable!()
            }
            [res] => *res,
            _ if arg_types.iter().any(|ty| ty.is_none()) => {
                return (default_return_ty(&signatures.raw), false)
            }
            _ => {
                new_candidates.clone_from(&candidates);
                candidates.retain(|candidate| {
                    zip(&arg_types, signatures[*candidate].args.as_ref())
                        .all(|(ty, req)| ty.as_ref().map_or(false, |ty| ty.satisfies_semantic(req)))
                });

                if candidates.len() > 1 {
                    new_candidates.clone_from(&candidates);
                    candidates.retain(|candidate| {
                        zip(&arg_types, signatures[*candidate].args.as_ref()).all(|(ty, req)| {
                            ty.as_ref().map_or(false, |ty| ty.satisfies_exact(req))
                        })
                    });
                    if candidates.is_empty() {
                        candidates = new_candidates;
                    }
                }

                candidates[0]
            }
        };

        for (dst, (src, arg)) in zip(signatures[res].args.as_ref(), zip(arg_types, args)) {
            if let Some(src) = src.and_then(|ty| ty.to_value()) {
                if let Some(cast) = dst.cast(&src) {
                    self.result.casts.insert(*arg, cast);
                }
            }
        }

        if signatures.len() > 1 {
            self.result.resolved_signatures.insert(expr, res);
        }

        (Some(Ty::Val(signatures[res].return_ty.clone())), valid_args)
    }

    fn expect<const EXACT: bool>(
        &mut self,
        expr: ExprId,
        parent_fun: Option<ExprId>,
        ty: Ty,
        req: Cow<'static, [TyRequirement]>,
    ) -> Option<usize> {
        let fun = if EXACT { Ty::satisfies_semantic } else { Ty::satisfies_with_conversion };

        let res = req.iter().position(|req| fun(&ty, req));
        match res {
            Some(matched) => {
                if !EXACT {
                    if let Some(parent_fun) = parent_fun {
                        self.result
                            .resolved_signatures
                            .insert(parent_fun, Signature::from(matched));
                    }
                    if let Some(ty) = ty.to_value() {
                        if let Some(cast) = req[matched].cast(&ty) {
                            self.result.casts.insert(expr, cast);
                        }
                    }
                }
            }
            None => self
                .result
                .diagnostics
                .push(TypeMismatch { expected: req, found_ty: ty, expr }.into()),
        }

        res
    }

    fn resolve_path(&mut self, stmt: StmtId, expr: ExprId, path: &Path) -> Option<ScopeDefItem> {
        let resolved_path = match self.body.stmt_scopes[stmt].resolve_path(self.db.upcast(), path) {
            Ok(resolved_path) => resolved_path,
            Err(err) => {
                self.result.diagnostics.push(InferenceDiagnostic::PathResolveError { err, expr });
                return None;
            }
        };

        let attr = match resolved_path {
            ResolvedPath::FlowAttriubte { branch, ref name } => {
                BranchTy::flow_attr(self.db, branch, name)?
            }

            ResolvedPath::PotentialAttribute { branch, ref name } => {
                BranchTy::potential_attr(self.db, branch, name)?
            }

            ResolvedPath::ScopeDefItem(def) => return Some(def),
        };

        match attr {
            Ok(attr) => Some(attr.into()),
            Err(err) => {
                self.result.diagnostics.push(InferenceDiagnostic::PathResolveError { err, expr });
                None
            }
        }
    }

    fn resolve_item_path<T: ScopeDefItemKind>(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        path: &Path,
    ) -> Option<T> {
        match self.body.stmt_scopes[stmt].resolve_item_path(self.db.upcast(), path) {
            Ok(item) => Some(item),
            Err(err) => {
                self.result.diagnostics.push(InferenceDiagnostic::PathResolveError { err, expr });
                None
            }
        }
    }

    // fn collect_fmt_literal(&mut self, stmt: StmtId, args: &[ExprId]){
    //     self.body
    // }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferenceDiagnostic {
    InvalidAssignDst {
        e: ExprId,
        maybe_different_operand: Option<ast::AssignOp>,
        assignment_kind: ast::AssignOp,
    },
    PathResolveError {
        err: PathResolveError,
        expr: ExprId,
    },
    ArgCntMismatch {
        expected: usize,
        found: usize,
        expr: ExprId,
        exact: bool,
    },

    ExpectedProbe {
        e: ExprId,
    },

    InvalidLimitFunction {
        expr: ExprId,
        func: FunctionId,
        invalid_arg0: bool,
        invalid_arg1: bool,
        invalid_ret: bool,
        output_args: Vec<LocalFunctionArgId>,
    },

    DisplayTypeMismatch {
        err: TypeMismatch,
        fmt_lit: ExprId,
        lit_range: TextRange,
        lint_ctx: Option<StmtId>,
    },

    MissingFmtArg {
        fmt_lit: ExprId,
        lit_range: TextRange,
    },

    InvalidFmtSpecifierChar {
        fmt_lit: ExprId,
        lit_range: TextRange,
        err_char: char,
        candidates: &'static [char],
    },

    InvalidFmtSpecifierEnd {
        fmt_lit: ExprId,
        lit_range: TextRange,
    },

    TypeMismatch(TypeMismatch),
    SignatureMismatch(SignatureMismatch),
    ArrayTypeMismatch(ArrayTypeMismatch),
    InvalidUnknown {
        e: ExprId,
    },
    NonStandardUnknown {
        e: ExprId,
        stmt: StmtId,
    },
}

impl_from!(TypeMismatch,SignatureMismatch, ArrayTypeMismatch for InferenceDiagnostic);
