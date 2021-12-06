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
    Lookup, NatureId, NodeId, Path, Stmt, StmtId, Type, VarId,
};
use stdx::impl_from;
use stdx::iter::zip;
use syntax::ast::{self, BinaryOp, UnaryOp};
use typed_index_collections::{TiSlice, TiVec};

use crate::builtin::{
    DDX_FLOW, DDX_POT, DDX_POT_DIFF, DDX_TEMP, NATURE_ACCESS_BRANCH, NATURE_ACCESS_NODES,
    NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW,
};
use crate::db::HirTyDB;
use crate::diagnostics::{ArrayTypeMissmatch, SignatureMissmatch, TypeMissmatch};
use crate::lower::{BranchTy, DisciplineAccess};
use crate::types::{default_return_ty, BuiltinInfo, Signature, SignatureData, Ty, TyRequirement};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ResolvedFun {
    User(FunctionId),
    BuiltIn(BuiltIn),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum AssignDst {
    Var(VarId),
    FunVar { fun: FunctionId, arg: Option<LocalFunctionArgId> },
    Flow(BranchWrite),
    Potential(BranchWrite),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum BranchWrite {
    Explicit(BranchId),
    Implict { hi: NodeId, lo: Option<NodeId> },
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
    pub assigment_destination: AHashMap<StmtId, AssignDst>,
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

        let expr_stmt_ty = match id {
            DefWithBodyId::ParamId(param) => Some(db.param_data(param).ty.clone()),
            DefWithBodyId::VarId(var) => Some(db.var_data(var).ty.clone()),
            _ => None,
        };
        let mut ctx = Ctx { result, body: &body, db, expr_stmt_ty };

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
                self.infere_assigment(stmt, expr, self.expr_stmt_ty.clone());
            }
            Stmt::Assigment { dst, val, ref assignment_kind } => {
                let dst_ty = self.infere_assigment_dst(stmt, dst, assignment_kind);
                self.infere_assigment(stmt, val, dst_ty);
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

    fn infere_assigment(&mut self, stmt: StmtId, val: ExprId, dst_ty: Option<Type>) {
        if let Some(val_ty) = self.infere_expr(stmt, val) {
            if let Some(value_ty) = val_ty.to_value() {
                if let Some(dst_ty) = dst_ty {
                    if dst_ty.is_assignable_to(&value_ty) {
                        if dst_ty != value_ty {
                            self.result.casts.insert(val, dst_ty);
                        }
                    } else {
                        self.result.diagnostics.push(
                            TypeMissmatch {
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
                    TypeMissmatch {
                        expected: Cow::Owned(vec![expected]),
                        found_ty: val_ty,
                        expr: val,
                    }
                    .into(),
                );
            }
        }
    }

    pub fn infere_assigment_dst(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        assigment_kind: &ast::AssignOp,
    ) -> Option<Type> {
        let e = self.infere_expr(stmt, expr);

        let (dst, ty) = match e? {
            Ty::Var(ty, var) => (AssignDst::Var(var), ty),
            Ty::FuntionVar { fun, ty, arg } => (AssignDst::FunVar { fun, arg }, ty),
            Ty::Val(Type::Real)
                if matches!(
                    self.result.resolved_calls.get(&expr),
                    Some(ResolvedFun::BuiltIn(BuiltIn::potential | BuiltIn::flow))
                ) =>
            {
                let mut args = Vec::new();
                self.body.exprs[expr].walk_child_exprs(|e| args.push(&self.result.expr_types[e]));
                let kind = match *self.result.resolved_signatures.get(&expr)? {
                    NATURE_ACCESS_BRANCH => BranchWrite::Explicit(args[0].unwrap_branch()),
                    NATURE_ACCESS_NODES => BranchWrite::Implict {
                        hi: args[0].unwrap_node(),
                        lo: Some(args[1].unwrap_node()),
                    },

                    NATURE_ACCESS_NODE_GND => {
                        BranchWrite::Implict { hi: args[0].unwrap_node(), lo: None }
                    }

                    NATURE_ACCESS_PORT_FLOW => {
                        self.result.diagnostics.push(InferenceDiagnostic::InvalidAssignDst {
                            e: expr,
                            maybe_different_operand: None,
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
                });
                return None;
            }
        };

        // check that the correct operator is used
        match (&dst, assigment_kind) {
            (AssignDst::Var(_) | AssignDst::FunVar { .. }, ast::AssignOp::Contribute) => {
                self.result.diagnostics.push(InferenceDiagnostic::InvalidAssignDst {
                    e: expr,
                    maybe_different_operand: Some(MaybeDifferentOperand::Assign),
                });
            }
            (AssignDst::Flow(_) | AssignDst::Potential(_), ast::AssignOp::Assign) => {
                self.result.diagnostics.push(InferenceDiagnostic::InvalidAssignDst {
                    e: expr,
                    maybe_different_operand: Some(MaybeDifferentOperand::Contribute),
                });
            }
            _ => {
                self.result.assigment_destination.insert(stmt, dst);
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
                ScopeDefItem::ParamId(param) => {
                    Ty::Param(self.db.param_data(param).ty.clone(), param)
                }
                ScopeDefItem::BranchId(branch) => Ty::Branch(branch),
                ScopeDefItem::BuiltIn(_)
                | ScopeDefItem::FunctionId(_)
                | ScopeDefItem::NatureAccess(_) => Ty::Function,
                ScopeDefItem::FunctionReturn(fun) => Ty::FuntionVar {
                    fun,
                    ty: self.db.function_data(fun).return_ty.clone(),
                    arg: None,
                },
                ScopeDefItem::FunctionArgId(arg) => {
                    let FunctionArgLoc { fun, id } = arg.lookup(self.db.upcast());
                    Ty::FuntionVar {
                        fun,
                        ty: self.db.function_data(fun).args[id].ty.clone(),
                        arg: Some(id),
                    }
                }
                ScopeDefItem::NatureAttrId(attr) => self.db.nature_attr_ty(attr)?,
            },

            Expr::BinaryOp { op: None, lhs, rhs } => {
                self.infere_expr(stmt, lhs);
                self.infere_expr(stmt, rhs);
                return None;
            }

            Expr::BinaryOp { lhs, rhs, op: Some(op) } => {
                self.infere_bin_op(stmt, expr, lhs, rhs, op)?
            }

            Expr::UnaryOp { expr: arg, op: UnaryOp::Identity | UnaryOp::Neg } => {
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
                    Cow::Borrowed(&[TyRequirement::Val(Type::Bool)]),
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
                )?
            }

            Expr::Call { ref fun, ref args } => {
                self.infere_fun_call(stmt, expr, fun.as_ref()?, args)?
            }
            Expr::Array(ref args) if args.is_empty() => Ty::Val(Type::EmptyArray),
            Expr::Array(ref args) => return self.infere_array(stmt, args),
            Expr::Literal(Literal::Inf) => Ty::Literal(Type::Integer),
            Expr::Literal(Literal::Float(_)) => Ty::Literal(Type::Real),
            Expr::Literal(Literal::Int(_)) => Ty::Literal(Type::Integer),
            Expr::Literal(Literal::String(_)) => Ty::Literal(Type::String),
        };

        self.result.expr_types.insert(expr, ty.clone());

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
                self.infere_nature_acces(stmt, expr, access, args);
                Some(Ty::Val(Type::Real))
            }
            ScopeDefItem::FunctionId(fun) => self.infere_user_fun_call(stmt, expr, fun, args),
            ScopeDefItem::BuiltIn(builtin) => {
                self.result.resolved_calls.insert(expr, ResolvedFun::BuiltIn(builtin));
                self.infere_builtin(stmt, expr, builtin, args)
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
        fun: FunctionId,
        args: &[ExprId],
    ) -> Option<Ty> {
        self.result.resolved_calls.insert(expr, ResolvedFun::User(fun));
        let fun_info = self.db.function_data(fun);
        if fun_info.args.len() != args.len() {
            self.result.diagnostics.push(InferenceDiagnostic::ArgCntMissmatch {
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
            Some(fun),
        )
    }

    fn infere_nature_acces(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        acccess: NatureAccess,
        args: &[ExprId],
    ) {
        // resolve as flow first because we don't yet know if this is a flow or pot access
        // This choise is arbitrary (but must be consistent with the code below
        if self.infere_builtin(stmt, expr, BuiltIn::flow, args).is_none() {
            return;
        }

        // Now that we know that the arguments are valid actually resolve whether this is flow or
        // pot access
        let acccess =
            self.infere_access_kind(acccess.0.lookup(self.db.upcast()).nature, expr, args[0]);

        // update resolved_calls in case this is actually a pot and not a flow access
        match acccess {
            Some(DisciplineAccess::Potential) => {
                self.result.resolved_calls.insert(expr, ResolvedFun::BuiltIn(BuiltIn::potential));
            }

            Some(DisciplineAccess::Flow) => {
                self.result.resolved_calls.insert(expr, ResolvedFun::BuiltIn(BuiltIn::flow));
            }

            None => (),
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

        let node = self.db.node_data(node);

        let def_map = self.db.def_map(nature.lookup(self.db.upcast()).root_file);
        let discipline =
            def_map.resolve_local_item_in_scope(def_map.root(), node.discipline.as_ref()?).ok()?;

        self.db.discipline_info(discipline).access(nature, self.db)
    }

    fn infere_builtin(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        builtin: BuiltIn,
        args: &[ExprId],
    ) -> Option<Ty> {
        let info: BuiltinInfo = builtin.into();

        if builtin == BuiltIn::limit {
            todo!()
        }

        let exact = Some(info.min_args) == info.max_args;
        if args.len() < info.min_args {
            let err = InferenceDiagnostic::ArgCntMissmatch {
                expected: info.min_args,
                found: args.len(),
                expr,
                exact,
            };
            self.result.diagnostics.push(err);
            return default_return_ty(info.signatures);
        }

        if info.max_args.map_or(false, |max_args| max_args < args.len()) {
            self.result.diagnostics.push(InferenceDiagnostic::ArgCntMissmatch {
                expected: info.min_args,
                found: args.len(),
                expr,
                exact,
            });
            return default_return_ty(info.signatures);
        }

        let signatures = match builtin {
            BuiltIn::ddx => {
                self.infere_ddx(stmt, expr, args[0], args[1]);
                return Some(Ty::Val(Type::Real));
            }
            _ if info.max_args.is_none() => {
                debug_assert_eq!(info.signatures.len(), 1, "{:?}", builtin);
                let mut signature = info.signatures[0].clone();
                signature.args.to_mut().resize_with(args.len(), || TyRequirement::AnyVal);
                Cow::Owned(TiVec::from(vec![signature]))
            }
            _ => Cow::Borrowed(TiSlice::from_ref(info.signatures)),
        };

        debug_assert_ne!(&signatures.raw, &[]);

        let ty = self.resolve_function_args(stmt, expr, args, signatures, None)?;

        Some(ty)
    }

    fn infere_ddx(&mut self, stmt: StmtId, expr: ExprId, val: ExprId, unkown: ExprId) {
        if let Some(ty) = self.infere_expr(stmt, val) {
            self.expect::<false>(expr, None, ty, Cow::Borrowed(&[TyRequirement::Val(Type::Real)]));
        }

        let ty = self.infere_expr(stmt, unkown);
        if ty.is_some() {
            let (call, signature) = if let (Some(ResolvedFun::BuiltIn(fun)), Some(signature)) = (
                self.result.resolved_calls.get(&unkown),
                self.result.resolved_signatures.get(&unkown),
            ) {
                (*fun, *signature)
            } else {
                if !matches!(&self.body.exprs[expr], Expr::Call { .. }) {
                    self.result.diagnostics.push(InferenceDiagnostic::InvalidUnkown { e: unkown });
                }
                return;
            };

            let signature = match (call, signature) {
                (BuiltIn::potential, NATURE_ACCESS_NODES) => {
                    self.result
                        .diagnostics
                        .push(InferenceDiagnostic::NonStandardUnkown { e: unkown, stmt });
                    DDX_POT_DIFF
                }
                (BuiltIn::potential, NATURE_ACCESS_NODE_GND) => DDX_POT,
                (BuiltIn::flow, NATURE_ACCESS_BRANCH) => DDX_FLOW,
                (BuiltIn::temperature, _) => {
                    self.result
                        .diagnostics
                        .push(InferenceDiagnostic::NonStandardUnkown { e: unkown, stmt });
                    DDX_TEMP
                }
                _ => {
                    self.result.diagnostics.push(InferenceDiagnostic::InvalidUnkown { e: unkown });
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
                        TypeMissmatch {
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
                        ArrayTypeMissmatch {
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

        self.resolve_function_args(stmt, expr, &[lhs, rhs], signatures, None)
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
    ) -> Option<Ty> {
        debug_assert!(signatures.iter().any(|sig| sig.args.len() == args.len()));
        let arg_types: Vec<_> = args.iter().map(|arg| self.infere_expr(stmt, *arg)).collect();

        let mut canditates: Vec<_> = signatures.keys().collect();
        let mut new_candidates = Vec::new();
        let mut errors = Vec::new();
        for (i, (arg, ty)) in zip(args, &arg_types).enumerate() {
            if let Some(ty) = ty {
                new_candidates.clone_from(&canditates);
                new_candidates.retain(|candidate| {
                    signatures[*candidate]
                        .args
                        .get(i)
                        .map_or(false, |req| ty.satisfies_with_conversion(req))
                });
                if new_candidates.is_empty() {
                    let candidate_types: Vec<TyRequirement> = canditates
                        .iter()
                        .filter_map(|candidate| signatures[*candidate].args.get(i).cloned())
                        .collect();
                    debug_assert_ne!(&candidate_types, &[]);
                    errors.push(TypeMissmatch {
                        expected: Cow::from(candidate_types),
                        found_ty: ty.clone(),
                        expr: *arg,
                    });
                } else {
                    mem::swap(&mut new_candidates, &mut canditates)
                }
            }
        }

        canditates.retain(|sig| signatures[*sig].args.len() == args.len());

        if !errors.is_empty() || canditates.is_empty() {
            self.result.diagnostics.push(
                SignatureMissmatch {
                    type_missmatches: errors.into_boxed_slice(),
                    signatures: signatures.clone(),
                    src,
                }
                .into(),
            );
            return default_return_ty(&signatures.raw);
        }

        let res = match canditates.as_slice() {
            [] => {
                unreachable!()
            }
            [res] => *res,
            _ if arg_types.iter().any(|ty| ty.is_none()) => {
                return default_return_ty(&signatures.raw);
            }
            _ => {
                new_candidates.clone_from(&canditates);
                canditates.retain(|candidate| {
                    zip(&arg_types, signatures[*candidate].args.as_ref())
                        .all(|(ty, req)| ty.as_ref().map_or(false, |ty| ty.satisfies_exact(req)))
                });

                if cfg!(debug_assert) && canditates.len() != 1 {
                    let sig: Vec<_> =
                        canditates.iter().map(|canditate| &signatures[*canditate]).collect();
                    eprintln!(
                        "ambigous signature {:?} all match {:?} (using first variant)",
                        sig, arg_types
                    );
                } else if canditates.is_empty() {
                    let sig: Vec<_> =
                        new_candidates.iter().map(|canditate| &signatures[*canditate]).collect();

                    unreachable!(
                        "ambigous signature {:#?} all match {:?} (nothing matches exactly...)",
                        sig, arg_types
                    );
                }

                canditates[0]
            }
        };

        for (dst, (src, arg)) in zip(signatures[res].args.as_ref(), zip(arg_types, args)) {
            if let TyRequirement::Val(dst) = dst {
                if let Some(src) = src.and_then(|ty| ty.to_value()) {
                    if &src != dst {
                        self.result.casts.insert(*arg, dst.clone());
                    }
                }
            }
        }

        if signatures.len() > 1 {
            self.result.resolved_signatures.insert(expr, res);
        }

        Some(Ty::Val(signatures[res].return_ty.clone()))
    }

    fn expect<const EXACT: bool>(
        &mut self,
        expr: ExprId,
        parent_fun: Option<ExprId>,
        ty: Ty,
        req: Cow<'static, [TyRequirement]>,
    ) -> Option<usize> {
        let fun = if EXACT { Ty::satisfies_exact } else { Ty::satisfies_with_conversion };

        let res = req.iter().position(|req| fun(&ty, req));
        match res {
            Some(matched) => {
                if !EXACT {
                    if let Some(parent_fun) = parent_fun {
                        self.result
                            .resolved_signatures
                            .insert(parent_fun, Signature::from(matched));
                    }

                    if let TyRequirement::Val(req) = &req[matched] {
                        if let Some(ty) = ty.to_value() {
                            if !req.is_semantically_equivalent(&ty) {
                                self.result.casts.insert(expr, req.clone());
                            }
                        }
                    }
                }
            }
            None => self
                .result
                .diagnostics
                .push(TypeMissmatch { expected: req, found_ty: ty, expr }.into()),
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
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum MaybeDifferentOperand {
    Contribute,
    Assign,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferenceDiagnostic {
    InvalidAssignDst { e: ExprId, maybe_different_operand: Option<MaybeDifferentOperand> },
    PathResolveError { err: PathResolveError, expr: ExprId },
    ArgCntMissmatch { expected: usize, found: usize, expr: ExprId, exact: bool },
    TypeMissmatch(TypeMissmatch),
    SignatureMissmatch(SignatureMissmatch),
    ArrayTypeMissmatch(ArrayTypeMissmatch),
    InvalidUnkown { e: ExprId },
    NonStandardUnkown { e: ExprId, stmt: StmtId },
}

impl_from!(TypeMissmatch,SignatureMissmatch, ArrayTypeMissmatch for InferenceDiagnostic);
