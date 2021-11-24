use std::{borrow::Cow, mem};

use ahash::AHashMap;
use arena::ArenaMap;
use hir_def::{
    body::Body,
    db::HirDefDB,
    expr::Literal,
    nameres::{NatureAccess, PathResolveError, ScopeDefItem, ScopeDefItemKind},
    BranchId, BuiltIn, Expr, ExprId, FunctionId, Lookup, NodeId, Path, StmtId, Type, VarId,
};
use stdx::{impl_from, iter::zip};
use syntax::ast::{BinaryOp, UnaryOp};
use typed_index_collections::{TiSlice, TiVec};

use crate::{
    builtin::BuiltinInfo,
    requirements::{
        ArrayTypeMissmatch, Signature, SignatureData, SignatureMissmatch, Ty, TyRequirement,
        TypeMissmatch,
    },
};

pub enum ResolvedFunction {
    User(FunctionId),
    BuiltIn { fun: BuiltIn },
}

pub enum AsssigmentDestination {
    Var(VarId),
    ExplicitBranchFlow(BranchId),
    ExplicitBranchPot(BranchId),
    ImplicitBranchPot { hi: NodeId, lo: Option<NodeId> },
    ImplicitBranchFlow { hi: NodeId, lo: Option<NodeId> },
}

impl AsssigmentDestination {
    pub fn ty(&self, db: &dyn HirDefDB) -> Type {
        if let AsssigmentDestination::Var(var) = *self {
            let var = var.lookup(db);
            let tree = var.item_tree(db);
            tree[var.id].ty.clone()
        } else {
            Type::Real
        }
    }
}

pub struct InferenceResult {
    expr_types: ArenaMap<Expr, Type>,
    resolved_calls: ArenaMap<Expr, ResolvedFunction>,
    resolved_signatures: ArenaMap<Expr, Signature>,
    assigment_destination: AHashMap<StmtId, ResolvedFunction>,
    casts: AHashMap<ExprId, Type>,
    diagnostics: Vec<InferenceDiagnostic>,
}

struct Ctx<'a> {
    result: InferenceResult,
    body: &'a Body,
    db: &'a dyn HirDefDB,
}

impl Ctx<'_> {
    // pub fn infere_stmt(&mut self, stmt: StmtId) {
    // let stmt = self.body.stmts[stmt];
    // match self.body.stmts[stmt] {
    //     hir_def::Stmt::Expr(e) => {
    //         let (ty, has_side_effects) = self.infere_expr(e);
    //         if !has_side_effects {
    //             // TODO WARN
    //         }
    //     }
    //     hir_def::Stmt::Assigment { dst, val, assignment_kind} => self.infere_expr(val),
    //     } => self.infere_expr(val),
    //     hir_def::Stmt::If { cond, then_branch, else_branch } => todo!(),
    //     hir_def::Stmt::ForLoop { init, cond, incr, body } => todo!(),
    //     hir_def::Stmt::WhileLoop { cond, body } => todo!(),
    //     hir_def::Stmt::Case { discr, case_arms } => todo!(),
    //     _ => stmt.walk_child_stmts(|stmt| self.infere_stmt(stmt)),
    // }
    // }

    // pub fn infere_assigment_dst(
    //     &self,
    //     e: ExprId,
    //     assigment_kind: ast::AssignOp,
    //     stmt: StmtId,
    // ) -> Result<(Type, AsssigmentDestination), InferenceDiagnostic> {
    //     let scope = self.body.stmt_scopes[stmt];
    //     match self.body.exprs[e] {
    //         Expr::Path { ref path, port: false } => {
    //             let res: Result<VarId, _> =
    //                 self.def_map.resolve_item_path_in_scope(scope, path, self.db);

    //             match assigment_kind {
    //                 ast::AssignOp::Assign => match res {
    //                     Ok(var) => {
    //                         let ty = self.db.var_inf(var).ty;
    //                         Ok((ty, AsssigmentDestination::Var(var)))
    //                     }
    //                     Err(err) => Err(InferenceDiagnostic::PathResolveError { err, e }),
    //                 },
    //                 ast::AssignOp::Contribute => Err(InferenceDiagnostic::InvalidContributeDst {
    //                     e,
    //                     maybe_assign: res.is_ok(),
    //                 }),
    //             }
    //         }
    //         Expr::Call { fun, args } => {}
    //         _ => todo!(),
    //     }
    // }

    // fn infere_nature_acces_call(
    //     &self,
    //     stmt: StmtId,
    //     expr: ExprId,
    //     acces: NatureAccess,
    //     args: &[ExprId],
    // ) -> Result<BranchAccess, InferenceDiagnostic> {
    //     let nature = acces.0;

    //     match *args {
    //         [arg] => match self.body.exprs[arg] {
    //             Expr::Path { path, port: true } => {
    //                 let port = self.body.stmt_scopes[stmt].resolve_item_path(self.db, path);
    //             }
    //         },
    //         [arg1, arg2] => (),
    //         ref args => {
    //             return Err(InferenceDiagnostic::ArgCntMissmatch {
    //                 expected: 2,
    //                 found: args.len(),
    //                 expr,
    //             })
    //         }
    //     }
    // }

    fn infere_cond(&mut self, stmt: StmtId, expr: ExprId) {
        if let Some(ty) = self.infere_expr(stmt, expr) {
            self.expect::<false>(expr, None, ty, Cow::Borrowed(&[TyRequirement::Val(Type::Bool)]));
        }
    }

    fn infere_expr(&mut self, stmt: StmtId, expr: ExprId) -> Option<Ty> {
        let ty = match self.body.exprs[expr] {
            Expr::Missing | Expr::BinaryOp { op: None, .. } => return None,
            Expr::Path { ref path, port: true } => {
                let port = self.resolve_item_path(stmt, expr, path)?;
                Ty::PortFlow(port)
            }

            Expr::Path { ref path, port: false } => {
                match self.resolve_path(stmt, expr, path)? {
                    ScopeDefItem::BlockId(_) | ScopeDefItem::ModuleId(_) => Ty::Scope,
                    ScopeDefItem::NatureId(nature) => Ty::Nature(nature),
                    ScopeDefItem::DisciplineId(discipline) => Ty::Discipline(discipline),
                    ScopeDefItem::NodeId(node) => Ty::Node(node),
                    ScopeDefItem::VarId(var) => Ty::Var(self.db.var_data(var).ty.clone(), var),
                    ScopeDefItem::ParamId(param) => {
                        Ty::Param(self.db.param_data(param).ty.clone(), param)
                    }
                    ScopeDefItem::BranchId(branch) => Ty::Branch(branch),
                    ScopeDefItem::DisciplineAttrId(_attr) => Ty::Val(Type::Real), // TODO are these really always real numbers?
                    ScopeDefItem::NatureAttrId(_attr) => Ty::Val(Type::Real), // TODO are these really always real numbers?
                    ScopeDefItem::BuiltIn(_)
                    | ScopeDefItem::FunctionId(_)
                    | ScopeDefItem::NatureAccess(_) => Ty::Function,
                    ScopeDefItem::FunctionReturn(fun) => Ty::Var(todo!()),
                    ScopeDefItem::FunctionArgId(arg) => Ty::Var(todo!()),
                }
            }

            Expr::BinaryOp { lhs, rhs, op: Some(op) } => {
                return self.infere_bin_op(stmt, expr, lhs, rhs, op)
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
                return self.resolve_function_args(
                    stmt,
                    expr,
                    &[then_val, else_val],
                    Cow::Borrowed(TiSlice::from_ref(SignatureData::SELECT)),
                );
            }

            Expr::Call { ref fun, ref args } => {
                return self.infere_fun_call(stmt, expr, fun.as_ref()?, args)
            }
            Expr::Array(ref args) if args.len() == 0 => Ty::Val(Type::EmptyArray),
            Expr::Array(ref args) => return self.infere_array(stmt, expr, args),
            Expr::Literal(Literal::Inf) => Ty::Literal(Type::Integer),
            Expr::Literal(Literal::Float(_)) => Ty::Literal(Type::Real),
            Expr::Literal(Literal::Int(_)) => Ty::Literal(Type::Integer),
            Expr::Literal(Literal::String(_)) => Ty::Literal(Type::String),
        };
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
                // self.infere_nature_acces(stmt,expr,access,args);
                todo!()
            }
            ScopeDefItem::FunctionId(fun) => todo!(),
            ScopeDefItem::BuiltIn(builtin) => self.infere_builtin(stmt, expr, builtin, args),
            found => {
                self.result.diagnostics.push(InferenceDiagnostic::PathResolveError {
                    err: PathResolveError::ExpectedItemKind {
                        expected: "a function",
                        found,
                        name: fun.segments.last().unwrap().to_owned(),
                    },
                    expr,
                });
                None
            }
        }
    }

    fn infere_nature_acces(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        access: NatureAccess,
        args: &[ExprId],
    ) -> Option<Ty> {
        let nature = access.0;
        let mut is_potential = false;
    }

    fn infere_builtin(
        &mut self,
        stmt: StmtId,
        expr: ExprId,
        builtin: BuiltIn,
        args: &[ExprId],
    ) -> Option<Ty> {
        let info: BuiltinInfo = builtin.into();

        if args.len() < info.min_args {
            self.result.diagnostics.push(InferenceDiagnostic::ArgCntMissmatch {
                expected: info.min_args,
                found: args.len(),
                expr,
            });
            return None;
        }

        if builtin == BuiltIn::limit {
            todo!()
        }

        if info.max_args.map_or(false, |max_args| max_args < args.len()) {
            self.result.diagnostics.push(InferenceDiagnostic::ArgCntMissmatch {
                expected: info.min_args,
                found: args.len(),
                expr,
            });
            return None;
        }

        let signature = match builtin {
            BuiltIn::ddx => todo!(),
            _ if info.max_args.is_none() => {
                debug_assert_eq!(info.signatures.len(), 1);
                let mut signature = info.signatures[0].clone();
                signature.args.to_mut().resize_with(args.len(), || TyRequirement::AnyVal);
                Cow::Owned(TiVec::from(vec![signature]))
            }
            _ => Cow::Borrowed(TiSlice::from_ref(info.signatures)),
        };

        let signature = self.resolve_function_args(stmt, expr, args, signature)?;

        if builtin == BuiltIn::potential
            && self.result.resolved_signatures.get(expr) == Some(&Signature::from(3usize))
        {
            self.result
                .diagnostics
                .push(InferenceDiagnostic::PotentialOfPortFlow { expr, port_flow: args[0] })
        }

        Some(signature)
    }

    fn infere_array(&mut self, stmt: StmtId, expr: ExprId, args: &[ExprId]) -> Option<Ty> {
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

        let mut iter = args.into_iter();
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
            BinaryOp::BooleanOr | BinaryOp::BooleanAnd => &[SignatureData::BOOL_BIN_OP],
            BinaryOp::LesserEqualTest
            | BinaryOp::GreaterEqualTest
            | BinaryOp::LesserTest
            | BinaryOp::GreaterTest => SignatureData::NUMERIC_COMPARISON,
            BinaryOp::Addition
            | BinaryOp::Multiplication
            | BinaryOp::Subtraction
            | BinaryOp::Division => SignatureData::NUMERIC_BIN_OP,
            BinaryOp::Remainder
            | BinaryOp::LeftShift
            | BinaryOp::RightShift
            | BinaryOp::BitwiseXor
            | BinaryOp::BitwiseEq
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseAnd => &[SignatureData::INT_BIN_OP],
            BinaryOp::Power => &[SignatureData::REAL_BIN_OP],
            BinaryOp::EqualityTest | BinaryOp::NegatedEqualityTest => SignatureData::ANY_COMPARISON,
        };
        let signatures = Cow::Borrowed(TiSlice::from_ref(signatures));

        self.resolve_function_args(stmt, expr, &[lhs, rhs], signatures)
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
    ) -> Option<Ty> {
        debug_assert!(signatures.iter().any(|sig| sig.args.len() == args.len()));
        let arg_types: Vec<_> = args.iter().map(|arg| self.infere_expr(stmt, expr)).collect();

        let mut canditates: Vec<_> = signatures.keys().collect();
        let mut new_candidates = Vec::new();
        let mut errors = Vec::new();
        for (i, (arg, ty)) in zip(args, arg_types).enumerate() {
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

        if !errors.is_empty() {
            self.result.diagnostics.push(
                SignatureMissmatch {
                    type_missmatches: errors.into_boxed_slice(),
                    signatures: signatures.clone(),
                }
                .into(),
            );
        }

        let res = match canditates.as_slice() {
            [] => {
                debug_assert!(false);
                return None;
            }
            [res] => *res,
            _ => {
                canditates.retain(|candidate| {
                    zip(arg_types, signatures[*candidate].args.as_ref())
                        .all(|(ty, req)| ty.map_or(false, |ty| ty.satisfies_exact(req)))
                });
                if let [res] = canditates.as_slice() {
                    *res
                } else {
                    return None;
                }
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
            self.result.resolved_signatures.insert(expr, res)
        }

        Some(Ty::Val(signatures[res].return_ty))
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
                        self.result.resolved_signatures.insert(parent_fun, Signature::from(matched))
                    }

                    if let TyRequirement::Val(req) = req[matched] {
                        if let Some(ty) = ty.to_value() {
                            if !req.is_semantically_equivalent(&ty) {
                                self.result.casts.insert(expr, req);
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
        match self.body.stmt_scopes[stmt].resolve_path(self.db, path) {
            Ok(item) => Some(item),
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
        match self.body.stmt_scopes[stmt].resolve_item_path(self.db, path) {
            Ok(item) => Some(item),
            Err(err) => {
                self.result.diagnostics.push(InferenceDiagnostic::PathResolveError { err, expr });
                None
            }
        }
    }
}

pub enum InferenceDiagnostic {
    InvalidContributeDst { e: ExprId, maybe_assign: bool },
    InvalidAssignmentDst { e: ExprId, maybe_contribute: bool },
    PathResolveError { err: PathResolveError, expr: ExprId },
    ArgCntMissmatch { expected: usize, found: usize, expr: ExprId },
    TypeMissmatch(TypeMissmatch),
    SignatureMissmatch(SignatureMissmatch),
    ArrayTypeMissmatch(ArrayTypeMissmatch),
    PotentialOfPortFlow { expr: ExprId, port_flow: ExprId },
}

impl_from!(TypeMissmatch,SignatureMissmatch, ArrayTypeMissmatch for InferenceDiagnostic);

pub enum BranchAccess {
    ExplicitBranchFlow(BranchId),
    ExplicitBranchPot(BranchId),
    ImplicitBranchPot { hi: NodeId, lo: Option<NodeId> },
    ImplicitBranchFlow { hi: NodeId, lo: Option<NodeId> },
    PortFlow(NodeId),
}
