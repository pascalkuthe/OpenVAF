use ahash::AHashMap;
use arena::ArenaMap;
use hir_def::{
    body::Body,
    db::HirDefDB,
    nameres::{
        DefMap, PathResolveError,
        ScopeDefItem::{self, ModuleId},
    },
    BranchId, BuiltIn, Expr, ExprId, FunctionId, ItemTree, Lookup, NodeId, Path, StmtId, Type,
    VarId,
};
use syntax::ast;

pub enum ResolvedFunction {
    User(FunctionId),
    BuiltIn { fun: BuiltIn, signature: usize },
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
    assigment_destination: AHashMap<StmtId, ResolvedFunction>,
    casts: AHashMap<ExprId, Type>,
}

struct Ctx<'a> {
    result: InferenceResult,
    body: &'a Body,
    def_map: &'a DefMap,
    db: &'a dyn HirDefDB,
}

impl Ctx<'_> {
    pub fn infere_stmt(&mut self, stmt: StmtId) {
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
    }

    pub fn infere_expr(&mut self, stmt: ExprId) -> (Type, bool) {}

    pub fn infere_fun_call(
        &self,
        e: ExprId,
        s: StmtId,
        fun: &Path,
        args: &[ExprId],
    ) -> Result<ResolvedFunction, InferenceDiagnostic> {
        let scope = self.body.stmt_scopes[s];
        let def = self
            .def_map
            .resolve_path_in_scope(scope, fun, self.db)
            .map_err(|err| InferenceDiagnostic::PathResolveError { err, e })?;
        match def {
            ScopeDefItem::NatureAccess(access) => {
                let nature = access.0;

            },
            ScopeDefItem::FunctionId(fun) => {
                foo
            },
            ScopeDefItem::BuiltIn(_) => todo!(),
            found => Err(InferenceDiagnostic::PathResolveError {
                err: PathResolveError::ExpectedItemKind {
                    expected: "a function",
                    found: found.item_kind(),
                    name: fun.segments.last().unwrap().to_owned(),
                },
                e,
            }),
        }
    }

    pub fn infere_assigment_dst(
        &self,
        e: ExprId,
        assigment_kind: ast::AssignOp,
        stmt: StmtId,
    ) -> Result<(Type, AsssigmentDestination), InferenceDiagnostic> {
        let scope = self.body.stmt_scopes[stmt];
        match self.body.exprs[e] {
            Expr::Path { ref path, port: false } => {
                let res: Result<VarId, _> =
                    self.def_map.resolve_item_path_in_scope(scope, path, self.db);

                match assigment_kind {
                    ast::AssignOp::Assign => match res {
                        Ok(var) => {
                            let ty = self.db.var_inf(var).ty;
                            Ok((ty, AsssigmentDestination::Var(var)))
                        }
                        Err(err) => Err(InferenceDiagnostic::PathResolveError { err, e }),
                    },
                    ast::AssignOp::Contribute => Err(InferenceDiagnostic::InvalidContributeDst {
                        e,
                        maybe_assign: res.is_ok(),
                    }),
                }
            }
            Expr::Call { fun, args } => {}
            _ => todo!(),
        }
    }

    fn infere_branch_access(
        &self,
        e: ExprId,
        fun: &Path,
        args: &[ExprId],
    ) -> Result<BranchAccess, InferenceDiagnostic> {
        self.infere_fun_call(fun, args);
    }
}

pub enum InferenceDiagnostic {
    InvalidContributeDst { e: ExprId, maybe_assign: bool },
    InvalidAssignmentDst { e: ExprId, maybe_contribute: bool },
    PathResolveError { err: PathResolveError, e: ExprId },
}

pub enum BranchAccess {
    ExplicitBranchFlow(BranchId),
    ExplicitBranchPot(BranchId),
    ImplicitBranchPot { hi: NodeId, lo: Option<NodeId> },
    ImplicitBranchFlow { hi: NodeId, lo: Option<NodeId> },
    PortFlow(NodeId),
}
