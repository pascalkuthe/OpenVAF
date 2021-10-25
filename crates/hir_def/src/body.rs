use std::sync::Arc;

use basedb::FileId;
use data_structures::{HashMap, arena::{Arena, ArenaMap}, index_vec::IndexVec, iter::Itertools};
use syntax::{ast, AstNode, AstPtr};

use crate::{
    db::HirDefDB, item_tree::ItemTreeNode, nameres::ScopeId, DefWithBehaviourId, DefWithExprId,
    DisciplineAttrLoc, Expr, ExprId, Lookup, ModuleLoc, NatureAttrLoc, ParamId, ParamLoc, FunctionLoc,
    Stmt, StmtId, VarLoc,
};

use self::lower::LowerCtx;

mod lower;

/// The body of an item (function, const etc.).
#[derive(Debug, Eq, PartialEq, Default)]
pub struct Body {
    pub exprs: Arena<Expr>,
    pub stmt_scopes: ArenaMap<Stmt, ScopeId>,
    pub stmts: Arena<Stmt>,
}

#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    expr_map: HashMap<AstPtr<ast::Expr>, ExprId>,
    expr_map_back: IndexVec<ExprId, Option<AstPtr<ast::Expr>>>,
    stmt_map: HashMap<AstPtr<ast::Stmt>, StmtId>,
    stmt_map_back: IndexVec<StmtId, Option<AstPtr<ast::Stmt>>>,
    /// Diagnostics accumulated during body lowering. These contain `AstPtr`s and so are stored in
    /// the source map (since they're just as volatile).
    diagnostics: Vec<BodyDiagnostic>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BodyDiagnostic {
    InfOutsideParamBound { ptr: AstPtr<ast::Literal> },
}

#[derive(Default, Debug, Eq, PartialEq)]
pub struct AnalogBehaviour {
    pub body: Body,
    pub root_stmts: Vec<StmtId>,
}

impl AnalogBehaviour {
    pub fn body_with_sourcemap_query(
        db: &dyn HirDefDB,
        root_file: FileId,
        id: DefWithBehaviourId,
    ) -> (Arc<AnalogBehaviour>, Arc<BodySourceMap>) {
        let mut body = Body::default();
        let mut source_map = BodySourceMap::default();

        let tree = db.item_tree(root_file);
        let def_map = db.def_map(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let (scope, stmts) = match id {
            DefWithBehaviourId::Module(id) => {
                let ModuleLoc { scope, item_tree } = id.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                (scope, ast.analog_behaviour().collect_vec())
            },

            DefWithBehaviourId::Function(id) => {
                let FunctionLoc { scope, item_tree } = id.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                (scope, ast.body().collect_vec())
            }
,
        };

        let mut ctx = LowerCtx {
            db,
            source_map: &mut source_map,
            body: &mut body,
            def_map: &def_map,
            tree: &tree,
            ast_id_map: &ast_id_map,
            curr_scope: scope,
        };

        let root_stmts = stmts.into_iter().map(|stmt| ctx.collect_stmt(stmt)).collect();

        let body = Self { root_stmts, body };
        (Arc::new(body), Arc::new(source_map))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParamBody {
    pub body: Body,
    pub default: ExprId,
    pub bounds: Vec<ParamBounds>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Range {
    pub start: ExprId,
    pub start_inclusive: bool,
    pub end: ExprId,
    pub end_inclusive: bool,
}

impl Range {
    fn new(ast: ast::Range, ctx: &mut LowerCtx) -> Self {
        Range {
            start: ctx.collect_opt_expr(ast.start()),
            start_inclusive: ast.start_inclusive(),
            end: ctx.collect_opt_expr(ast.end()),
            end_inclusive: ast.end_inclusive(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParamBounds {
    Exclud(ExprId),
    ExcludeRange(Range),
    From(Range),
}

impl ParamBody {
    pub fn body_with_sourcemap_query(
        db: &dyn HirDefDB,
        root_file: FileId,
        id: ParamId,
    ) -> (Arc<ParamBody>, Arc<BodySourceMap>) {
        let mut body = Body::default();
        let mut source_map = BodySourceMap::default();

        let tree = db.item_tree(root_file);
        let def_map = db.def_map(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let ParamLoc { item_tree, scope } = id.lookup(db);
        let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());

        let mut ctx = LowerCtx {
            db,
            source_map: &mut source_map,
            body: &mut body,
            def_map: &def_map,
            tree: &tree,
            ast_id_map: &ast_id_map,
            curr_scope: scope,
        };

        let default = ctx.collect_opt_expr(ast.default());
        let bounds = ast
            .constraints()
            .filter_map(|constraint| {
                let constraint = match constraint.kind()? {
                    ast::ConstraintKind::From(range) => {
                        ParamBounds::From(Range::new(range, &mut ctx))
                    }
                    ast::ConstraintKind::Exclude(expr) => {
                        ParamBounds::Exclud(ctx.collect_expr(expr))
                    }
                    ast::ConstraintKind::ExcludeRange(range) => {
                        ParamBounds::ExcludeRange(Range::new(range, &mut ctx))
                    }
                };
                Some(constraint)
            })
            .collect();

        let body = ParamBody { body, bounds, default };
        (Arc::new(body), Arc::new(source_map))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ExprBody {
    pub body: Body,
    pub val: ExprId,
}

impl ExprBody {
    pub fn body_with_sourcemap_query(
        db: &dyn HirDefDB,
        root_file: FileId,
        def: DefWithExprId,
    ) -> (Arc<ExprBody>, Arc<BodySourceMap>) {
        let mut body = Body::default();
        let mut source_map = BodySourceMap::default();

        let def_map = db.def_map(root_file);
        let tree = db.item_tree(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let (expr, scope) = match def {
            DefWithExprId::Var(var) => {
                let VarLoc { scope, item_tree } = var.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                (ast.default(), scope)
            }
            DefWithExprId::NatureAttr(attr) => {
                let NatureAttrLoc { scope, item_tree } = attr.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                (ast.val(), scope)
            }
            DefWithExprId::DisciplineAttr(attr) => {
                let DisciplineAttrLoc { scope, item_tree } = attr.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                (ast.val(), scope)
            }
        };

        let mut ctx = LowerCtx {
            db,
            source_map: &mut source_map,
            body: &mut body,
            def_map: &def_map,
            tree: &tree,
            ast_id_map: &ast_id_map,
            curr_scope: scope,
        };

        let val = ctx.collect_opt_expr(expr);
        let body = ExprBody { body, val };
        (Arc::new(body), Arc::new(source_map))
    }
}
