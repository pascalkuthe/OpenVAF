use std::sync::Arc;

use ahash::AHashMap as HashMap;
use arena::{Arena, ArenaMap};
use basedb::{
    lints::{Lint, LintSrc},
    FileId,
};
use syntax::{ast, AstNode, AstPtr};

use crate::{
    attrs::{AttrDiagnostic, LintAttrs},
    db::HirDefDB,
    item_tree::ItemTreeNode,
    DefWithBehaviourId, DefWithExprId, DisciplineAttrLoc, Expr, ExprId, FunctionLoc, Lookup,
    ModuleLoc, NatureAttrLoc, ParamId, ParamLoc, ScopeId, Stmt, StmtId, VarLoc,
};

use lower::LowerCtx;

pub use ast::ConstraintKind;

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
    pub expr_map: HashMap<AstPtr<ast::Expr>, ExprId>,
    pub expr_map_back: ArenaMap<Expr, Option<AstPtr<ast::Expr>>>,
    pub stmt_map: HashMap<AstPtr<ast::Stmt>, StmtId>,
    pub stmt_map_back: ArenaMap<Stmt, Option<AstPtr<ast::Stmt>>>,
    lint_map: ArenaMap<Stmt, LintAttrs>,

    /// Diagnostics accumulated during body lowering. These contain `AstPtr`s and so are stored in
    /// the source map (since they're just as volatile).
    pub diagnostics: Vec<AttrDiagnostic>,
}

impl BodySourceMap {
    pub fn lint_src(&self, stmt: StmtId, lint: Lint) -> LintSrc {
        self.lint_map[stmt].lint_src(lint)
    }
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
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let (curr_scope, stmts): (_, Vec<_>) = match id {
            DefWithBehaviourId::ModuleId(id) => {
                let ModuleLoc { scope, id: item_tree } = id.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                let item_tree = tree[item_tree].erased_id;
                ((scope, item_tree), ast.analog_behaviour().collect())
            }

            DefWithBehaviourId::FunctionId(id) => {
                let FunctionLoc { scope, id: item_tree } = id.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                let item_tree = tree[item_tree].erased_id;
                ((scope, item_tree), ast.body().collect())
            }
        };

        let registry = db.lint_registry();

        let mut ctx = LowerCtx {
            db,
            source_map: &mut source_map,
            body: &mut body,
            ast_id_map: &ast_id_map,
            curr_scope,
            registry: &registry,
            tree: &tree,
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
    pub bounds: Vec<ParamConstraint>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Range {
    pub start: ExprId,
    pub start_inclusive: bool,
    pub end: ExprId,
    pub end_inclusive: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ConstraintValue {
    Value(ExprId),
    Range(Range),
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParamConstraint {
    pub kind: ConstraintKind,
    pub val: ConstraintValue,
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
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let ParamLoc { id: item_tree, scope } = id.lookup(db);
        let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
        let item_tree = tree[item_tree].erased_id;

        let registry = db.lint_registry();
        let mut ctx = LowerCtx {
            db,
            source_map: &mut source_map,
            body: &mut body,
            ast_id_map: &ast_id_map,
            curr_scope: (scope, item_tree),
            registry: &registry,
            tree: &tree,
        };

        let default = ctx.collect_opt_expr(ast.default());
        let bounds = ast
            .constraints()
            .filter_map(|constraint| {
                let kind = constraint.kind()?;
                let val = match constraint.val()? {
                    ast::ConstraintValue::Val(val) => ConstraintValue::Value(ctx.collect_expr(val)),
                    ast::ConstraintValue::Range(range) => ConstraintValue::Range(Range {
                        start: ctx.collect_opt_expr(range.start()),
                        start_inclusive: range.start_inclusive(),
                        end: ctx.collect_opt_expr(range.end()),
                        end_inclusive: range.end_inclusive(),
                    }),
                };
                Some(ParamConstraint { kind, val })
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

        let tree = db.item_tree(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let (expr, scope) = match def {
            DefWithExprId::VarId(var) => {
                let VarLoc { scope, id: item_tree } = var.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                let item_tree = tree[item_tree].erased_id;
                (ast.default(), (scope, item_tree))
            }
            DefWithExprId::NatureAttrId(attr) => {
                let NatureAttrLoc { scope, id: item_tree } = attr.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                let item_tree = tree[item_tree].erased_id;
                (ast.val(), (scope, item_tree))
            }
            DefWithExprId::DisciplineAttrId(attr) => {
                let DisciplineAttrLoc { scope, id: item_tree } = attr.lookup(db);
                let ast = ast_id_map.get(tree[item_tree].ast_id()).to_node(ast.syntax());
                let item_tree = tree[item_tree].erased_id;
                (ast.val(), (scope, item_tree))
            }
        };

        let registry = db.lint_registry();
        let mut ctx = LowerCtx {
            db,
            source_map: &mut source_map,
            body: &mut body,
            ast_id_map: &ast_id_map,
            curr_scope: scope,
            registry: &registry,
            tree: &tree,
        };

        let val = ctx.collect_opt_expr(expr);
        let body = ExprBody { body, val };
        (Arc::new(body), Arc::new(source_map))
    }
}
