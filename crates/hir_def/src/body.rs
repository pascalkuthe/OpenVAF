use std::sync::Arc;

use ahash::AHashMap as HashMap;
use arena::{Arena, ArenaMap};
pub use ast::ConstraintKind;
use basedb::lints::{Lint, LintSrc};
use basedb::{AttrDiagnostic, LintAttrs};
use lasso::Rodeo;
use lower::LowerCtx;
use syntax::{ast, AstNode, AstPtr};

use crate::db::HirDefDB;
use crate::item_tree::{DisciplineAttr, ItemTreeId, ItemTreeNode, NatureAttr};
use crate::nameres::{DefMapSource, LocalScopeId};
use crate::{
    DefWithBehaviourId, DefWithExprId, DisciplineAttrLoc, DisciplineLoc, Expr, ExprId, FunctionLoc,
    Lookup, ModuleLoc, NatureAttrLoc, NatureLoc, ParamId, ParamLoc, ScopeId, Stmt, StmtId, VarLoc,
};

mod lower;

mod pretty;
#[cfg(test)]
mod tests;

/// The body of an item
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
    pub str_lit_interner: Rodeo,
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
        id: DefWithBehaviourId,
    ) -> (Arc<AnalogBehaviour>, Arc<BodySourceMap>) {
        let mut body = Body::default();
        let mut source_map = BodySourceMap::default();

        let root_file = id.file(db);
        let tree = db.item_tree(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let (curr_scope, stmts): (_, Vec<_>) = match id {
            DefWithBehaviourId::ModuleId(id) => {
                let ModuleLoc { scope, id: item_tree } = id.lookup(db);
                let ast_id = tree[item_tree].ast_id();
                let ast = ast_id_map.get(ast_id).to_node(ast.syntax());
                ((scope, ast_id.into()), ast.analog_behaviour().collect())
            }

            DefWithBehaviourId::FunctionId(id) => {
                let FunctionLoc { id: item_tree, .. } = id.lookup(db);
                let scope = ScopeId {
                    root_file,
                    local_scope: LocalScopeId::from(0u32),
                    src: DefMapSource::Function(id),
                };
                debug_assert_eq!(scope.local_scope, db.function_def_map(id).entry());
                let ast_id = tree[item_tree].ast_id();
                let ast = ast_id_map.get(ast_id).to_node(ast.syntax());
                ((scope, ast_id.into()), ast.body().collect())
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
        id: ParamId,
    ) -> (Arc<ParamBody>, Arc<BodySourceMap>) {
        let mut body = Body::default();
        let mut source_map = BodySourceMap::default();
        let root_file = id.lookup(db).scope.root_file;

        let tree = db.item_tree(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let ParamLoc { id: item_tree, scope } = id.lookup(db);
        let ast_id = tree[item_tree].ast_id();
        let ast = ast_id_map.get(ast_id).to_node(ast.syntax());

        let registry = db.lint_registry();
        let mut ctx = LowerCtx {
            db,
            source_map: &mut source_map,
            body: &mut body,
            ast_id_map: &ast_id_map,
            curr_scope: (scope, ast_id.into()),
            registry: &registry,
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

        // Add a dummy stmt so that we can associate scope,lint src etc with the epxressions here;
        ctx.missing_stmt();
        debug_assert_eq!(body.stmts.len(), 1);

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
        def: DefWithExprId,
    ) -> (Arc<ExprBody>, Arc<BodySourceMap>) {
        let mut body = Body::default();
        let mut source_map = BodySourceMap::default();

        let root_file = def.file(db);

        let tree = db.item_tree(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let ast = db.parse(root_file).tree();

        let (expr, scope) = match def {
            DefWithExprId::VarId(var) => {
                let VarLoc { scope, id: item_tree } = var.lookup(db);
                let ast_id = tree[item_tree].ast_id();
                let ast = ast_id_map.get(ast_id).to_node(ast.syntax());
                (ast.default(), (scope, ast_id.into()))
            }
            DefWithExprId::NatureAttrId(attr) => {
                let NatureAttrLoc { nature, id } = attr.lookup(db);
                let NatureLoc { root_file, id: discipline_id } = nature.lookup(db);
                let nature = &tree[discipline_id];
                let idx = usize::from(nature.attrs.start()) + usize::from(id);
                let attr = &tree[ItemTreeId::<NatureAttr>::from(idx)];
                let ast = ast_id_map.get(attr.ast_id).to_node(ast.syntax());
                (ast.val(), (ScopeId::root(root_file), attr.ast_id.into()))
            }
            DefWithExprId::DisciplineAttrId(attr) => {
                let DisciplineAttrLoc { discipline, id } = attr.lookup(db);
                let DisciplineLoc { root_file, id: discipline_id } = discipline.lookup(db);
                let discipline = &tree[discipline_id];
                let idx = usize::from(discipline.extra_attrs.start()) + usize::from(id);
                let attr = &tree[ItemTreeId::<DisciplineAttr>::from(idx)];
                let ast = ast_id_map.get(attr.ast_id).to_node(ast.syntax());
                (ast.val(), (ScopeId::root(root_file), attr.ast_id.into()))
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
        };

        let val = ctx.collect_opt_expr(expr);

        // Add a dummy stmt so that we can associate scope,lint src etc with the epxressions
        ctx.missing_stmt();
        debug_assert_eq!(body.stmts.len(), 1);

        let body = ExprBody { body, val };
        (Arc::new(body), Arc::new(source_map))
    }
}
