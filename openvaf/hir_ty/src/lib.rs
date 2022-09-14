pub mod builtin;
pub mod db;
pub mod diagnostics;
pub mod inference;
pub mod lower;
pub mod types;
pub mod validation;

use std::iter::once;
use std::ops::Deref;

use basedb::diagnostics::DiagnosticSink;
use basedb::{AstIdMap, FileId};
use hir_def::db::HirDefDB;
use hir_def::nameres::diagnostics::DefDiagnosticWrapped;
use hir_def::nameres::{DefMap, LocalScopeId, ScopeDefItem, ScopeOrigin};
use hir_def::{DefWithBodyId, ScopeId};
pub use lower::{BranchTy, DisciplineTy, NatureTy};
use smol_str::SmolStr;
use syntax::name::Name;
use syntax::sourcemap::SourceMap;
use syntax::{Parse, SourceFile};

use crate::db::HirTyDB;
use crate::diagnostics::InferenceDiagnosticWrapped;
use crate::validation::{
    BodyValidationDiagnostic, BodyValidationDiagnosticWrapped, TypeValidationDiagnosticWrapped,
};

pub fn collect_diagnostics(db: &dyn HirTyDB, root_file: FileId, dst: &mut impl DiagnosticSink) {
    let def_map = db.def_map(root_file);
    let ast_id_map = db.ast_id_map(root_file);
    let parse = db.parse(root_file);
    let sm = db.sourcemap(root_file);
    let item_tree = db.item_tree(root_file);

    let diagnostics = validation::TypeValidationDiagnostic::collect(db, root_file);

    for diag in diagnostics {
        let diag = TypeValidationDiagnosticWrapped {
            diag: &diag,
            parse: &parse,
            db,
            sm: &sm,
            map: &ast_id_map,
            item_tree: &item_tree,
        };
        dst.add_diagnostic(&diag, root_file, db.upcast());
    }

    collect_def_map_diagnostics(db, &def_map, root_file, &parse, &sm, &ast_id_map, dst);
    let root_scope = def_map.root();
    for child in def_map[root_scope].children.values() {
        if let ScopeOrigin::Module(module) = def_map[*child].origin {
            collect_body_diagnostcs(db, dst, module.into(), &parse, &sm, root_file, &ast_id_map)
        }

        collect_scope_diagnostc(db, &def_map, &parse, &sm, &ast_id_map, root_file, dst, *child)
    }
}

// FIXME bundle required syntax info into struct in BaseDB
#[allow(clippy::too_many_arguments)]

fn collect_scope_diagnostc(
    db: &dyn HirTyDB,
    def_map: &DefMap,
    parse: &Parse<SourceFile>,
    sm: &SourceMap,
    ast_id_map: &AstIdMap,
    root_file: FileId,
    dst: &mut impl DiagnosticSink,
    scope: LocalScopeId,
) {
    for def in def_map[scope].declarations.values() {
        if let Ok(def) = (*def).try_into() {
            collect_body_diagnostcs(db, dst, def, parse, sm, root_file, ast_id_map);
        }

        let def_map = match def {
            ScopeDefItem::FunctionId(fun) => db.function_def_map(*fun),
            ScopeDefItem::BlockId(block) => {
                if let Some(def_map) = db.block_def_map(*block) {
                    def_map
                } else {
                    continue;
                }
            }
            _ => continue,
        };

        collect_scope_diagnostc(
            db,
            &def_map,
            parse,
            sm,
            ast_id_map,
            root_file,
            dst,
            def_map.entry(),
        );
        collect_def_map_diagnostics(db, &def_map, root_file, parse, sm, ast_id_map, dst);
    }
}

fn collect_def_map_diagnostics(
    db: &dyn HirTyDB,
    def_map: &DefMap,
    root_file: FileId,
    parse: &Parse<SourceFile>,
    sm: &SourceMap,
    ast_id_map: &AstIdMap,
    dst: &mut impl DiagnosticSink,
) {
    for diag in &def_map.diagnostics {
        let diag = DefDiagnosticWrapped { db: db.upcast(), diag, parse, sm, ast_id_map };
        dst.add_diagnostic(&diag, root_file, db.upcast())
    }
}

fn collect_body_diagnostcs(
    db: &dyn HirTyDB,
    dst: &mut impl DiagnosticSink,
    def: DefWithBodyId,
    parse: &Parse<SourceFile>,
    sm: &SourceMap,
    root_file: FileId,
    ast_id_map: &AstIdMap,
) {
    let body_sm = db.body_source_map(def);
    let diagnostics = &db.inference_result(def).diagnostics;
    for diag in diagnostics {
        let diag = InferenceDiagnosticWrapped { body_sm: &body_sm, diag, parse, db, sm };
        let db: &dyn HirDefDB = db.upcast();
        dst.add_diagnostic(&diag, root_file, db.upcast())
    }

    let diagnostics = BodyValidationDiagnostic::collect(db, def);
    for diag in diagnostics {
        let diag = BodyValidationDiagnosticWrapped {
            body_sm: &body_sm,
            diag: &diag,
            parse,
            db,
            sm,
            map: ast_id_map,
        };
        let db: &dyn HirDefDB = db.upcast();
        dst.add_diagnostic(&diag, root_file, db.upcast())
    }
}

// TODO spin out into HIR wrapper crate

pub fn collect_path(path: &[Name], name: &Name) -> SmolStr {
    if path.is_empty() {
        // fast path
        return name.clone().into();
    }
    path.iter().flat_map(|path| [&*path, "."]).chain(once(name.deref())).collect()
}

pub fn visit_relative_defs(
    db: &dyn HirTyDB,
    scope: ScopeId,
    mut f: impl FnMut(&[Name], &Name, ScopeDefItem),
) {
    fn visit_relative_defs_impl(
        stack: &mut Vec<Name>,
        db: &dyn HirTyDB,
        map: &DefMap,
        local_scope: LocalScopeId,
        f: &mut impl FnMut(&[Name], &Name, ScopeDefItem),
    ) {
        for (name, child) in &map[local_scope].children {
            stack.push(name.clone());
            visit_relative_defs_impl(stack, db, map, *child, f);
            stack.pop();
        }

        for (name, def) in &map[local_scope].declarations {
            f(stack, name, *def);
            let map = match def {
                ScopeDefItem::FunctionId(fun) => db.function_def_map(*fun),
                ScopeDefItem::BlockId(block) => {
                    if let Some(map) = db.block_def_map(*block) {
                        map
                    } else {
                        continue;
                    }
                }
                _ => continue,
            };

            stack.push(name.clone());
            visit_relative_defs_impl(stack, db, &map, map.entry(), f);
            stack.pop();
        }
    }

    let mut stack = Vec::new();
    let map = scope.def_map(db.upcast());
    visit_relative_defs_impl(&mut stack, db, &map, scope.local_scope, &mut f)
}
