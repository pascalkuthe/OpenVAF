use basedb::AstIdMap;
use hir_def::db::HirDefDB;
use hir_def::nameres::diagnostics::DefDiagnosticWrapped;
use hir_def::nameres::{DefMap, LocalScopeId, ScopeDefItem, ScopeOrigin};
use hir_def::DefWithBodyId;
use hir_ty::diagnostics::InferenceDiagnosticWrapped;
use hir_ty::validation::{
    self, BodyValidationDiagnostic, BodyValidationDiagnosticWrapped,
    TypeValidationDiagnosticWrapped,
};
use syntax::sourcemap::SourceMap;
use syntax::{Parse, SourceFile};

pub use basedb::diagnostics::*;
pub use basedb::{BaseDB, FileId};

use crate::{CompilationDB, HirDatabase};

pub(crate) fn collect(db: &CompilationDB, root_file: FileId, sink: &mut impl DiagnosticSink) {
    sink.add_diagnostics(&*db.preprocess(root_file).diagnostics, root_file, db);
    sink.add_diagnostics(db.parse(root_file).errors(), root_file, db);

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
        sink.add_diagnostic(&diag, root_file, db);
    }

    collect_def_map(db, &def_map, root_file, &parse, &sm, &ast_id_map, sink);
    let root_scope = def_map.root();
    for child in def_map[root_scope].children.values() {
        if let ScopeOrigin::Module(module) = def_map[*child].origin {
            collect_body_diagnostcs(
                db,
                sink,
                DefWithBodyId::ModuleId { initial: true, module },
                &parse,
                &sm,
                root_file,
                &ast_id_map,
            );
            collect_body_diagnostcs(
                db,
                sink,
                DefWithBodyId::ModuleId { initial: false, module },
                &parse,
                &sm,
                root_file,
                &ast_id_map,
            )
        }

        collect_scope(db, &def_map, &parse, &sm, &ast_id_map, root_file, sink, *child)
    }
}

// FIXME bundle required syntax info into struct in BaseDB
#[allow(clippy::too_many_arguments)]
fn collect_scope(
    db: &CompilationDB,
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

        collect_scope(db, &def_map, parse, sm, ast_id_map, root_file, dst, def_map.entry());
        collect_def_map(db, &def_map, root_file, parse, sm, ast_id_map, dst);
    }
}

fn collect_def_map(
    db: &dyn HirDatabase,
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
    db: &dyn HirDatabase,
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
