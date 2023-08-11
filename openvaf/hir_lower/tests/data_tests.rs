use std::path::Path;

use basedb::AbsPathBuf;
use expect_test::expect_file;
use hir::CompilationDB;
use hir_lower::{MirBuilder, PlaceKind};
use lasso::Rodeo;
use mini_harness::{harness, Result};
use mir_build::FunctionBuilderContext;
use stdx::{ignore_dev_tests, ignore_never, is_va_file, openvaf_test_data, project_root};

fn lower(db: &CompilationDB) {
    let unit = db.compilation_unit();
    let mut ctx = FunctionBuilderContext::default();
    for module in unit.modules(db) {
        let mut required_vars = [].into_iter();
        MirBuilder::new(
            db,
            module,
            &|kind| {
                matches!(kind, PlaceKind::Contribute { .. } | PlaceKind::ImplicitResidual { .. })
            },
            &mut required_vars,
        )
        .with_ctx(&mut ctx)
        .build(&mut Rodeo::new());
    }
}
fn integration_test(dir: &Path) -> Result {
    let name = dir.file_name().unwrap().to_str().unwrap().to_lowercase();
    let main_file = dir.join(format!("{name}.va")).canonicalize().unwrap();
    let db = CompilationDB::new_fs(AbsPathBuf::assert(main_file), &[], &[], &[]).unwrap();
    lower(&db);
    Ok(())
}

fn mir_test(file: &Path) -> Result {
    let db = CompilationDB::new_fs(AbsPathBuf::assert(file.canonicalize().unwrap()), &[], &[], &[])
        .unwrap();
    assert_eq!(db.compilation_unit().test_diagnostics(&db), "");

    let module = db.compilation_unit().modules(&db)[0];
    let mut empty_iter = [].into_iter();
    let mut literals = Rodeo::new();
    let mir = MirBuilder::new(
        &db,
        module,
        &|kind| {
            matches!(
                kind,
                PlaceKind::Contribute { .. }
                    | PlaceKind::ImplicitResidual { .. }
                    | PlaceKind::Var(_)
            )
        },
        &mut empty_iter,
    )
    .build(&mut literals);

    expect_file![file.with_extension("mir")].assert_eq(&mir.0.to_debug_string());
    Ok(())
}

harness! {
    Test::from_dir_filtered("integration", &integration_test, &Path::is_dir, &ignore_dev_tests, &project_root().join("integration_tests")),
    Test::from_dir_filtered("mir", &mir_test, &is_va_file, &ignore_never, &openvaf_test_data("mir"))
}
