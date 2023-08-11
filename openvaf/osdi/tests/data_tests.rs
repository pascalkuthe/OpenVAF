use std::path::Path;

use camino::Utf8Path;
use hir::diagnostics::ConsoleSink;
use hir::CompilationDB;
use llvm::OptLevel;
use mini_harness::{harness, Result};
use mir_llvm::LLVMBackend;
use paths::AbsPathBuf;
use sim_back::collect_modules;
use stdx::{ignore_slow_tests, project_root};
use target::spec::Target;

fn test_compile(root_file: &Path) {
    let root_file = AbsPathBuf::assert(root_file.canonicalize().unwrap());
    let db = CompilationDB::new_fs(root_file, &[], &[], &[]).unwrap();
    let modules = collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap();
    let target = Target::host_target().unwrap();
    let back = LLVMBackend::new(&[], &target, "native".to_owned(), &[]);
    let emit = !stdx::IS_CI;
    osdi::compile(&db, &modules, Utf8Path::new("foo.o"), &target, &back, emit, OptLevel::None);
}

fn integration_test(dir: &Path) -> Result {
    let name = dir.file_name().unwrap().to_str().unwrap().to_lowercase();
    let main_file = dir.join(format!("{name}.va"));
    test_compile(&main_file);

    Ok(())
}

harness! {
    Test::from_dir("integration", &integration_test, &ignore_slow_tests, &project_root().join("integration_tests"))
}
