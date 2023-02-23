use std::path::Path;

use basedb::diagnostics::sink::Buffer;
use basedb::diagnostics::{Config, ConsoleSink, DiagnosticSink};
use basedb::{
    AbsPathBuf, BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsEntry, VfsPath, VfsStorage,
};
use expect_test::expect_file;
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_def::nameres::{DefMap, LocalScopeId, ScopeDefItem, ScopeOrigin};
use hir_def::DefWithBodyId;
use mini_harness::{harness, Result};
use parking_lot::RwLock;
use stdx::{is_va_file, openvaf_test_data, project_root, run_dev_tests};

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase)]
pub struct TestDataBase {
    storage: salsa::Storage<TestDataBase>,
    vfs: Option<RwLock<Vfs>>,
    root_file: Option<FileId>,
}

impl TestDataBase {
    pub fn new(root_file_path: VfsPath, root_file: VfsEntry) -> Self {
        let mut res = Self { storage: salsa::Storage::default(), vfs: None, root_file: None };
        let vfs = RwLock::new(Vfs::default());
        let db: &mut dyn BaseDB = &mut res;
        let root_file = db.setup_test_db(root_file_path, root_file, &mut vfs.write());
        res.root_file = Some(root_file);
        res.vfs = Some(vfs);
        res
    }

    pub fn new_from_fs(path: &Path) -> Self {
        let path = AbsPathBuf::assert(path.canonicalize().unwrap());
        let file_contents = std::fs::read(&path);
        TestDataBase::new(path.into(), file_contents.into())
    }

    pub fn root_file(&self) -> FileId {
        self.root_file.unwrap()
    }
    pub fn vfs(&self) -> &RwLock<Vfs> {
        self.vfs.as_ref().unwrap()
    }

    pub fn lower_and_check(&self) -> String {
        let root_file = self.root_file();
        let def_map = self.def_map(root_file);
        let mut buf = Buffer::no_color();
        {
            let mut sink = ConsoleSink::buffer(Config::default(), self, &mut buf);
            sink.annonymize_paths();
            let root_scope = def_map.entry();
            self.lower_and_check_rec(root_scope, &def_map, &mut sink);
        }
        let data = buf.into_inner();
        String::from_utf8(data).unwrap()
    }

    fn lower_and_check_rec(&self, scope: LocalScopeId, def_map: &DefMap, dst: &mut ConsoleSink) {
        let root_file = self.root_file();

        for (_, declaration) in &def_map[scope].declarations {
            if let Ok(id) = (*declaration).try_into() {
                let diagnostics = &self.body_source_map(id).diagnostics;
                dst.add_diagnostics(diagnostics, root_file, self);
            }
            if let ScopeDefItem::FunctionId(fun) = *declaration {
                let def_map = self.function_def_map(fun);
                let entry = self.function_def_map(fun).entry();
                self.lower_and_check_rec(entry, &def_map, dst)
            }
        }

        for (_, child) in &def_map[scope].children {
            self.lower_and_check_rec(*child, def_map, dst)
        }
    }
}

/// This impl tells salsa where to find the salsa runtime.
impl salsa::Database for TestDataBase {}
impl VfsStorage for TestDataBase {
    fn vfs(&self) -> &RwLock<Vfs> {
        self.vfs()
    }
}
impl Upcast<dyn BaseDB> for TestDataBase {
    fn upcast(&self) -> &(dyn BaseDB + 'static) {
        self
    }
}

fn integration_test(dir: &Path) -> Result {
    let name = dir.file_name().unwrap().to_str().unwrap().to_lowercase();
    let main_file = dir.join(format!("{name}.va"));

    let db = TestDataBase::new_from_fs(&main_file);
    let diagnostics = db.lower_and_check();
    expect_file![dir.join("hir_def.log")].assert_eq(&diagnostics);

    Ok(())
}

fn body_test(file: &Path) -> Result {
    let db = TestDataBase::new_from_fs(file);
    let def_map = db.def_map(db.root_file());

    let mut actual = String::new();
    for (_, scope) in &def_map[def_map.entry()].children {
        if let ScopeOrigin::Module(module) = def_map[*scope].origin {
            let analog_block = DefWithBodyId::ModuleId { initial: false, module };
            actual.push_str(&db.body(analog_block).dump(&db));
            for (_, scope) in &def_map[*scope].children {
                if let ScopeOrigin::Function(func) = def_map[*scope].origin {
                    actual.push_str(&db.body(func.into()).dump(&db))
                }
            }
        }
    }
    expect_file![file.with_extension("body")].assert_eq(&actual);

    Ok(())
}

fn item_tree_test(file: &Path) -> Result {
    let db = TestDataBase::new_from_fs(file);
    let actual = db.item_tree(db.root_file()).dump();
    expect_file![file.with_extension("item_tree")].assert_eq(&actual);
    Ok(())
}

fn def_map_test(file: &Path) -> Result {
    let db = TestDataBase::new_from_fs(file);
    let actual = db.def_map(db.root_file()).dump(&db);
    expect_file![file.with_extension("def_map")].assert_eq(&actual);
    Ok(())
}

harness! {
    Test::from_dir_filtered("integration", &integration_test, &run_dev_tests, &project_root().join("integration_tests")),
    Test::from_dir_filtered("body", &body_test, &is_va_file, &openvaf_test_data("body")),
    Test::from_dir_filtered("item_tree", &item_tree_test, &is_va_file, &openvaf_test_data("item_tree")),
    Test::from_dir_filtered("def_map", &def_map_test, &is_va_file, &openvaf_test_data("item_tree"))
}
