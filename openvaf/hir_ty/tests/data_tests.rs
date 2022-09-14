use std::path::Path;

use basedb::diagnostics::sink::Buffer;
use basedb::diagnostics::{Config, ConsoleSink};
use basedb::{
    AbsPathBuf, BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsEntry, VfsPath, VfsStorage,
};
use expect_test::expect_file;
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_ty::collect_diagnostics;
use hir_ty::db::HirTyDatabase;
use mini_harness::{harness, Result};
use parking_lot::RwLock;
use stdx::{is_va_file, openvaf_test_data, project_root, run_dev_tests};

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase, HirTyDatabase)]
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

        let mut buf = Buffer::no_color();
        {
            let mut sink = ConsoleSink::buffer(Config::default(), self, &mut buf);
            sink.annonymize_paths();
            collect_diagnostics(self, root_file, &mut sink);
        }
        let data = buf.into_inner();
        String::from_utf8(data).unwrap()
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

impl Upcast<dyn HirDefDB> for TestDataBase {
    fn upcast(&self) -> &(dyn HirDefDB + 'static) {
        self
    }
}

fn integration_test(dir: &Path) -> Result {
    let name = dir.file_name().unwrap().to_str().unwrap().to_lowercase();
    let main_file = dir.join(format!("{name}.va"));

    let db = TestDataBase::new_from_fs(&main_file);
    let diagnostics = db.lower_and_check();
    expect_file![dir.join("frontend.log")].assert_eq(&diagnostics);

    Ok(())
}

fn ui_test(file: &Path) -> Result {
    let db = TestDataBase::new_from_fs(file);
    let actual = db.lower_and_check();
    expect_file![file.with_extension("log")].assert_eq(&actual);
    Ok(())
}

harness! {
    Test::from_dir_filtered("integration", &integration_test, &run_dev_tests, &project_root().join("integration_tests")),
    Test::from_dir_filtered("ui", &ui_test, &is_va_file, &openvaf_test_data("ui"))
}
