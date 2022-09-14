use std::path::Path;

use basedb::diagnostics::sink::Buffer;
use basedb::diagnostics::{Config, ConsoleSink};
use basedb::{
    AbsPathBuf, BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsEntry, VfsPath, VfsStorage,
};
use expect_test::expect_file;
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_def::nameres::ScopeDefItem;
use hir_lower::{MirBuilder, PlaceKind};
use hir_ty::collect_diagnostics;
use hir_ty::db::HirTyDatabase;
use lasso::Rodeo;
use mini_harness::{harness, Result};
use mir_build::FunctionBuilderContext;
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

    pub fn check(&self) -> String {
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

    fn lower(&self) {
        let root_file = self.root_file();
        let def_map = self.def_map(root_file);
        let root_scope = def_map.root();
        let mut ctx = FunctionBuilderContext::default();
        for (_, declaration) in &def_map[root_scope].declarations {
            if let ScopeDefItem::ModuleId(id) = *declaration {
                let mut required_vars = [].into_iter();
                let builder = MirBuilder::new(
                    self,
                    id.into(),
                    &|kind| {
                        matches!(
                            kind,
                            PlaceKind::Contribute { .. } | PlaceKind::ImplicitResidual { .. }
                        )
                    },
                    &mut required_vars,
                )
                .with_ctx(&mut ctx);

                builder.build(&mut Rodeo::new());
            }
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

impl Upcast<dyn HirDefDB> for TestDataBase {
    fn upcast(&self) -> &(dyn HirDefDB + 'static) {
        self
    }
}

fn integration_test(dir: &Path) -> Result {
    let name = dir.file_name().unwrap().to_str().unwrap().to_lowercase();
    let main_file = dir.join(format!("{name}.va"));

    let db = TestDataBase::new_from_fs(&main_file);
    db.lower();

    Ok(())
}

fn mir_test(file: &Path) -> Result {
    let db = TestDataBase::new_from_fs(file);
    let def_map = db.def_map(db.root_file());
    assert_eq!(db.check(), "");

    let def = *def_map[def_map.root()]
        .declarations
        .values()
        .find_map(|def| if let ScopeDefItem::ModuleId(id) = def { Some(id) } else { None })
        .unwrap();
    let def = def.into();

    let mut empty_iter = [].into_iter();
    let mut literals = Rodeo::new();
    let mir = MirBuilder::new(
        &db,
        def,
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
    Test::from_dir_filtered("integration", &integration_test, &run_dev_tests, &project_root().join("integration_tests")),
    Test::from_dir_filtered("mir", &mir_test, &is_va_file, &openvaf_test_data("mir"))
}
