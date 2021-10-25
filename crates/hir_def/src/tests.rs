use crate::{
    db::{HirDefDB, HirDefDatabase, InternDatabase},
    nameres::{DefMap, ScopeId},
};
use basedb::{lints::LintResolver, BaseDB, BaseDatabase, FileId, Vfs, VfsStorage};
use data_structures::sync::RwLock;

mod integration;

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase)]
pub struct TestDataBase {
    storage: salsa::Storage<TestDataBase>,
    vfs: Option<RwLock<Vfs>>,
    root_file: Option<FileId>,
}

impl TestDataBase {
    pub fn new(root_file_name: &str, root_file: &str) -> Self {
        let mut res = Self { storage: salsa::Storage::default(), vfs: None, root_file: None };
        let vfs = RwLock::new(Vfs::default());
        let foo: &mut dyn BaseDB = &mut res;
        let root_file = foo.setup_test_db(root_file_name, root_file, &mut vfs.borrow_mut());
        res.root_file = Some(root_file);
        res.vfs = Some(vfs);
        res
    }

    pub fn root_file(&self) -> FileId {
        self.root_file.unwrap()
    }
    pub fn vfs(&self) -> &RwLock<Vfs> {
        self.vfs.as_ref().unwrap()
    }
    pub fn lower_and_check(&self) {
        let root_file = self.root_file();
        let def_map = self.def_map(root_file);
        // TODO assert that the diagnostics are empty
        let root_scope = def_map.root();
        self.lower_and_check_rec(root_scope, &def_map);
    }

    fn lower_and_check_rec(&self, scope: ScopeId, def_map: &DefMap) {
        let root_file = self.root_file();

        for (_, declaration) in &def_map[scope].declarations {
            if let Ok(id) = (*declaration).try_into() {
                self.analog_behaviour(root_file, id);
            }

            if let Ok(id) = (*declaration).try_into() {
                self.param_body(root_file, id);
            }

            if let Ok(id) = (*declaration).try_into() {
                self.expr_body(root_file, id);
            }
        }

        for (_, child) in &def_map[scope].children {
            self.lower_and_check_rec(*child, def_map)
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
impl LintResolver for TestDataBase {}
