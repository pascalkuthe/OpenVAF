mod parser_integration;
mod parser_unit_test;

use data_structures::sync::RwLock;

use syntax::{Parse, Preprocess, SourceFile};
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use vfs::{FileId, Vfs};

use crate::{
    diagnostics::assert_empty_diagnostics, lints::LintResolver, BaseDB, BaseDatabase, VfsStorage,
};

#[salsa::database(BaseDatabase)]
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
    pub fn parse_and_check(&self) -> Parse<SourceFile> {
        let filter = tracing_subscriber::EnvFilter::from_env("OPENVAF_TEST_LOG");
        let printer = tracing_subscriber::fmt::layer().compact().without_time().with_ansi(true);
        let subscriber = tracing_subscriber::Registry::default().with(filter).with(printer);
        let _guard = tracing::subscriber::set_default(subscriber);

        let root_file = self.root_file();
        let Preprocess { diagnostics, .. } = self.preprocess(root_file);
        assert_empty_diagnostics(self, root_file, &diagnostics);
        let parse = self.parse(root_file);
        assert_empty_diagnostics(self, root_file, parse.errors());
        parse
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
