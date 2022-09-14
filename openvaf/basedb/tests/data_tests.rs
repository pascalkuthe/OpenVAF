use basedb::diagnostics::{ConsoleSink, DiagnosticSink};
use basedb::{BaseDB, BaseDatabase, VfsPath, VfsStorage};
use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::Config;
use expect_test::expect_file;
use mini_harness::{harness, Result};
use parking_lot::RwLock;
use stdx::{is_va_file, openvaf_test_data, project_root, run_dev_tests};
use syntax::{Parse, SourceFile};
use vfs::{AbsPathBuf, FileId, Vfs, VfsEntry};

use std::path::Path;

#[salsa::database(BaseDatabase)]
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
    pub fn parse_and_check(&self) -> (Parse<SourceFile>, String) {
        let root_file = self.root_file();
        let preprocessor_diagnostics = self.preprocess(root_file).diagnostics;
        let parse = self.parse(root_file);
        let attr_tree = self.lint_attr_tree(root_file);

        let mut buf = Buffer::no_color();
        {
            let mut sink = ConsoleSink::buffer(Config::default(), self, &mut buf);
            sink.annonymize_paths();
            sink.add_diagnostics(&*preprocessor_diagnostics, root_file, self);
            sink.add_diagnostics(parse.errors(), root_file, self);
            sink.add_diagnostics(&*attr_tree.diagnostics, root_file, self);
        }
        let data = buf.into_inner();
        let diagnostics = String::from_utf8(data).unwrap();
        (parse, diagnostics)
    }
}

/// This impl tells salsa where to find the salsa runtime.
impl salsa::Database for TestDataBase {}
impl VfsStorage for TestDataBase {
    fn vfs(&self) -> &RwLock<Vfs> {
        self.vfs()
    }
}

fn integration_test(dir: &Path) -> Result {
    let name = dir.file_name().unwrap().to_str().unwrap().to_lowercase();
    let main_file = dir.join(format!("{name}.va"));

    let db = TestDataBase::new_from_fs(&main_file);
    let (_, actual) = db.parse_and_check();

    expect_file![dir.join("parser_diagnostics.log")].assert_eq(&actual);

    Ok(())
}

fn ui_test(file: &Path) -> Result {
    let db = TestDataBase::new_from_fs(file);
    let (_, actual) = db.parse_and_check();

    expect_file![file.with_extension("log")].assert_eq(&actual);

    Ok(())
}

fn ast_test(file: &Path) -> Result {
    let db = TestDataBase::new_from_fs(file);
    let (_, actual) = db.parse_and_check();

    expect_file![file.with_extension("va_ast")].assert_eq(&actual);

    Ok(())
}

harness! {
    Test::from_dir_filtered("integration", &integration_test, &run_dev_tests, &project_root().join("integration_tests")),
    Test::from_dir_filtered("ui", &ui_test, &is_va_file, &openvaf_test_data("syn_ui")),
    Test::from_dir_filtered("ast", &ast_test, &is_va_file, &openvaf_test_data("ast"))
}
