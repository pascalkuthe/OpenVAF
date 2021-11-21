mod parser_integration;
mod parser_unit_test;

use parking_lot::RwLock;
use quote::{format_ident, quote};
use syntax::{Parse, Preprocess, SourceFile};
use vfs::{FileId, Vfs};

use crate::{
    diagnostics::assert_empty_diagnostics, lints::LintResolver, BaseDB, BaseDatabase, VfsStorage,
};

use sourcegen::{add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat};

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
        let root_file = foo.setup_test_db(root_file_name, root_file, &mut vfs.write());
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
        // let filter = tracing_subscriber::EnvFilter::from_env("OPENVAF_TEST_LOG");
        // let printer = tracing_subscriber::fmt::layer().compact().without_time().with_ansi(true);
        // let subscriber = tracing_subscriber::Registry::default().with(filter).with(printer);
        // let _guard = tracing::subscriber::set_default(subscriber);

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

#[test]
pub fn generate_integration_tests() {
    let tests = collect_integration_tests();
    let file = project_root().join("crates/basedb/src/tests/parser_integration.rs");
    let test_impl = tests.map(|(test_name,files)|{
        let root_file_path = project_root()
            .join(format!("integration_tests/{}/{}.va", test_name, test_name.to_lowercase()))
            .to_str().unwrap().to_owned();
        let file_names = files
            .iter()
            .map(|file| format!("/{}", file))
        ;
        let test_case = format_ident!("{}",test_name.to_lowercase());

        quote! {

            #[test]
            fn #test_case(){
                if skip_slow_tests(){
                    return
                }
                let root_file = read_to_string(PathBuf::from(#root_file_path)).unwrap();
                let db = TestDataBase::new("/root.va",&root_file);
                #(
                    {
                        let path = project_root().join("integration_tests").join(#test_name).join(#files);
                        let file_contents =read_to_string(path).unwrap();
                        db.vfs().write().add_virt_file(#file_names, &file_contents);
                    }
                )*
                db.parse_and_check();
            }
        }
    });

    let file_string = quote!(
        use crate::{tests::TestDataBase};
        use sourcegen::{skip_slow_tests,project_root};
        use std::{fs::read_to_string,path::PathBuf};
        #(#test_impl)*
    )
    .to_string();

    let file_string = add_preamble(
        "generate_integration_tests",
        reformat(file_string),
    );

    ensure_file_contents(&file, &file_string);
}
