mod parser_integration;
mod parser_unit_test;

use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::Config;
use parking_lot::RwLock;
use quote::{format_ident, quote};
use sourcegen::{
    add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat,
};
use syntax::{Parse, SourceFile};
use vfs::{FileId, Vfs, VfsEntry};

use crate::diagnostics::{ConsoleSink, DiagnosticSink};
use crate::{BaseDB, BaseDatabase, VfsStorage};

#[salsa::database(BaseDatabase)]
pub struct TestDataBase {
    storage: salsa::Storage<TestDataBase>,
    vfs: Option<RwLock<Vfs>>,
    root_file: Option<FileId>,
}

impl TestDataBase {
    pub fn new(root_file_name: &str, root_file: VfsEntry) -> Self {
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
    pub fn parse_and_check(&self) -> (Parse<SourceFile>, String) {
        // let filter = tracing_subscriber::EnvFilter::from_env("OPENVAF_TEST_LOG");
        // let printer = tracing_subscriber::fmt::layer().compact().without_time().with_ansi(true);
        // let subscriber = tracing_subscriber::Registry::default().with(filter).with(printer);
        // let _guard = tracing::subscriber::set_default(subscriber);

        let root_file = self.root_file();
        let preprocessor_diagnostics = self.preprocess(root_file).diagnostics;
        let parse = self.parse(root_file);
        let attr_tree = self.lint_attr_tree(root_file);

        let mut buf = Buffer::no_color();
        {
            let mut sink = ConsoleSink::buffer(Config::default(), self, &mut buf);
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

#[test]
pub fn generate_integration_tests() {
    let tests = collect_integration_tests();
    let file = project_root().join("crates/basedb/src/tests/parser_integration.rs");
    let test_impl = tests.into_iter().map(|(test_name,files)|{
        let root_file = format!("/{}.va",test_name.to_lowercase());
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
                let db = TestDataBase::new(#root_file,Default::default());
                let mut vfs = db.vfs().write();
                #(
                    let path = project_root().join("integration_tests").join(#test_name).join(#files);
                    vfs.add_virt_file(#file_names, read(path).into());
                )*
                drop(vfs);
                let (_, actual) = db.parse_and_check();
                expect_file![project_root().join("integration_tests").join(#test_name).join("parser_diagnostics.log")].assert_eq(&actual);
            }
        }
    });

    let header = "use std::fs::read;

        use expect_test::expect_file;
        use sourcegen::{skip_slow_tests,project_root};

        use crate::tests::TestDataBase;
    ";

    let file_string = quote!(
        #(#test_impl)*
    );
    let file_string = format!("{}\n{}", header, file_string);
    let file_string = add_preamble("generate_integration_tests", reformat(file_string));
    ensure_file_contents(&file, &file_string);
}
