use basedb::diagnostics::sink::Buffer;
use basedb::diagnostics::{Config, ConsoleSink, DiagnosticSink};
use basedb::{BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsStorage};
use parking_lot::RwLock;
use quote::{format_ident, quote};
use sourcegen::{
    add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat,
};
use stdx::SKIP_HOST_TESTS;

use crate::db::{HirDefDB, HirDefDatabase, InternDatabase};
use crate::nameres::{DefMap, LocalScopeId, ScopeDefItem};

mod generate_builtins;
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
        let root_file =
            foo.setup_test_db(root_file_name, root_file.to_owned().into(), &mut vfs.write());
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
    pub fn lower_and_check(&self) -> String {
        let root_file = self.root_file();
        let def_map = self.def_map(root_file);
        let mut buf = Buffer::no_color();
        {
            let mut sink = ConsoleSink::buffer(Config::default(), self, &mut buf);
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
                // TODO add diagnostics to sink when implemented
                self.function_def_map(fun);
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

#[test]
pub fn generate_integration_tests() {
    if SKIP_HOST_TESTS{
        return ;
    }
    let tests = collect_integration_tests();
    let file = project_root().join("crates/hir_def/src/tests/integration.rs");
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
                let db = TestDataBase::new(#root_file,"");
                let mut vfs = db.vfs().write();
                #(
                    let path = project_root().join("integration_tests").join(#test_name).join(#files);
                    vfs.add_virt_file(#file_names, read(path).into());
                )*
                drop(vfs);
                let diagnostics = db.lower_and_check();
                assert_eq!(&diagnostics,"");
                let def_map = db.def_map(db.root_file());
                let actual = def_map.dump(&db);
                expect_file![project_root().join("integration_tests").join(#test_name).join("def_map.txt")].assert_eq(&actual);
            }
        }
    });

    let header = "use std::fs::read;

        use expect_test::expect_file;
        use sourcegen::{skip_slow_tests,project_root};

        use crate::tests::TestDataBase;
        use crate::db::HirDefDB;
    ";

    let file_string = quote!(
        #(#test_impl)*
    );
    let file_string = format!("{}\n{}", header, file_string);
    let file_string = add_preamble("generate_integration_tests", reformat(file_string));
    ensure_file_contents(&file, &file_string);
}
