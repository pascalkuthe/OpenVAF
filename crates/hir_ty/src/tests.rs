use basedb::{BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsStorage};
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_def::nameres::{DefMap, LocalScopeId};
use parking_lot::RwLock;
use quote::{format_ident, quote};
use sourcegen::{
    add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat,
};
use stdx::format_to;

use crate::db::{HirTyDB, HirTyDatabase};
use crate::validation;

mod integration;

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase, HirTyDatabase)]
pub struct TestDataBase {
    storage: salsa::Storage<TestDataBase>,
    vfs: Option<RwLock<Vfs>>,
    root_file: Option<FileId>,
}

impl Upcast<dyn HirDefDB> for TestDataBase {
    fn upcast(&self) -> &(dyn HirDefDB + 'static) {
        &*self
    }
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
        let mut dst = String::new();

        let diagnostics = validation::TypeValidationDiagnostic::collect(self, root_file);
        if !diagnostics.is_empty() {
            format_to!(dst, "{:#?}", diagnostics);
        }

        let root_scope = def_map.root();
        self.lower_and_check_rec(root_scope, &def_map, &mut dst);
        dst
    }

    fn lower_and_check_rec(&self, scope: LocalScopeId, def_map: &DefMap, dst: &mut String) {
        for (_, declaration) in &def_map[scope].declarations {
            if let Ok(id) = (*declaration).try_into() {
                let res = &self.inference_result(id);
                if !res.diagnostics.is_empty() {
                    format_to!(dst, "{:#?}", res.diagnostics);
                }
                let diagnostics = validation::BodyValidationDiagnostic::collect(self, id);
                if !diagnostics.is_empty() {
                    format_to!(dst, "{:#?}", diagnostics);
                }
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

#[test]
pub fn generate_integration_tests() {
    let tests = collect_integration_tests();
    let file = project_root().join("crates/hir_ty/src/tests/integration.rs");
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
                let actual = db.lower_and_check();
                expect_file![project_root().join("integration_tests").join(#test_name).join("inference_diagnostics.log")].assert_eq(&actual);
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
