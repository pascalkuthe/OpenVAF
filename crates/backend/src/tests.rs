use basedb::{BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsStorage};
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_def::Type;
use hir_ty::db::HirTyDatabase;
use parking_lot::RwLock;
use quote::format_ident;
use quote::quote;
use sourcegen::{
    add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat,
};

use crate::{find_branch_place, find_module};

mod eval_derivatives;
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
        let db: &mut dyn BaseDB = &mut res;
        let root_file =
            db.setup_test_db(root_file_name, root_file.to_owned().into(), &mut vfs.write());
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

    pub fn full_compile(&self) {
        let module = find_module(self, self.root_file());
        let (cfg, interner, literals) =
            crate::compile_to_cfg(self, module, true, false, true, &mut |places, dst| {
                find_branch_place(places, dst)
            });
        let module = crate::compile_to_bin(
            self,
            "test",
            cfg,
            &interner,
            &literals,
            (&Type::Real, &0f64.into()),
        );
        assert!(module.verify_and_print(), "Invalid LLVM code generated")
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
    let tests = collect_integration_tests();
    let file = project_root().join("crates/backend/src/tests/integration.rs");
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
                db.full_compile();
            }
        }
    });

    let header = "use std::fs::read;

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
