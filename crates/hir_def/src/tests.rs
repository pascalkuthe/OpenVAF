use crate::{
    db::{HirDefDB, HirDefDatabase, InternDatabase},
    nameres::{DefMap, LocalScopeId},
};
use basedb::{
    diagnostics::{sink::Buffer, Config, ConsoleSink, DiagnosticSink},
    lints::{ErasedItemTreeId, Lint, LintLevel, LintResolver},
    BaseDB, BaseDatabase, FileId, Vfs, VfsStorage,
};
use parking_lot::RwLock;
use quote::{format_ident, quote};
use sourcegen::{add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat};

mod integration;
mod lints;
mod generate_builtins;

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
    pub fn lower_and_check(&self) -> String {
        let root_file = self.root_file();
        let def_map = self.def_map(root_file);
        let mut buf = Buffer::no_color();
        {
            let mut sink = ConsoleSink::buffer(Config::default(), self, &mut buf);
            let diagnostics = &self.item_tree(root_file).diagnostics;
            sink.add_diagnostics(diagnostics, root_file, self);
            let root_scope = def_map.root();
            self.lower_and_check_rec(root_scope, &def_map, &mut sink);
        }
        let data = buf.into_inner();
        String::from_utf8(data).unwrap()
    }

    fn lower_and_check_rec(&self, scope: LocalScopeId, def_map: &DefMap, dst: &mut ConsoleSink) {
        let root_file = self.root_file();

        for (_, declaration) in &def_map[scope].declarations {
            if let Ok(id) = (*declaration).try_into() {
                let diagnostics = &self.body_source_map(root_file, id).diagnostics;
                dst.add_diagnostics(diagnostics, root_file, self);
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
impl LintResolver for TestDataBase {
    fn lint_overwrite(
        &self,
        lint: Lint,
        item_tree: ErasedItemTreeId,
        root_file: FileId,
    ) -> Option<LintLevel> {
        self.item_tree(root_file).lint_lvl(item_tree, lint)
    }
}

#[test]
pub fn generate_integration_tests() {
    let tests = collect_integration_tests();
    let file = project_root().join("crates/hir_def/src/tests/integration.rs");
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
                let diagnostics = db.lower_and_check();
                assert_eq!(&diagnostics,"")
            }
        }
    });

    let file_string = quote!(
        use crate::{tests::TestDataBase, db::HirDefDB};
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
