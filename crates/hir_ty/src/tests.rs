use std::{fs::read_to_string, path::PathBuf};

use crate::db::{HirTyDB, HirTyDatabase};
use basedb::{
    lints::{ErasedItemTreeId, Lint, LintLevel, LintResolver},
    BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsStorage,
};
use hir_def::{
    db::{HirDefDB, HirDefDatabase, InternDatabase},
    nameres::{DefMap, LocalScopeId},
};
use parking_lot::RwLock;
use sourcegen::project_root;
use stdx::format_to;

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
        let mut dst = String::new();
        let root_scope = def_map.entry();
        self.lower_and_check_rec(root_scope, &def_map, &mut dst);
        dst
    }

    fn lower_and_check_rec(&self, scope: LocalScopeId, def_map: &DefMap, dst: &mut String) {
        for (_, declaration) in &def_map[scope].declarations {
            if let Ok(id) = (*declaration).try_into() {
                let res = &self.inference_result(id);
                if !res.diagnostics.is_empty() {
                    format_to!(dst, "{:?}", res.diagnostics);
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
fn diode() {
    let root_file = r#"
`include "constants.vams"
`include "disciplines.vams"

`define OPP(nam,uni,des)               (*units=uni,                   desc=des*) parameter           real    nam = 0.0 ;

module diode_va(A,C);
    inout A, C;
    electrical A,C,CI;

    branch (A,CI) br_a_ci;
    branch (CI,C) br_ci_c;

    (*desc= "Saturation current", units = "A"*) parameter real Is = 1e-14 from [0:inf];

    (*desc= "Ohmic res", units = "Ohm" *) parameter real Rs = 0.0 from [0:inf];

    (*desc= "Emission coefficient"*) parameter real N = 1.0 from [0:inf];

    (*desc= "Junction capacitance", units = "F"*) parameter real Cj0 = 0.0 from [0:inf];

    (*desc= "Junction potential", units = "V"*) parameter real Vj = 1.0 from [0.2:2];

    (*desc= "Grading coefficient"*) parameter real M = 0.5 from [0:inf];
    `OPP(para,"uni","desc")


    real Vd, Vr, Qd;

    real Id;
    real vte;
    real vcrit;

    real VT,x,y,vf;

    analog begin


        if (Rs < 1e-3) begin
            V(br_ci_c) <+ 0;

            VT = `P_K*$temperature /`P_Q;
            if (Rs < 1e-3) begin
                    vcrit = vte * log(vte / (`M_SQRT2 * Is));
            end
        end


        if (Rs > 1e-3) begin
            VT = `P_K*$temperature /`P_Q;
            if (Rs < 1e-3) begin
                    vcrit = vte * log(vte / (`M_SQRT2 * Is));
            end
        end

        vte = VT * N;
        vcrit = vte * log(vte / (`M_SQRT2 * Is));

        Vd = $limit(V(br_a_ci), "pnjlim", VT, vcrit);
        Vr = V(br_ci_c);

        Id = Is * (exp(Vd / vte) - 1);

        //junction capacitance
        //smoothing of voltage over cap
        vf   = Vj*(1 - pow(3, -1/M));
        x    = (vf-Vd)/VT;
        y    = sqrt(x*x + 1.92);
        Vd   = vf-VT*(x + y)/(2);
        Qd   = Cj0*Vj * (1-pow(1-Vd/Vj, 1-M))/(1-M);

        I(br_a_ci) <+ Id + ddt(Qd);
        I(br_ci_c) <+ Vr / Rs;
    end
endmodule
    "#;
    let db = TestDataBase::new("/root.va", &root_file);
    {
        let path = project_root().join("integration_tests").join("DIODE").join("diode.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/diode.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "")
}

// #[test]
// pub fn generate_integration_tests() {
//     let tests = collect_integration_tests();
//     let file = project_root().join("crates/hir_def/src/tests/integration.rs");
//     let test_impl = tests.map(|(test_name,files)|{
//         let root_file_path = project_root()
//             .join(format!("integration_tests/{}/{}.va", test_name, test_name.to_lowercase()))
//             .to_str().unwrap().to_owned();
//         let file_names = files
//             .iter()
//             .map(|file| format!("/{}", file))
//         ;
//         let test_case = format_ident!("{}",test_name.to_lowercase());

//         quote! {

//             #[test]
//             fn #test_case(){
//                 if skip_slow_tests(){
//                     return
//                 }
//                 let root_file = read_to_string(PathBuf::from(#root_file_path)).unwrap();
//                 let db = TestDataBase::new("/root.va",&root_file);
//                 #(
//                     {
//                         let path = project_root().join("integration_tests").join(#test_name).join(#files);
//                         let file_contents =read_to_string(path).unwrap();
//                         db.vfs().write().add_virt_file(#file_names, &file_contents);
//                     }
//                 )*
//                 let diagnostics = db.lower_and_check();
//                 assert_eq!(&diagnostics,"")
//             }
//         }
//     });

//     let file_string = quote!(
//         use crate::{tests::TestDataBase, db::HirDefDB};
//         use sourcegen::{skip_slow_tests,project_root};
//         use std::{fs::read_to_string,path::PathBuf};
//         #(#test_impl)*
//     )
//     .to_string();

//     let file_string = add_preamble("generate_integration_tests", reformat(file_string));

//     ensure_file_contents(&file, &file_string);
// }
