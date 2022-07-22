use std::path::Path;

use ::sourcegen::{
    add_preamble, collect_integration_tests, ensure_file_contents, project_root, reformat,
};
use llvm::OptLevel;
use mir_llvm::LLVMBackend;
use paths::AbsPathBuf;
use quote::format_ident;
use sim_back::CompilationDB;
use target::spec::Target;

use quote::quote;

mod integration;
mod sourcegen;

// #[test]
// fn diode() {
//     let target = Target::host_target().unwrap();
//     let back = LLVMBackend::new(&[], &target, OptLevel::None);
//     let path = project_root().join("integration_tests/HICUML2/hicuml2.va");
//     let base = Path::new("diode");
//     let obj = base.with_extension("o");
//     let out = base.with_extension("osdi");

//     crate::compile(&path, obj.as_ref(), &target, &back);
//     let shell: Shell = Shell::new().unwrap();
//     cmd!(shell, "gcc -shared -o {out} {obj}").run().expect("failed to generate.so");
//     shell.remove_path(obj).unwrap();
// }

fn test_compile(root_file: &Path) {
    let root_file = AbsPathBuf::assert(root_file.canonicalize().unwrap());
    let db = CompilationDB::new(root_file, &[], &[], &[]).unwrap();
    let modules = db.collect_modules().unwrap();
    let target = Target::host_target().unwrap();
    let back = LLVMBackend::new(&[], &target, "native".to_owned(), &[]);
    let emit = !stdx::IS_CI;
    crate::compile(&db, &modules, Path::new("foo.o"), &target, &back, emit, OptLevel::Aggressive);
}

#[test]
pub fn generate_integration_tests() {
    let tests = collect_integration_tests();
    let file = project_root().join("crates/osdi/src/tests/integration.rs");
    let test_impl = tests.into_iter().map(|(test_name, _)| {
        let test_case = format_ident!("{}", test_name.to_lowercase());
        let root_file_name = format!("{}.va", test_name.to_lowercase());

        quote! {
            #[test]
            fn #test_case(){
                if skip_slow_tests(){
                    return
                }

                let root_file = project_root().join("integration_tests").join(#test_name).join(#root_file_name);
                super::test_compile(&root_file);
            }
        }

    });

    let header = "
        use sourcegen::{skip_slow_tests, project_root};
    ";

    let file_string = quote!(
        #(#test_impl)*
    );
    let file_string = format!("{}\n{}", header, file_string);
    let file_string = add_preamble("generate_integration_tests", file_string);
    let file_string = reformat(file_string);
    ensure_file_contents(&file, &file_string);
}
