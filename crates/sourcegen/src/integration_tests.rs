use std::fs;

use quote::{format_ident, quote};

use crate::{ensure_file_contents, project_root, reformat};

pub fn integration_tests() -> impl Iterator<Item = (String, Vec<String>)> {
    const ERR: &str = "failed to read integration_tests directory";
    let dir = project_root().join("integration_tests");
    fs::read_dir(dir).expect(ERR).map(|entry|{
        let entry = entry.expect(ERR);
        assert!(entry.file_type().expect(ERR).is_dir());
        let name = entry.path().file_name().expect(ERR).to_str().expect("intergation test names must be valid UTF-8").to_owned();
        let children = fs::read_dir(entry.path()).expect(ERR).filter_map(|entry|{
            let entry = entry.expect(ERR);
            let include =  entry.path().extension().and_then(|ex|ex.to_str()).map_or(false, |ex|matches!(ex,"va"|"vams"|"include"));
            if !include{
                eprintln!("warning: {} will not be includeded in the integration test {}!\n Only files with the extensions .va and .include are included",entry.path().display(), &name)
            }
            include.then(||entry.file_name().to_str().unwrap().to_owned())
        }).collect();
        assert!(entry.path().join(format!("{}.va",name.to_lowercase())).exists());
        (name, children)
    })
}

#[test]
pub fn parser() {
    let tests = integration_tests();
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

    ensure_file_contents(&file, &reformat(file_string));
}

#[test]
pub fn hir_def() {
    let tests = integration_tests();
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

    ensure_file_contents(&file, &reformat(file_string));
}
