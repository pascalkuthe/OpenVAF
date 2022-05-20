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
