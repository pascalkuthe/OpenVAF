use std::path::Path;

use ::sourcegen::project_root;
use llvm::OptLevel;
use mir_llvm::LLVMBackend;
use target::spec::Target;
use xshell::{cmd, Shell};

mod sourcegen;

#[test]
fn diode() {
    let target = Target::host_target().unwrap();
    let back = LLVMBackend::new(&[], &target, OptLevel::None);
    let path = project_root().join("integration_tests/EKV_LONGCHANNEL/ekv_longchannel.va");
    let base = Path::new("diode");
    let obj = base.with_extension("o");
    let out = base.with_extension("osdi");

    crate::compile(&path, obj.as_ref(), &target, &back);
    let shell: Shell = Shell::new().unwrap();
    cmd!(shell, "gcc -shared -o {out} {obj}").run().expect("failed to generate.so");
    shell.remove_path(obj).unwrap();
}
