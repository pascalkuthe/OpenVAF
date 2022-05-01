use std::env;
use std::path::Path;

use xshell::{cmd, Shell};

fn main() {
    let sh = Shell::new().unwrap();
    let osdi_dir = sourcegen::project_root().join("crates").join("osdi");
    let src_file = osdi_dir.join("stdlib.c");

    sh.change_dir(osdi_dir);
    for file in sh.read_dir("header").unwrap() {
        if file.extension().map_or(true, |ext| ext != "h")
            || !file
                .file_stem()
                .and_then(|name| name.to_str())
                .map_or(false, |name| name.starts_with("osdi_"))
        {
            continue;
        }

        let name = file.file_stem().unwrap().to_str().unwrap();
        let def_name = name.to_uppercase();
        let version_str = name.strip_prefix("osdi_").unwrap();

        let out_dir = env::var_os("OUT_DIR").unwrap();
        let out_file = Path::new(&out_dir).join(&format!("stdlib_{version_str}.bc"));
        println!("cargo:rerun-if-changed={}", file.display());
        cmd!(sh, "clang -emit-llvm -O3 -D{def_name} -o {out_file} -c {src_file}")
            .run()
            .expect("failed to generate bitcode");
    }

    println!("cargo:rerun-if-changed={}", src_file.display());
}
