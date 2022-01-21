use std::path::PathBuf;

fn main() {
    if std::env::var("CARGO_FEATURE_BUILD").is_ok() {
        let src = project_root().join("target").join(std::env::var("PROFILE").unwrap());
        println!("cargo:rustc-link-search=native={}", src.to_str().unwrap());
    }

    println!("cargo:rustc-link-lib=dylib=verilogae");
}

pub fn project_root() -> PathBuf {
    let dir = env!("CARGO_MANIFEST_DIR");
    let mut res = PathBuf::from(dir);
    while !res.join("README.md").exists() {
        res = res.parent().expect("reached fs root without finding project root").to_owned()
    }

    res
}
