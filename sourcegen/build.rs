use std::env;

fn main() {
    if env::var("HOST") != env::var("TARGET") {
        println!("cargo:rustc-cfg=cross_compile")
    }
}
