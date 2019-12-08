extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::env;
use std::path::{Path, PathBuf};

mod frontend;


fn main() {
    let args: Vec<String> = env::args().collect();
    let home = env::var_os("HOME").unwrap();
    let mut path = PathBuf::from(home);
    path.push(Path::new("test.v"));
    match { frontend::run_frontend(path.as_path()) } {
        Ok(ast) => {
            println!("{}", ast.0.get(ast.1).unwrap())
        }
        Err(E) => println!("{}", E)
    }
}
