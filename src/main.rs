extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate slab;

use std::env;
use std::path::{Path, PathBuf};

mod frontend;


fn main() {
    let args: Vec<String> = env::args().collect();
    let home = env::var_os("HOME").unwrap();
    let mut path = PathBuf::from(home);
    path.push(Path::new("test.v"));
    match { frontend::parse_file(path.as_path()) } {
        Ok(node) => println!("{:?}", node),
        Err(E) => println!("{}", E)
    }
}
