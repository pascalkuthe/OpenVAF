extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::env;

mod frontend;


fn main() {
    let args: Vec<String> = env::args().collect();
    frontend::parse_file(&args[0]);
}
