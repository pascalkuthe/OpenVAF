extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::env;
use std::fmt::Debug;
use std::ops::Deref;
use std::path::{Path, PathBuf};

use indextree::{Arena, NodeId};

mod frontend;


fn main() {
    let args: Vec<String> = env::args().collect();
    let home = env::var_os("HOME").unwrap();
    let mut path = PathBuf::from(home);
    path.push(Path::new("test.v"));
    match { frontend::run_frontend(path.as_path()) } {
        Ok(ast) => {
            pprint_tree(ast.1, &ast.0)
        }
        Err(E) => println!("{}", E)
    }
}

fn pprint_tree<T: Debug>(top_node: NodeId, arena: &Arena<T>) {
    fn pprint_tree<T: Debug>(node: NodeId, arena: &Arena<T>, prefix: String, last: bool) {
        let prefix_current = if last { "`- " } else { "|- " };

        println!("{}{}{:?}", prefix, prefix_current, arena.get(node).unwrap().get());

        let prefix_child = if last { "   " } else { "|  " };
        let prefix = prefix + prefix_child;

        if node.children(arena).next().is_some() {
            let last_child = node.children(arena).count() - 1;

            for (i, child) in node.children(arena).enumerate() {
                pprint_tree(child, arena, prefix.to_string(), i == last_child);
            }
        }
    }

    pprint_tree(top_node, arena, "".to_string(), true);
}