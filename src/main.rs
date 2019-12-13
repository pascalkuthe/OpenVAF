extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::env;
use std::fmt::Debug;
use std::ops::Deref;
use std::path::{Path, PathBuf};

use fern::colors::{Color, ColoredLevelConfig};
use indextree::{Arena, NodeId};
use log::*;

mod frontend;


fn main() {
    setup_logger();
    let args: Vec<String> = env::args().collect();
    let home = env::var_os("HOME").unwrap();
    let mut path = PathBuf::from(home);
    path.push(Path::new("bjtp.va"));
    match { frontend::run_frontend(path.as_path()) } {
        Ok(ast) => {
            pprint_tree(ast.1, &ast.0)
        },
        Err(e) => ()
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

fn setup_logger() -> Result<(), fern::InitError> {
    let colors_line = ColoredLevelConfig::new()
        .error(Color::Red)
        .warn(Color::Yellow)
        // we actually don't need to specify the color for debug and info, they are white by default
        .info(Color::Blue)
        .debug(Color::White)
        // depending on the terminals color scheme, this is the same as the background color
        .trace(Color::BrightBlack);

    // configure colors for the name of the level.
    // since almost all of them are the some as the color for the whole line, we
    // just clone `colors_line` and overwrite our changes
    let colors_level = colors_line.clone().info(Color::Green);
    // here we set up our fern Dispatch
    fern::Dispatch::new()
        .format(move |out, message, record| {
            out.finish(format_args!(
                "{color_line}[{date}][{target}][{level}{color_line}] {message}\x1B[0m",
                color_line = format_args!(
                    "\x1B[{}m",
                    colors_line.get_color(&record.level()).to_fg_str()
                ),
                date = chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                target = record.target(),
                level = colors_level.color(record.level()),
                message = message,
            ));
        })
        .level(log::LevelFilter::Debug)
        .chain(std::io::stdout())
        .chain(fern::log_file("output.log")?)
        .apply()?;
    Ok(())
}
