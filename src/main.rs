//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


#[macro_use]
extern crate pest_derive;

use std::path::Path;

use fern::colors::{Color, ColoredLevelConfig};
use log::*;

mod parsing;
mod ast;

//TODO test for better speed on Linux
//extern crate jemallocator;
//#[global_allocator]
//static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn main() {
    setup_logger();
    match { parsing::parse_to_unverified_ast(Path::new("tests/bjt.va")) } {
        Ok(ast) => {
            ast.pprint();
        },
        Err(e) => error!("{}", e)
    }
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
        .level(log::LevelFilter::Info)
        .chain(std::io::stdout())
        .chain(fern::log_file("output.log")?)
        .apply()?;
    Ok(())
}
