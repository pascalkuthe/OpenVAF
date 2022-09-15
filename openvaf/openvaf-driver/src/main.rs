use std::io::Write;
use std::process::exit;

use anyhow::Result;
use camino::Utf8PathBuf;
use clap::ArgMatches;
use mimalloc::MiMalloc;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use cli_def::{main_command, INPUT};
use openvaf::{run, CompilationDestination, CompilationTermination, Opts};

use crate::cli_process::matches_to_opts;

mod cli_def;
mod cli_process;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

pub fn main() {
    let matches = main_command().get_matches();
    let input: Utf8PathBuf = matches.get_one(INPUT).cloned().unwrap_or_else(Utf8PathBuf::new);

    match wrapped_main(matches) {
        Ok(err_code) => exit(err_code),
        Err(err) => {
            let mut stderr = StandardStream::stderr(ColorChoice::Auto);

            for cause in err.chain() {
                stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true)).unwrap();
                write!(&mut stderr, "error").unwrap();
                stderr.set_color(ColorSpec::new().set_bold(true)).unwrap();
                write!(&mut stderr, ":").unwrap();
                stderr.set_color(&ColorSpec::new()).unwrap();
                writeln!(&mut stderr, " {cause}").unwrap();
            }

            stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true)).unwrap();
            write!(&mut stderr, "error").unwrap();
            stderr.set_color(ColorSpec::new().set_bold(true)).unwrap();
            write!(&mut stderr, ":").unwrap();
            stderr.set_color(&ColorSpec::new()).unwrap();
            writeln!(&mut stderr, " failed to compile {input}").unwrap();
        }
    }
}

pub const DATA_ERROR: i32 = 65;

fn wrapped_main(matches: ArgMatches) -> Result<i32> {
    let opts = matches_to_opts(matches)?;
    match run(&opts)? {
        CompilationTermination::Compiled { lib_file } => {
            if matches!(opts.output, CompilationDestination::Cache { .. }) {
                println!("{lib_file}");
            }
            0
        }
        CompilationTermination::FatalDiagnostic => DATA_ERROR,
    };

    Ok(0)
}
