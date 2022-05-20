use std::ffi::OsStr;
use std::io::Write;
use std::process::exit;

use openvaf_driver::cli_def::INPUT;
use openvaf_driver::{main_command, run};

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

pub fn main() {
    let matches = main_command().get_matches();

    let input: String =
        matches.value_of_os(INPUT).unwrap_or_else(|| OsStr::new("")).to_string_lossy().into_owned();
    match run(matches) {
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
