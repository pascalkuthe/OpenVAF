use std::fs;
use std::path::Path;

use anyhow::bail;
use camino::Utf8Path;
use clap::builder::{PossibleValue, PossibleValuesParser, ValueParser};
use clap::{Arg, ArgAction, Command, ValueHint};
use openvaf::{builtin_lints, get_target_names, host_triple, LintLevel};
use path_absolutize::Absolutize;

const ABOUT: &str = r"For further information visit https://openvaf.semimod.de.";

pub fn main_command() -> Command {
    Command::new("openvaf")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Pascal Kuthe - Semimod GmbH")
        .after_long_help(ABOUT)
        .after_help(ABOUT)
        .args([
            def_arg(),
            include_dir(),
            lint_arg(LintLevel::Allow),
            lint_arg(LintLevel::Warn),
            lint_arg(LintLevel::Deny),
            lints(),
            output(),
            batchmode(),
            cache_dir(),
            opt_lvl(),
            target(),
            supported_targets(),
            target_cpu(),
            codegen_opts(),
            interface(),
            expand(),
            input(),
        ])
        .subcommand_required(false)
        .arg_required_else_help(true)
}

pub const INTERFACE: &str = "interface";
pub const BATCHMODE: &str = "batchmode";
pub const TARGET: &str = "target";
pub const SUPPORTED_TARGETS: &str = "supported-targets";
pub const LINTS: &str = "lints";
pub const TARGET_CPU: &str = "target_cpu";
pub const CODEGEN: &str = "codegen";
pub const INPUT: &str = "input";
pub const INCLUDE: &str = "include";
pub const OUTPUT: &str = "output";
pub const CACHE_DIR: &str = "cache-dir";
pub const OPT_LVL: &str = "opt_lvl";
pub const DEFINE: &str = "define";
pub const PRINT_EXPANSION: &str = "print-expansion";
pub const ALLOW: &str = "allow";
pub const WARN: &str = "warn";
pub const DENY: &str = "deny";

fn interface() -> Arg {
    Arg::new(INTERFACE)
        .long(INTERFACE)
        .help("Simulator interface for which the code is compiled.")
        .long_help("Simulator interface for which the code is compiled.\n\npossible values\n\nOSDI - Open Source Device Interface supported by NGSPICE")
        .short('i')
        .value_parser(["OSDI"])
        .default_value("OSDI")
        .num_args(1)
        .hide_possible_values(true)
        .required(false)
}

fn batchmode() -> Arg {
    flag(BATCHMODE, "batch").short('b').help("Enable batchmode compilation.").
        long_help("Enable batchmode compilation. In this mode files are only recompiled when required and the results are stored")
}

fn target() -> Arg {
    let vals = get_target_names().fold(String::new(), |mut dst, it| {
        dst.push('\n');
        dst.push_str(it);
        dst
    });
    Arg::new(TARGET)
        .long(TARGET)
        .help("Target triple for which the code is compiled.")
        .long_help(format!(
            "Target triple for which the code is compiled.\n\npossible values:\n{vals}"
        ))
        .value_parser(PossibleValuesParser::new(get_target_names()))
        .default_value(host_triple())
        .value_name("TARGET")
        .required(false)
        .value_hint(ValueHint::Other)
        .hide_possible_values(true)
}

fn supported_targets() -> Arg {
    flag(SUPPORTED_TARGETS, SUPPORTED_TARGETS)
        .help("Print target triples supported by OpenVAF.")
        .long_help(
        "Print target triples supported by OpenVAF.\nOnly thhese values can be passed to --target.",
    )
}

fn lints() -> Arg {
    flag(LINTS, LINTS)
        .help("Print a list of all known lints.")
        .long_help("Print a list of all known lints.\nOnly these values can be passed to --allow, --warn, and --deny.")
}

fn target_cpu() -> Arg {
    Arg::new(TARGET_CPU)
        .long(TARGET_CPU)
        .help("Target cpu for which the code is compiled.")
        .long_help("Target cpu for which the code is compile.\nBy default \'native\' is used to allow best possible performance.\nIn this case the best optimizations for the current hardware are used.\nTo distribute the output to people with unknown hardware set this option to generic.\nFor cross compilation \'generic\' is used by default.\n\nEXAMPLES: skylake, native, generic")
        .value_name("CPU")
        .required(false)
        .value_hint(ValueHint::Other)
}

fn codegen_opts() -> Arg {
    Arg::new(CODEGEN)
        .long(CODEGEN)
        .short('C')
        .help("Set a codegen option.")
        .long_help("Set a codegen option.\nThese options are passed directly to LLVM.")
        .value_name("OPT[=VALUE]")
        .action(ArgAction::Append)
        .required(false)
        .value_hint(ValueHint::Other)
}

fn input() -> Arg {
    input_file_path_arg(INPUT)
        .help("The root Verilog-A file.")
        .required_unless_present_any([LINTS, SUPPORTED_TARGETS])
}

fn include_dir() -> Arg {
    dir_path_arg(INCLUDE)
        .long(INCLUDE)
        .short('I')
        .help("Search directory for include files.")
        .required(false)
        .action(ArgAction::Append)
}

fn output() -> Arg {
    output_file_path_arg(OUTPUT)
        .long(OUTPUT)
        .short('o')
        .help("Set the path to the output file.")
        .conflicts_with("batchmode")
        .required(false)
}

fn flag(name: &'static str, long: &'static str) -> Arg {
    Arg::new(name).long(long).action(ArgAction::SetTrue)
}

fn cache_dir() -> Arg {
    dir_path_arg(CACHE_DIR)
        .long(CACHE_DIR)
        .help("Directory where artifacts are stored in batchmode.")
        .required(false)
        .requires("batchmode")
}

fn dir_path_arg(name: &'static str) -> Arg {
    let parse = |raw: &str| {
        let path = Utf8Path::new(raw).to_owned();

        match fs::metadata(&path) {
            Err(err) => bail!("{err}"),
            Ok(info) if !info.is_dir() => bail!("is not a directory"),
            _ => Ok(path),
        }
    };

    Arg::new(name)
        .value_name("DIR")
        .value_hint(ValueHint::DirPath)
        .value_parser(ValueParser::new(parse))
}

fn output_file_path_arg(name: &'static str) -> Arg {
    let parse = |raw: &str| {
        let path = Utf8Path::new(raw).to_owned();

        if path.exists() {
            match fs::metadata(&path) {
                Err(err) => bail!("{err}"),
                Ok(info) if !info.is_file() => bail!("is not a file"),
                _ => Ok(path),
            }
        } else {
            let raw_path: &Path = path.as_ref();
            let raw_path = match raw_path.absolutize() {
                Ok(path) => path,
                Err(err) => bail!("failed to access {err}"),
            };

            let parent = match raw_path.parent() {
                Some(parent) => parent,
                None => bail!("is not a file"),
            };

            match fs::metadata(parent) {
                Err(err) => bail!("{} {err}", parent.display()),
                Ok(info) if !info.is_dir() => bail!("{} is not a directory", parent.display()),
                _ => Ok(path),
            }
        }
    };

    Arg::new(name)
        .value_name("FILE")
        .value_hint(ValueHint::FilePath)
        .value_parser(ValueParser::new(parse))
}

fn input_file_path_arg(name: &'static str) -> Arg {
    let parse = |raw: &str| {
        let path = Utf8Path::new(raw).to_owned();

        match fs::metadata(&path) {
            Err(err) => bail!("{err}"),
            Ok(info) if !info.is_file() => bail!("is not a file"),
            _ => Ok(path),
        }
    };

    Arg::new(name).value_name("FILE").value_parser(parse)
}

fn opt_lvl() -> Arg {
    Arg::new(OPT_LVL)
        .long(OPT_LVL)
        .short('O')
        .help("Set how much the code is optimized.")
        .long_help("Set how much the generated machine code is optimized:\nA higher optimization level means slower compile times but faster simulations.\n\npossible values\n\n0 - no optimizations\n1 - optimize minimally\n2 - optimize more\n3 - optimize even more")
        .value_name("LEVEL")
        .value_hint(ValueHint::Other)
        .value_parser(["0","1","2","3"])
        .hide_possible_values(true)
        .default_value("3").required(false)
}

fn expand() -> Arg {
    flag(PRINT_EXPANSION, "print-expansion")
        .help("Abort after preprocessing and print expanded sourcecode.")
        .long_help(
            "Aborts the compilation after the preprocessor finishes.
The sourcecode with all macros (`define) expanded and preprocessor
directives (`include) resolved is emitted to stdout.",
        )
}

fn def_arg() -> Arg {
    Arg::new(DEFINE)
        .short('D')
        .help("Defines a MACRO for use within the preprocessors")
        .long_help("Defines a MACO for use within the preprocessors.\nIf the value is omitted \"1\" is used.")
        .value_name("MACRO[=VALUE]")
        .action(ArgAction::Append)
        .value_hint(ValueHint::Other).required(false)
}

fn lint_arg(lvl: LintLevel) -> Arg {
    let arg = match lvl {
        LintLevel::Warn => Arg::new(WARN).long(WARN).short('W').help("Make this lint a warning.")
            .long_help("Make this lint a warning.\nAccepts any lint (obtained with --lints) or on of the following:\n\nall - all lints\nerrors - all lints whose lvl is set to deny"),
        LintLevel::Allow => Arg::new(ALLOW).long(ALLOW).short('A').help("Ignore this lint.")
            .long_help("Ignore this lint.\nAccepts any lint (obtained with --lints) or on of the following:\n\nall - all lints\nwarnings - all lints whose lvl is set to warn\nerrors - all lints whose lvl is set to deny"),
        LintLevel::Deny => Arg::new(DENY).long(DENY).short('E').help("Make this lint an error")
            .long_help("Make this lint an error.\nAccepts any lint (obtained with --lints) or on of the following:\n\nall - all lints\nwarnings - all lints whose lvl is set to warn"),
    };

    let all_lints = builtin_lints::ALL.iter().map(|lint| PossibleValue::new(lint.name));

    arg.num_args(1)
        .action(ArgAction::Append)
        .value_name("LINT")
        .value_hint(ValueHint::Other)
        .value_parser(PossibleValuesParser::new(
            [
                PossibleValue::new("all").help("all lints"),
                PossibleValue::new("warnings").help("all lints whose lvl is set to warn"),
                PossibleValue::new("errors").help("all lints whose lvl is set to deny"),
            ]
            .into_iter()
            .chain(all_lints),
        ))
        .required(false)
        .hide_possible_values(true)
}
