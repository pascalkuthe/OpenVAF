use std::io::Write;
use std::process::exit;

use anyhow::{bail, Context, Result};
use camino::Utf8PathBuf;
use clap::ArgMatches;
use openvaf::{builtin_lints, get_target_names, host_triple, AbsPathBuf, LintLevel, OptLevel};
use termcolor::{Color, ColorChoice, ColorSpec, WriteColor};

use crate::cli_def::{
    ALLOW, BATCHMODE, CACHE_DIR, CODEGEN, DEFINE, DENY, DRYRUN, INCLUDE, INPUT, LINTS, OPT_LVL,
    OUTPUT, SUPPORTED_TARGETS, TARGET, TARGET_CPU, WARN,
};
use crate::{CompilationDestination, Opts};

pub fn matches_to_opts(matches: ArgMatches) -> Result<Opts> {
    if matches.get_flag(LINTS) {
        print_lints();
        exit(0)
    }
    if matches.get_flag(SUPPORTED_TARGETS) {
        print_targets();
        exit(0)
    }

    let input: Utf8PathBuf = matches.get_one::<Utf8PathBuf>(INPUT).unwrap().clone();

    let mut lints = Vec::new();

    if let Some(allow) = matches.get_many::<String>(ALLOW) {
        lints.extend(allow.map(|lint| (lint.to_owned(), LintLevel::Allow)));
    }

    if let Some(warn) = matches.get_many::<String>(WARN) {
        lints.extend(warn.map(|lint| (lint.to_owned(), LintLevel::Warn)));
    }
    if let Some(deny) = matches.get_many::<String>(DENY) {
        lints.extend(deny.map(|lint| (lint.to_owned(), LintLevel::Deny)));
    }

    let output = if matches.get_flag(BATCHMODE) {
        let cache_dir = if let Some(val) = matches.get_one::<Utf8PathBuf>(CACHE_DIR) {
            val.clone()
        } else {
            let path = directories_next::ProjectDirs::from("com", "semimod", "openvaf")
                .context(
                    "failed to find cache directory\nhelp: use --cache-dir to aquire it manually",
                )?
                .cache_dir()
                .to_owned();
            if let Ok(res) = Utf8PathBuf::from_path_buf(path) {
                res
            } else {
                bail!("failed to find cache directory\nhelp: use --cache-dir to aquire it manually",)
            }
        };
        CompilationDestination::Cache { cache_dir }
    } else {
        let lib_file = if let Some(output) = matches.get_one::<Utf8PathBuf>(OUTPUT) {
            output.clone()
        } else {
            input.with_extension("osdi")
        };

        CompilationDestination::Path { lib_file }
    };

    let codegen_opts = matches
        .get_many::<String>(CODEGEN)
        .map_or_else(Vec::new, |values| values.cloned().collect());

    let defines = matches
        .get_many::<String>(DEFINE)
        .map_or_else(Vec::new, |values| values.cloned().collect());

    let include: Result<_> = matches.get_many::<Utf8PathBuf>(INCLUDE).map_or_else(
        || Ok(Vec::new()),
        |include| include.map(|path| Ok(AbsPathBuf::assert(path.canonicalize()?))).collect(),
    );

    let include = include?;

    let opt_lvl = match &**matches.get_one::<String>(OPT_LVL).unwrap() {
        "0" => OptLevel::None,
        "1" => OptLevel::Less,
        "2" => OptLevel::Default,
        "3" => OptLevel::Aggressive,
        lvl => bail!("unkown opt lvl {lvl}"),
    };

    let host = host_triple();
    let target = matches.get_one::<String>(TARGET).cloned().unwrap_or_else(|| host.to_owned());
    let default_cpu = if host != target { "generic" } else { "native" };

    let target = if let Some(target) = openvaf::Target::search(&target) {
        target
    } else {
        // shold never happend but helpful to provide support just in case
        bail!("The target {target} is not supported by  this binary")
    };

    let target_cpu: String =
        matches.get_one(TARGET_CPU).cloned().unwrap_or_else(|| default_cpu.to_owned());

    Ok(Opts {
        input,
        lints,
        codegen_opts,
        defines,
        include,
        output,
        opt_lvl,
        target,
        target_cpu,
        dry_run: matches.get_flag(DRYRUN),
    })
}

fn print_lints() {
    let mut stdout = termcolor::StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red))).unwrap();
    writeln!(&mut stdout, "ERRORS:").unwrap();
    stdout.set_color(&ColorSpec::new()).unwrap();
    for lint in builtin_lints::ALL {
        if lint.default_lvl == LintLevel::Deny {
            writeln!(&mut stdout, "    {}", lint.name).unwrap();
        }
    }

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).unwrap();
    writeln!(&mut stdout, "WARNINGS:").unwrap();
    stdout.set_color(&ColorSpec::new()).unwrap();

    for lint in builtin_lints::ALL {
        if lint.default_lvl == LintLevel::Warn {
            writeln!(&mut stdout, "    {}", lint.name).unwrap();
        }
    }

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green))).unwrap();
    writeln!(&mut stdout, "ALLOWED:").unwrap();
    stdout.set_color(&ColorSpec::new()).unwrap();

    for lint in builtin_lints::ALL {
        if lint.default_lvl == LintLevel::Allow {
            writeln!(&mut stdout, "    {}", lint.name).unwrap();
        }
    }
}

fn print_targets() {
    let mut stdout = termcolor::StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).unwrap();
    writeln!(&mut stdout, "TARGETS:").unwrap();
    stdout.set_color(&ColorSpec::new()).unwrap();

    for target in get_target_names() {
        writeln!(&mut stdout, "    {}", target).unwrap();
    }
}
