use std::ffi::OsStr;
use std::io::Write;
use std::path::Path;
use std::process::exit;

use anyhow::{bail, Context, Result};
use basedb::lints::{self, LintLevel};
use clap::ArgMatches;
use llvm::OptLevel;
use path_absolutize::Absolutize;
use paths::AbsPathBuf;
use target::host_triple;
use target::spec::{get_targets, Target};
use termcolor::{Color, ColorChoice, ColorSpec, WriteColor};

use crate::cli_def::{
    ALLOW, BATCHMODE, CACHE_DIR, CODEGEN, DENY, INCLUDE, INPUT, LINTS, OPT_LVL, OUTPUT,
    SUPPORTED_TARGETS, TARGET, TARGET_CPU, WARN,
};

pub struct Opts {
    pub defines: Vec<String>,
    pub codegen_opts: Vec<String>,
    pub cache_dir: AbsPathBuf,
    pub lints: Vec<(String, LintLevel)>,
    pub input: AbsPathBuf,
    pub output: AbsPathBuf,
    pub include: Vec<AbsPathBuf>,
    pub batch: bool,
    pub opt_lvl: OptLevel,
    pub target: Target,
    pub target_cpu: String,
}

fn input_path_arg(raw: &OsStr) -> Result<AbsPathBuf> {
    Ok(AbsPathBuf::assert(Path::new(raw).canonicalize()?))
}

fn output_path_arg(raw: &OsStr) -> Result<AbsPathBuf> {
    let path = Path::new(raw).absolutize()?.into_owned();
    Ok(AbsPathBuf::assert(path))
}

impl Opts {
    pub fn new(matches: ArgMatches) -> Result<Self> {
        if matches.is_present(LINTS) {
            print_lints();
            exit(0)
        }
        if matches.is_present(SUPPORTED_TARGETS) {
            print_targets();
            exit(0)
        }

        let input = input_path_arg(matches.value_of_os(INPUT).unwrap())?;

        let output = if let Some(output) = matches.value_of_os(OUTPUT) {
            output_path_arg(output)?
        } else {
            let path: &Path = input.as_ref();
            AbsPathBuf::assert(path.with_extension("osdi"))
        };

        let mut lints = Vec::new();

        if let Some(allow) = matches.values_of(ALLOW) {
            lints.extend(allow.map(|lint| (lint.to_owned(), LintLevel::Allow)));
        }

        if let Some(warn) = matches.values_of(WARN) {
            lints.extend(warn.map(|lint| (lint.to_owned(), LintLevel::Warn)));
        }
        if let Some(deny) = matches.values_of(DENY) {
            lints.extend(deny.map(|lint| (lint.to_owned(), LintLevel::Deny)));
        }

        let batch = matches.is_present(BATCHMODE);
        let cache_dir = if let Some(val) = matches.value_of_os(CACHE_DIR) {
            Path::new(val).canonicalize().unwrap()
        } else if batch {
            directories_next::ProjectDirs::from("com", "semimod", "openvaf")
                .context(
                    "failed to find cache directory\nhelp: use --cache-dir to aquire it manually",
                )?
                .cache_dir()
                .to_owned()
        } else {
            Path::new(input.as_os_str()).to_owned()
        };

        let cache_dir = AbsPathBuf::assert(cache_dir);

        let codegen_opts = matches
            .values_of(CODEGEN)
            .map_or_else(Vec::new, |values| values.map(String::from).collect());

        let defines = matches.values_of(CODEGEN).map_or_else(Vec::new, |values| {
            values
                .map(|opt| {
                    // let (name, val) = opt.split_once('=').unwrap_or((opt, "1"));
                    // (name.to_owned(), val.to_owned())
                    opt.to_owned()
                })
                .collect()
        });

        let include: Result<_> = matches
            .values_of_os(INCLUDE)
            .map_or_else(|| Ok(Vec::new()), |includes| includes.map(input_path_arg).collect());

        let opt_lvl = match matches.value_of(OPT_LVL).unwrap() {
            "0" => OptLevel::None,
            "1" => OptLevel::Less,
            "2" => OptLevel::Default,
            "3" => OptLevel::Aggressive,
            lvl => bail!("unkown opt lvl {lvl}"),
        };

        let target = matches
            .value_of(TARGET)
            .map_or_else(|| host_triple().to_owned(), |target| target.to_owned());

        let target = if let Some(target) = Target::search(&target) {
            target
        } else {
            // shold never happend but helpful to provide support just in case
            bail!("The target {target} is not supported by  this binary")
        };

        let target_cpu = matches.value_of(TARGET_CPU).unwrap().to_owned();

        Ok(Opts {
            input,
            lints,
            batch,
            cache_dir,
            codegen_opts,
            defines,
            include: include?,
            output,
            opt_lvl,
            target,
            target_cpu,
        })
    }
}

fn print_lints() {
    let mut stdout = termcolor::StandardStream::stdout(ColorChoice::Auto);

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red))).unwrap();
    writeln!(&mut stdout, "ERRORS:").unwrap();
    stdout.set_color(&ColorSpec::new()).unwrap();
    for lint in lints::builtin::ALL {
        if lint.default_lvl == LintLevel::Deny {
            writeln!(&mut stdout, "    {}", lint.name).unwrap();
        }
    }

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).unwrap();
    writeln!(&mut stdout, "WARNINGS:").unwrap();
    stdout.set_color(&ColorSpec::new()).unwrap();

    for lint in lints::builtin::ALL {
        if lint.default_lvl == LintLevel::Warn {
            writeln!(&mut stdout, "    {}", lint.name).unwrap();
        }
    }

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green))).unwrap();
    writeln!(&mut stdout, "ALLOWED:").unwrap();
    stdout.set_color(&ColorSpec::new()).unwrap();

    for lint in lints::builtin::ALL {
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

    for target in get_targets() {
        writeln!(&mut stdout, "    {}", target).unwrap();
    }
}
