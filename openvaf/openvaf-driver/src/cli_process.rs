use std::io::Write;
use std::process::exit;

use anyhow::{bail, Context, Result};
use basedb::lints::{self, LintLevel};
use camino::Utf8PathBuf;
use clap::ArgMatches;
use llvm::OptLevel;
use paths::AbsPathBuf;
use target::host_triple;
use target::spec::{get_target_names, Target};
use termcolor::{Color, ColorChoice, ColorSpec, WriteColor};

use crate::cli_def::{
    ALLOW, BATCHMODE, CACHE_DIR, CODEGEN, DENY, INCLUDE, INPUT, LINTS, OPT_LVL, OUTPUT,
    SUPPORTED_TARGETS, TARGET, TARGET_CPU, WARN,
};

pub struct Opts {
    pub defines: Vec<String>,
    pub codegen_opts: Vec<String>,
    pub cache_dir: Utf8PathBuf,
    pub lints: Vec<(String, LintLevel)>,
    pub input: Utf8PathBuf,
    pub output: Utf8PathBuf,
    pub include: Vec<AbsPathBuf>,
    pub batch: bool,
    pub opt_lvl: OptLevel,
    pub target: Target,
    pub target_cpu: String,
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

        let input: Utf8PathBuf = matches.get_one::<Utf8PathBuf>(INPUT).unwrap().clone();

        let output = if let Some(output) = matches.get_one::<Utf8PathBuf>(OUTPUT) {
            output.clone()
        } else {
            input.with_extension("osdi")
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
        let cache_dir = if let Some(val) = matches.get_one::<Utf8PathBuf>(CACHE_DIR) {
            val.clone()
        } else if batch {
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
        } else {
            input.clone()
        };

        let codegen_opts = matches
            .values_of(CODEGEN)
            .map_or_else(Vec::new, |values| values.map(String::from).collect());

        let defines = matches
            .values_of(CODEGEN)
            .map_or_else(Vec::new, |values| values.map(|opt| opt.to_owned()).collect());

        let include: Result<_> = matches.get_many::<Utf8PathBuf>(INCLUDE).map_or_else(
            || Ok(Vec::new()),
            |include| include.map(|path| Ok(AbsPathBuf::assert(path.canonicalize()?))).collect(),
        );

        let include = include?;

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
            include,
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

    for target in get_target_names() {
        writeln!(&mut stdout, "    {}", target).unwrap();
    }
}
