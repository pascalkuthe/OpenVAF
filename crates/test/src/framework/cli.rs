/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::Config;
use data_structures::iter::Itertools;
use data_structures::pretty;
use eyre::{bail, Result, WrapErr};
use sourcegen::project_root;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};

pub const LONG_HELP: &str = r"
The OpenVAF test suite is intended to validate the correctness of OpenVAF compiler toolchain.

It is usually intended for developers and mostly not very useful to users. If you are developing
OpenVAF you probably want to call the test suite with the following command:

cargo run --release

If any test fails on your computer please submit a bug to
https://gitlab.com/DSPOM/OpenVAF/-/issues/new! Test results are always printed to the tes_results
directory. Please attach the appropriate files to any bug reports.

USAGE:
    test [FLAGS] [OPTIONS]

FLAGS:
    -h, --help
            Prints help information

        --print-mir
            Setting this flag causes the test_suite to emit a textual representation of the MIR to
            the test_results directory

    -V, --version
            Prints version information


OPTIONS:
    -m  --models <models>..
            Filter which models to run tests for.
            
            If this argument is passed to the test suite only the tests which use the models passed
            to this argument are run. Note that this argument is mostly equivalent to the --test-
            cases argument. However if a model file is added which is named as another hardcoded
            test case (which does not use the model) this argument can be used instead. Furthermore
            this equivalence may change in the future.
            
            EXAMPLE
            
            test --models BSIM6, HICUML2
            
            Runs tests for the BSIM v6 and HICUM/L2 models.

        --src-dirs <src-dirs>
            The path to the integration_tests folder where the tested VA models can be found
            [default: automatically determined]

    -c  --test-cases <test-cases>..
            Filter which test cases to run.
            
            If this argument is passed to the test suite only the test cases passed to this argument
            are run. You usually want to combine this whith the --tests argument when the same test
            case is used multiple times.
            
            EXAMPLE
            
            test --tests-cases module_header, HICUML2
            
            Runs the HICUML2 test_case (of the middle test for example) and the module_header
            test_case of Parser_Units test.

    -t  --tests <tests>...
            Filter which tests to run.
            
            If this argument is passed to the test suite only the tests whose name is passed to this
            argument are run.
            
            EXAMPLE
            
            test --tests MIDDLE, Parser_Units
            
            Runs the Parser_Units and MIDDLE test.
";

pub const SHORT_HELP: &str = r"
The OpenVAF test suite is intended to validate the correctness of OpenVAF compiler toolchain

USAGE:
    test [FLAGS] [OPTIONS]

FLAGS:
    -h, --help         Prints help information
        --print-mir    Setting this flag causes the test_suite to emit a textual representation of
                       the MIR to the test_results directory
    -V, --version      Prints version information

OPTIONS:
    -m  --models <models>...            Filter which models to run tests for
        --src-dirs <src-dirs>           The path to the src directory of the test_suite where the
                                        tested VA files can be found [default: working directory]
    -c  --test-cases <test-cases>...    Filter which test cases to run
    -t  --tests <tests>...              Filter which tests to run
";

fn shellexpand_input(raw: &OsStr) -> std::result::Result<PathBuf, String> {
    shellexpand(raw).map_err(|err| {
        println!("{:?}", err);
        "".to_string()
    })
}

fn shellexpand(raw: &OsStr) -> Result<PathBuf> {
    if let Some(utf8_str) = raw.to_str() {
        let root_file: &str = &shellexpand::full(utf8_str)?;
        Path::new(root_file).canonicalize().with_context(|| format!("Failed to open {}", root_file))
    } else {
        let path = Path::new(raw);
        path.canonicalize().with_context(|| format!("Failed to open {}", path.display()))
    }
}

fn parse_list(raw: &str) -> Result<Vec<String>> {
    Ok(raw.split(',').map(|x| x.trim().to_owned()).collect())
}

impl Config {
    pub fn parse() -> Result<Self> {
        let mut pargs = pico_args::Arguments::from_env();

        // Help has a higher priority and should be handled separately.
        if pargs.contains("-h") {
            print!("{}", SHORT_HELP);
            std::process::exit(0);
        }

        if pargs.contains("--help") {
            print!("{}", LONG_HELP);
            std::process::exit(0);
        }

        let config = Self {
            // Parses a required value that implements `FromStr`.
            // Returns an error if not present.
            tests: pargs.opt_value_from_fn(["-t", "--tests"], parse_list)?,
            test_cases: pargs.opt_value_from_fn(["-c", "--test-cases"], parse_list)?,
            models: pargs.opt_value_from_fn(["-m", "--models"], parse_list)?,
            src_dirs: pargs
                .opt_value_from_os_str(["-d", "--src-dirs"], shellexpand_input)?
                .unwrap_or(project_root().join("integration_tests")),

            print_mir: pargs.contains(["-p", "--print-mir"]),
        };

        // It's up to the caller what to do with the remaining arguments.
        let remaining = pargs.finish();
        if !remaining.is_empty() {
            bail!(
                "unrecognized arguments: {}.",
                pretty::List::new(
                    remaining.iter().map(|x| x.to_string_lossy().to_owned()).collect_vec()
                )
            );
        }

        Ok(config)
    }
}
