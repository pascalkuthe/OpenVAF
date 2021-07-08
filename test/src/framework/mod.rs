/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */
mod collection;
mod models;
mod runner;
pub(crate) mod session;

use crate::framework::models::Model;
use clap::clap_derive::Clap;
pub use collection::TestInitInfo;
pub use eyre::Result;
use eyre::WrapErr;
use linkme::distributed_slice;
pub use runner::run_tests;
pub use session::TestSession;
use std::ffi::OsStr;
use std::fmt::{Debug, Formatter};
use std::path::{Path, PathBuf};

fn shellexpand_input(raw: &OsStr) -> std::result::Result<PathBuf, String> {
    shellexpand(raw).map_err(|err| {
        println!("{:?}", err);
        "".to_string()
    })
}

fn shellexpand(raw: &OsStr) -> Result<PathBuf> {
    if let Some(utf8_str) = raw.to_str() {
        let root_file: &str = &shellexpand::full(utf8_str)?;
        Path::new(root_file)
            .canonicalize()
            .with_context(|| format!("Failed to open {}", root_file))
    } else {
        let path = Path::new(raw);
        path.canonicalize()
            .with_context(|| format!("Failed to open {}", path.display()))
    }
}

#[derive(Clap)]
#[clap(name = "openvaf-test")]
/// The OpenVAF test suite is intended to validate the correctness of OpenVAF compiler toolchain.
///
/// It is usually intended for developers and mostly not very useful to users.
/// If you are developing OpenVAF you probably want to call the test suite with the following command:
///
/// cargo run --release -- --src-dirs ../test
///
/// If any test fails on your computer please submit a bug to https://gitlab.com/DSPOM/OpenVAF/-/issues/new!
/// Test results are always printed to the tes_results directory.
/// Please attach the appropriate files to any bug reports.
pub struct Config {
    /// Filter which tests to run.
    ///
    /// If this argument is passed to the test suite only the tests whose name is passed to this argument are run.
    ///
    /// Example
    ///
    /// openvaf-test --tests MIDDLE, Parser_Units
    ///
    /// Runs the Parser_Units and MIDDLE test.
    #[clap(long)]
    pub tests: Option<Vec<String>>,

    /// Filter which test cases to run.
    ///
    /// If this argument is passed to the test suite only the test cases passed to this argument are run.
    /// You usually want to combine this whith the --tests argument when the same test case is used multiple times.
    ///
    /// Example
    ///
    /// openvaf-test --tests-cases module_header, HICUML2
    ///
    /// Runs the HICUML2 test_case (of the middle test for example) and the module_header test_case of Parser_Units test.
    #[clap(long)]
    pub test_cases: Option<Vec<String>>,

    /// Filter which models to run tests for.
    ///
    /// If this argument is passed to the test suite only the tests which use the models passed to this argument are run.
    /// Note that this argument is mostly equivalent to the --test-cases argument.
    /// However if a model file is added which is named as another hardcoded test case (which does not use the model) this argument can be used instead.
    /// Furthermore this equivalence may change in the future.
    ///
    /// Example
    ///
    /// openvaf-test --models BSIM6, HICUML2
    ///
    /// Runs tests for the BSIM v6 and HICUM/L2 models.
    #[clap(long)]
    pub models: Option<Vec<String>>,

    /// The path to the src directory of the test_suite where the tested VA files can be found
    #[clap(long, parse(try_from_os_str = shellexpand_input), default_value = "test")]
    pub src_dirs: PathBuf,

    /// Setting this flag causes the test_suite to emit a textual representation of the MIR to the test_results directory
    #[clap(long)]
    pub print_mir: bool,
}

#[distributed_slice]
pub static TESTS: [Test] = [..];

pub struct Test {
    pub stage: &'static str,
    pub name: &'static str,
    pub collect_test_cases: fn(&TestInitInfo) -> Box<[TestCase]>,
    pub run: fn(&TestSession) -> Result<()>,
    pub model: Option<&'static Model>,
}

impl Debug for Test {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Test {} with cases {}",
            self.name,
            self.model
                .map_or("".to_owned(), |model| format!("(model: {})", model.0))
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct TestCase {
    pub name: &'static str,
    pub model: Option<Model>,
}

#[macro_export]
macro_rules! test{
    (
        @FULL
        stage: $stage: expr,
        name: $name: ident,
        model: $model: expr,
        run: $run: expr,
        collect_test_cases: $test_cases: path
    ) => {
        ::paste::paste!{
            #[allow(non_upper_case_globals)]
            #[::linkme::distributed_slice($crate::framework::TESTS)]
            static [<__TEST__ $name>]: $crate::framework::Test = $crate::framework::Test {
                stage: $stage,
                name: ::std::stringify!($name),
                collect_test_cases: $test_cases,
                run: $run,
                model: $model
            };
        }

    };

    (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        collect_test_cases: $test_cases: path
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: None,
            run: $run,
            collect_test_cases: $test_cases
        );
    };

        (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        model: $model: expr
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: Some($model),
            run: $run,
            collect_test_cases: $crate::framework::TestInitInfo::singular_test_case,
        );
    };

    (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        test_cases: [$($test_cases: literal),*]
    ) => {
        ::paste::paste!{
            test!(
                stage: $stage,
                name: $name,
                run: $run,
                collect_test_cases: [<__test_cases__ $name>]
            );

            #[allow(non_snake_case)]
            fn [<__test_cases__ $name>](_: &$crate::framework::TestInitInfo)-> Box<[$crate::framework::TestCase]>{
                vec![$( $crate::framework::TestCase{ name: $test_cases, model: None}),*].into_boxed_slice()
            }
        }
    };

    (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        test_cases: [$($test_cases: literal),*]
    ) => {
        ::paste::paste!{

            test!(
                stage: $stage,
                name: $name,
                run: $run,
                collect_test_cases: [<__test_cases__ $name>]
            );

            #[allow(non_snake_case)]
            fn [<__test_cases__ $name>](_: &$crate::framework::TestInitInfo)-> Box<[$crate::framework::TestCase]>{
                vec![$( $crate::framework::TestCase{ name: $test_cases, model: None}),*].into_boxed_slice()
            }
        }
    };



        (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: None,
            run: $run,
        );
    };

}
