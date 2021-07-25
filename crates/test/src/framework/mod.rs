/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub use crate::framework::models::Model;
pub use collection::TestInitInfo;
pub use eyre::Result;
use linkme::distributed_slice;
pub use runner::run_tests;
pub use session::TestSession;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;

#[cfg(feature = "cli")]
use indicatif::{ProgressBar, ProgressStyle};

use data_structures::Cow;

#[cfg(not(feature = "cli"))]
type ProgressBar = ();
#[cfg(not(feature = "cli"))]
type ProgressStyle = ();

macro_rules! pb_println{
    ($pb: expr, $($args:tt)*) => {
        #[cfg(feature = "cli")]
            $pb.println(format!($($args)*));
        #[cfg(not(feature = "cli"))]
            println!($($args)*);
    };
}

#[cfg(feature = "cli")]
mod cli;
mod collection;
mod models;
mod runner;
pub(crate) mod session;

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
    /// test --tests MIDDLE, Parser_Units
    ///
    /// Runs the Parser_Units and MIDDLE test.
    pub tests: Option<Vec<String>>,

    /// Filter which test cases to run.
    ///
    /// If this argument is passed to the test suite only the test cases passed to this argument are run.
    /// You usually want to combine this whith the --tests argument when the same test case is used multiple times.
    ///
    /// Example
    ///
    /// test --tests-cases module_header, HICUML2
    ///
    /// Runs the HICUML2 test_case (of the middle test for example) and the module_header test_case of Parser_Units test.
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
    /// test --models BSIM6, HICUML2
    ///
    /// Runs tests for the BSIM v6 and HICUM/L2 models.
    pub models: Option<Vec<String>>,

    /// The path to the integration_tests directory of the test_suite where the tested VA files can be found
    pub src_dirs: PathBuf,

    /// Setting this flag causes the test_suite to emit a textual representation of the MIR to the test_results directory
    pub print_mir: bool,
}

#[distributed_slice]
pub static TESTS: [Test] = [..];

pub struct Test {
    pub stage: &'static str,
    pub name: &'static str,
    pub collect_test_cases: fn(&TestInitInfo) -> Box<[TestCase]>,
    pub run: fn(&mut TestSession) -> Result<()>,
    pub root_file: Option<fn(Option<TestCase>, Option<&'static Model>) -> Cow<'static, str>>,
    pub model: Option<&'static Model>,
}

impl Debug for Test {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Test {} with cases {}",
            self.name,
            self.model.map_or("".to_owned(), |model| format!("(model: {})", model.0))
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct TestCase {
    pub name: &'static str,
    pub model: Option<&'static Model>,
}

#[macro_export]
macro_rules! test{
    (
        @FULL
        stage: $stage: expr,
        name: $name: ident,
        model: $model: expr,
        run: $run: expr,
        collect_test_cases: $test_cases: path,
        root_file: $root_file:expr,
    ) => {
        ::paste::paste!{
            #[allow(non_upper_case_globals)]
            #[::linkme::distributed_slice($crate::framework::TESTS)]
            static [<__TEST__ $name>]: $crate::framework::Test = $crate::framework::Test {
                stage: $stage,
                name: ::std::stringify!($name),
                collect_test_cases: $test_cases,
                run: $run,
                model: $model,
                root_file: $root_file
            };
        }

    };

    (
        stage: $stage: expr,
        name: $name: ident,
        model: $model: expr,
        run: $run: expr,
        collect_test_cases: $test_cases: path,
        root_file: $root_file:expr,
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: None,
            run: $run,
            collect_test_cases: $test_cases,
            root_file: Some($root_file),
        );

    };

    (
        stage: $stage: expr,
        name: $name: ident,
        model: $model: expr,
        run: $run: expr,
        collect_test_cases: $test_cases: path,
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: None,
            run: $run,
            collect_test_cases: $test_cases,
            root_file: None,
        );

    };


    (
        stage: $stage: expr,
        name: $name: ident,
        model: $model: expr,
        run: $run: expr,
        collect_test_cases: $test_cases: path,
        root_file: const $root_file:expr,
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: None,
            run: $run,
            collect_test_cases: $test_cases,
            root_file: Some(|_,_|::data_structures::Cow::const_str($root_file)),
        );

    };

    (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        collect_test_cases: $test_cases: path,
        $($rem:tt)*
    ) => {
        test!(
            stage: $stage,
            name: $name,
            model: None,
            run: $run,
            collect_test_cases: $test_cases,
            $($rem)*
        );
    };

        (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        model: $model: expr,
        $($rem:tt)*
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: Some($model),
            run: $run,
            collect_test_cases: $crate::framework::TestInitInfo::singular_test_case,
            $($rem)*
        );
    };

    (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        test_cases: [$($test_cases: literal),*],
        $($rem:tt)*
    ) => {
        ::paste::paste!{
            test!(
                stage: $stage,
                name: $name,
                run: $run,
                collect_test_cases: [<__test_cases__ $name>],
                $($rem)*
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
        test_cases: [$(($test_cases: literal, $root_files: expr)),*],
        $($rem:tt)*
    ) => {
        ::paste::paste!{
            test!(
                stage: $stage,
                name: $name,
                run: $run,
                collect_test_cases: [<__test_cases__ $name>],
                root_file: [<__root_files $name>],
                $($rem)*
            );

            #[doc(hidden)]
            #[allow(non_snake_case)]
            fn [<__test_cases__ $name>](_: &$crate::framework::TestInitInfo)-> Box<[$crate::framework::TestCase]>{
                vec![$( $crate::framework::TestCase{ name: $test_cases, model: None}),*].into_boxed_slice()
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            fn [<__root_files $name>](case: Option<$crate::framework::TestCase>, _model: Option<& $crate::framework::Model>)-> ::data_structures::Cow<'static, str>{
                match case.unwrap().name{
                    $($test_cases => ::data_structures::Cow::const_str($root_files),)*
                    _ => todo!(),
                }
            }
        }
    };

    (
        stage: $stage: expr,
        name: $name: ident,
        run: $run: expr,
        $($rem:tt)*
    ) => {
        test!(
            @FULL
            stage: $stage,
            name: $name,
            model: None,
            run: $run,
            $($rem)*
        );
    };

}

// workaround for missing tests due to language bug -.-
fn keep_alive() {
    crate::source_gen::__keep_alive();
    crate::parsing_tests::__keep_alive();
}

pub fn model_root_file(_case: Option<TestCase>, model: Option<&Model>) -> Cow<'static, str> {
    let path = model.unwrap().mainfile();
    Cow::owned(std::fs::read_to_string(path).unwrap())
}
