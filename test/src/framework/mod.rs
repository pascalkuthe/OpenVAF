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
mod session;

use crate::framework::models::Model;
pub use collection::TestInitInfo;
pub use eyre::Result;
use linkme::distributed_slice;
pub use runner::run_tests;
pub use session::TestSession;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;

pub struct Config {
    pub test: Option<Vec<String>>,
    pub test_cases: Option<Vec<String>>,
    pub models: Option<Vec<String>>,
    pub capture: bool,
    pub src_dirs: PathBuf,
    pub print_verbose_info: bool,
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
