/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::models::Model;
use crate::framework::{Config, Test, TestCase, TESTS};
use openvaf_data_structures::iter::Itertools;
use std::cmp::max;
use std::iter;

pub struct TestInitInfo<'a> {
    pub config: &'a Config,
    pub models: &'a [Model],
    pub test: &'static Test,
}

impl<'a> TestInitInfo<'a> {
    pub fn model_tests_cases(info: &TestInitInfo) -> Box<[TestCase]> {
        info.models
            .iter()
            .map(|x| TestCase {
                name: x.0,
                model: Some(*x),
            })
            .collect_vec()
            .into()
    }

    // When more cases should be added
    pub fn model_tests_cases_vec(&self) -> Vec<TestCase> {
        self.models
            .iter()
            .map(|x| TestCase {
                name: x.0,
                model: Some(*x),
            })
            .collect_vec()
            .into()
    }

    pub fn singular_test_case(_: &TestInitInfo) -> Box<[TestCase]> {
        Vec::new().into_boxed_slice()
    }
}

impl<'a> TestInitInfo<'a> {
    fn collect_test_cases(self) -> CollectedTest {
        let tests_cases = (self.test.collect_test_cases)(&self);

        let Self { config, test, .. } = self;

        if let Some(ref tests) = config.test {
            if !tests.iter().any(|x| x == test.name) {
                return CollectedTest::new_ignored(tests_cases);
            }
        }

        if let Some(ref models) = config.models {
            if let Some(model) = test.model {
                if !models.iter().any(|x| x == model.0) {
                    return CollectedTest::new_ignored(tests_cases);
                }
            } else {
                return CollectedTest::new_ignored(tests_cases);
            }
        }

        let mut test_cases: Vec<_> = tests_cases
            .iter()
            .copied()
            .zip(iter::repeat(true))
            .collect();

        if let Some(ref allowed_models) = config.models {
            for (test_case, run) in &mut test_cases {
                *run &= test_case
                    .model
                    .map_or(false, |model| allowed_models.iter().any(|x| x == model.0))
            }
        }

        if let Some(ref allowed_cases) = config.test_cases {
            for (test_case, run) in &mut test_cases {
                *run = *run
                    && allowed_cases
                        .iter()
                        .find(|x| &*x == &test_case.name)
                        .is_some()
            }
        }

        CollectedTest::new(test_cases.into_boxed_slice())
    }
}

struct CollectedTest {
    cases: Box<[(TestCase, bool)]>,
    ignored: bool,
    cases_to_run: usize,
    longest_case_name_len: usize,
}

impl CollectedTest {
    fn new_ignored(cases: Box<[TestCase]>) -> Self {
        let cases = cases
            .iter()
            .map(|x| (*x, false))
            .collect_vec()
            .into_boxed_slice();
        Self {
            cases,
            ignored: true,
            cases_to_run: 0,
            longest_case_name_len: 0,
        }
    }

    fn new(cases: Box<[(TestCase, bool)]>) -> Self {
        if cases.len() == 0 {
            Self {
                cases,
                ignored: false,
                cases_to_run: 1,
                longest_case_name_len: 0,
            }
        } else {
            let mut longest_case_name_len = 0;
            let cases_to_run = cases
                .iter()
                .filter(|(case, run)| {
                    if *run {
                        longest_case_name_len = max(longest_case_name_len, case.name.len());
                        true
                    } else {
                        false
                    }
                })
                .count();
            Self {
                cases,
                ignored: cases_to_run == 0,
                cases_to_run,
                longest_case_name_len,
            }
        }
    }
}

pub struct TestsToRun {
    pub max_test_len: usize,
    pub max_testcase_len: usize,
    pub test_cnt: usize,
    pub tests: Vec<(&'static Test, Option<Box<[(TestCase, bool)]>>)>,
}

impl TestsToRun {
    pub fn collect(config: &Config, models: &[Model]) -> Self {
        let mut max_test_len = 4usize;
        let mut max_testcase_len = 0usize;
        let mut test_cnt = 0;
        let tests = TESTS
            .iter()
            .map(|test| {
                let CollectedTest {
                    cases,
                    ignored,
                    cases_to_run,
                    longest_case_name_len,
                } = TestInitInfo {
                    config,
                    models: &models,
                    test,
                }
                .collect_test_cases();

                if !ignored {
                    test_cnt += cases_to_run;
                    max_test_len = max(test.name.len(), max_test_len);
                    max_testcase_len = max(longest_case_name_len, max_testcase_len);
                    (test, Some(cases))
                } else {
                    (test, None)
                }
            })
            .collect_vec();

        max_test_len += 4;
        Self {
            max_test_len,
            max_testcase_len,
            test_cnt,
            tests,
        }
    }
}
