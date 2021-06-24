/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::collection::TestsToRun;
use crate::framework::models::{find_models, Model};
use crate::framework::session::TestSession;
use crate::framework::{Config, Test, TestCase};
use color_eyre::config::HookBuilder;
use color_eyre::eyre::WrapErr;
use color_eyre::Section;
use console::{Style, Term};
use eyre::Result;
use humantime::Duration;
use indicatif::{ProgressBar, ProgressStyle};
use openvaf_session::Session;
use std::cmp::min;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Formatter};
use std::fs::{create_dir_all, File};
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::{Duration as StdDuration, Instant};
use std::{fmt, panic, thread};
use tracing_subscriber::layer::SubscriberExt;

impl Test {
    pub fn output_dir(&self) -> Result<PathBuf> {
        let res = PathBuf::new().join("test_results").join(self.name);
        create_dir_all(&res)?;
        Ok(res)
    }

    fn log_file_stem(&self, test_case: Option<TestCase>) -> Result<PathBuf> {
        let dir = self.output_dir()?;
        let res = match test_case {
            Some(test_case) => dir.join(format!("{}_compiler_trace", test_case.name)),
            None => dir.join("compiler_trace"),
        };
        Ok(res)
    }

    pub fn pretty_log_file(&self, test_case: Option<TestCase>) -> Result<File> {
        let mut file = self.log_file_stem(test_case)?;
        assert!(file.set_extension("log"));
        Ok(File::create(file)?)
    }

    pub fn json_log_file(&self, test_case: Option<TestCase>) -> Result<File> {
        let mut file = self.log_file_stem(test_case)?;
        assert!(file.set_extension("json"));

        Ok(File::create(file)?)
    }

    pub fn dump_crash(dir: PathBuf, test_case: Option<TestCase>, content: &str, pb: &ProgressBar) {
        let mut hasher = DefaultHasher::new();
        content.hash(&mut hasher);
        let hash = hasher.finish();
        let res = match test_case {
            Some(test_case) => dir.join(format!("{}_crash_{}.log", test_case.name, hash)),
            None => dir.join(format!("crash_{}.log", hash)),
        };
        pb.println(format!("Saving crash report to '{}'", res.display()));
        if let Err(err) = File::create(res)
            .and_then(|mut file| file.write(content.as_bytes()))
            .wrap_err("Failed to write crash report")
            .note("Crash report was not saved. Copy from the terminal instead")
        {
            pb.println(format!("\n{}\n", err))
        }
    }

    fn run(
        &'static self,
        test_case: Option<TestCase>,
        config: &Config,
        models: &[Model],
        pb: &ProgressBar,
    ) -> Result<bool> {
        //let filter = tracing_subscriber::EnvFilter::from_env("OPENVAF_TEST_LOG");

        let (json_file, _json_log_guard) =
            tracing_appender::non_blocking(self.json_log_file(test_case).wrap_err_with(|| {
                format!(
                    "Failed to create json log file for {} {}",
                    self.name,
                    test_case.map_or("", |case| case.name)
                )
            })?);
        let (compact_file, _log_guard) = tracing_appender::non_blocking(
            self.pretty_log_file(test_case).wrap_err_with(|| {
                format!(
                    "Failed to create log file for {} {}",
                    self.name,
                    test_case.map_or("", |case| case.name)
                )
            })?,
        );

        //let pretty_print = tracing_subscriber::fmt().pretty();
        let json_file = tracing_subscriber::fmt::layer()
            .json()
            .without_time()
            .with_writer(json_file);
        let pretty_file = tracing_subscriber::fmt::layer()
            .compact()
            .without_time()
            .with_ansi(false)
            .with_writer(compact_file);

        let collector = tracing_subscriber::registry()
            //.with(filter)
            .with(pretty_file)
            .with(json_file);

        let (panic_hook, _) = HookBuilder::default()
            .panic_section(format!(
                "Test {} ({}) has failed with an Internal Compiler Error (ICE)",
                self.name,
                test_case.map_or("", |test_case| test_case.name)
            ))
            .into_hooks();

        tracing::subscriber::with_default(collector, || {
            let old_hook = panic::take_hook();
            let out_dir = self.output_dir()?;
            let pb2 = pb.clone();

            panic::set_hook(Box::new(move |panic_info| {
                let report = panic_hook.panic_report(panic_info);
                pb2.println(report.to_string());
                Self::dump_crash(out_dir.clone(), test_case, &report.to_string(), &pb2)
            }));

            let openvaf_session = Arc::new(Session::new());

            let res = openvaf_session.run(|| {
                std::panic::catch_unwind(move || {
                    let session = TestSession {
                        config,
                        models,
                        test: self,
                        test_case,
                    };
                    (self.run)(&session)
                })
            });

            panic::set_hook(old_hook);
            let res = match res {
                Ok(Ok(())) => true,
                Ok(Err(err)) => {
                    let err = err.with_section(|| {
                        format!(
                            "Test {} ({}) has failed!!",
                            self.name,
                            test_case.map_or("", |test_case| test_case.name)
                        )
                    });
                    pb.println(format!("\n{:?}\n", err));
                    let out_dir = self.output_dir()?;
                    Self::dump_crash(out_dir, test_case, &err.to_string(), pb);
                    false
                }
                Err(_) => false, //panic already printed its error
            };
            // ensure that the session is not dropeed before the errors are printed
            drop(openvaf_session);
            Ok(res)
        })
    }
}

struct TestTitle {
    test: &'static Test,
    case: Option<TestCase>,
    max_test_len: usize,
    max_testcase_len: usize,
}

impl Display for TestTitle {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(case) = self.case {
            write!(
                f,
                "{:^testwidth$}{}{:^casewidth$}",
                self.test.name,
                TEST_CASE_SEPARATOR,
                case.name,
                testwidth = self.max_test_len,
                casewidth = self.max_testcase_len
            )?;
        } else {
            write!(
                f,
                "{:^testwidth$}{}{:^casewidth$}",
                self.test.name,
                TEST_CASE_SEPARATOR_DUMMY,
                "",
                testwidth = self.max_test_len,
                casewidth = self.max_testcase_len
            )?;
        }
        Ok(())
    }
}

enum TestResult {
    Skipped,
    Passed(StdDuration),
    Failed(StdDuration),
}

struct TestSummary {
    test: &'static Test,
    case: Option<TestCase>,
    max_test_len: usize,
    max_testcase_len: usize,
    result: TestResult,
}

impl Display for TestSummary {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let prefix = match self.result {
            TestResult::Passed(_) => Style::new()
                .italic()
                .green()
                .for_stdout()
                .apply_to("passed"),
            TestResult::Skipped => Style::new().italic().dim().for_stdout().apply_to("skipped"),
            TestResult::Failed(_) => Style::new().italic().red().for_stdout().apply_to("failed"),
        };
        let title = TestTitle {
            test: self.test,
            case: self.case,
            max_test_len: self.max_test_len,
            max_testcase_len: self.max_testcase_len,
        };

        write!(f, "{:^7} {}", prefix, title)?;
        if let TestResult::Failed(duration) | TestResult::Passed(duration) = self.result {
            write!(f, " ({})", Duration::from(duration))?;
        }
        Ok(())
    }
}

const VERSION: &'static str = env!("CARGO_PKG_VERSION");
const TEST_CASE_SEPARATOR: &'static str = " -- ";
const TEST_CASE_SEPARATOR_DUMMY: &'static str = "    ";

pub fn run_tests(config: &Config) -> Result<()> {
    std::fs::remove_dir_all("test_results")
        .wrap_err("Failed to remove previous test_results directory")?;

    let base = format!(
        " {} {} ",
        Style::new().bold().yellow().apply_to("OPENVAF TEST SUITE"),
        Style::new()
            .italic()
            .yellow()
            .apply_to(format!("v{}", VERSION))
    );
    println!(
        "{:->width$}\n\n",
        base,
        width = min(Term::stdout().size().1 as usize, 70)
    );

    let pb = ProgressBar::new_spinner();
    pb.set_message("Collecting Tests");

    let models = find_models(config)?;
    let TestsToRun {
        max_test_len,
        max_testcase_len,
        test_cnt,
        tests,
    } = TestsToRun::collect(config, &models);

    pb.finish_and_clear();

    let pb = ProgressBar::new(test_cnt as u64);
    pb.set_style(ProgressStyle::default_bar()
        .template(&if Term::stdout().size().1 > 80 {
            format!("\n{{prefix:.cyan.bold}} [{{bar:57}}] {{pos}}/{{len}} {{msg:<{}}} {{spinner}} ({{elapsed}})", max_testcase_len + max_test_len+4)
        } else {
            "{prefix:>12.cyan.bold} [{bar:57}] {pos}/{len} {spinner:.blue}".to_owned()
        })
        .progress_chars("=> "));

    pb.set_prefix("Running");
    let start = Instant::now();

    let mut skipped = 0;
    let mut fail = 0;
    let mut pass = 0;

    let update_flag = Arc::new(Mutex::new(true));
    {
        let pb = pb.clone();
        let update_flag = update_flag.clone();
        thread::spawn(move || {
            while *update_flag.lock().unwrap() {
                pb.tick();
                thread::sleep(StdDuration::from_millis(100))
            }
        });
    }

    for (test, test_cases) in tests {
        if let Some(cases_to_run) = test_cases {
            if cases_to_run.len() == 0 {
                pb.set_message(
                    TestTitle {
                        test,
                        case: None,
                        max_test_len,
                        max_testcase_len,
                    }
                    .to_string(),
                );
                pb.reset_elapsed();
                let res = test.run(None, config, &models, &pb)?;
                let elapsed = pb.elapsed();
                let result = if res {
                    pass += 1;
                    TestResult::Passed(elapsed)
                } else {
                    fail += 1;
                    TestResult::Failed(elapsed)
                };
                pb.println(
                    TestSummary {
                        test,
                        case: None,
                        max_test_len,
                        max_testcase_len,
                        result,
                    }
                    .to_string(),
                );
                pb.inc(1);
            } else {
                for (case, run) in cases_to_run.iter() {
                    if *run {
                        pb.set_message(
                            TestTitle {
                                test,
                                case: Some(*case),
                                max_test_len,
                                max_testcase_len,
                            }
                            .to_string(),
                        );
                        pb.reset_elapsed();
                        let res = test.run(Some(*case), config, &models, &pb)?;
                        let elapsed = pb.elapsed();
                        let result = if res {
                            pass += 1;
                            TestResult::Passed(elapsed)
                        } else {
                            fail += 1;
                            TestResult::Failed(elapsed)
                        };
                        pb.println(
                            TestSummary {
                                test,
                                case: Some(*case),
                                max_test_len,
                                max_testcase_len,
                                result,
                            }
                            .to_string(),
                        );
                        pb.inc(1);
                    } else {
                        pb.set_message("");
                        pb.println(
                            TestSummary {
                                test,
                                case: Some(*case),
                                max_test_len,
                                max_testcase_len,
                                result: TestResult::Skipped,
                            }
                            .to_string(),
                        );
                        skipped += 1;
                    }
                }
            }
            pb.set_message("")
        } else {
            pb.println(
                TestSummary {
                    test,
                    case: None,
                    max_test_len,
                    max_testcase_len,
                    result: TestResult::Skipped,
                }
                .to_string(),
            );
            skipped += 1;
        }
    }

    *update_flag.lock().unwrap() = false;

    pb.finish();
    pb.finish_and_clear();

    println!(
        "\n\n{} {} tests in {}: {} {} {} {} {} {}",
        Style::new().bold().cyan().apply_to("Finished"),
        fail + pass,
        Duration::from(start.elapsed()),
        pass,
        Style::new().green().italic().apply_to("passed"),
        fail,
        Style::new().red().italic().apply_to("failed"),
        skipped,
        Style::new().dim().italic().apply_to("skipped"),
    );
    Ok(())
}
