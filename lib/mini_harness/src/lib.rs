use std::fmt::{self, Debug, Display, Formatter};
use std::fs::read_dir;
use std::path::Path;
use std::process::{self, exit};
use std::str::FromStr;
use std::time::{Duration, Instant};

pub use flags::Test as Arguments;

mod flags;

pub type Result<T = (), E = Failed> = std::result::Result<T, E>;

pub struct Test<'a> {
    pub name: String,
    pub runner: Box<dyn FnOnce() -> Result + 'a>,
    pub ignored: bool,
}

impl<'a> Test<'a> {
    pub fn new(name: &str, runner: &'a (dyn Fn() -> Result + 'a)) -> Self {
        Self { name: name.to_owned(), runner: Box::new(runner), ignored: false }
    }

    pub fn ignored(mut self) -> Self {
        self.ignored = true;
        self
    }

    pub fn from_dir<'r>(
        name: &'r str,
        runner: &'a dyn Fn(&Path) -> Result,
        ignore: &'r dyn Fn(&Path) -> bool,
        dir: &Path,
    ) -> impl IntoIterator<Item = Test<'a>> + 'r
    where
        'a: 'r,
    {
        Self::from_dir_filtered(name, runner, &|_| true, ignore, dir)
    }

    pub fn from_list<'r, T: Debug + Clone>(
        name: &'r str,
        runner: &'a dyn Fn(T) -> Result,
        ignore: &'r dyn Fn(T) -> bool,
        args: &'r [T],
    ) -> impl IntoIterator<Item = Test<'a>> + 'r
    where
        'a: 'r,
    {
        args.iter().map(move |arg| {
            let arg_ = arg.clone();
            Test {
                name: format!("{name} {arg:?}"),
                runner: Box::new(move || runner(arg_)),
                ignored: ignore(arg.clone()),
            }
        })
    }

    pub fn from_dir_filtered<'r>(
        name: &'r str,
        runner: &'a dyn Fn(&Path) -> Result,
        filter: &'r dyn Fn(&Path) -> bool,
        ignore: &'r dyn Fn(&Path) -> bool,
        dir: &Path,
    ) -> impl IntoIterator<Item = Test<'a>> + 'r
    where
        'a: 'r,
    {
        read_dir(dir).expect("reading test data must succeed").flatten().filter_map(move |entry| {
            let path = entry.path();
            if !filter(&path) {
                return None;
            }
            let test = Test {
                name: format!("{name}::{}", path.file_name().unwrap().to_string_lossy()),
                ignored: ignore(&path),
                runner: Box::new(move || runner(&path)),
            };
            Some(test)
        })
    }
}
impl Test<'_> {
    /// Runs the given runner, catching any panics and treating them as a failed test.
    fn run(runner: Box<dyn FnOnce() -> Result + '_>) -> Result<(), String> {
        use std::panic::{catch_unwind, AssertUnwindSafe};

        let res = catch_unwind(AssertUnwindSafe(runner));
        match res {
            Ok(res) => res.map_err(|err| err.msg),
            Err(err) => {
                let payload = err
                    .downcast_ref::<String>()
                    .map(|s| s.as_str())
                    .or_else(|| err.downcast_ref::<&str>().copied());

                let msg = match payload {
                    Some(payload) => format!("test panicked: {payload}"),
                    None => "test panicked".to_owned(),
                };
                Err(msg)
            }
        }
    }
}

/// Indicates that a test has failed
#[derive(Debug, Clone)]
pub struct Failed {
    msg: String,
}

impl<M: fmt::Display> From<M> for Failed {
    fn from(msg: M) -> Self {
        Self { msg: msg.to_string() }
    }
}

impl Arguments {
    pub fn parse_cli() -> Arguments {
        match Arguments::from_env() {
            Ok(res) => res,
            Err(err) => {
                eprintln!("{err}");
                // match the exit code used by libtest
                exit(101)
            }
        }
    }
}

/// Runs all given tests.
///
/// This is the central function of this crate. It provides the framework for
/// the testing harness. It does all the printing and house keeping.
pub fn run_harness(args: &Arguments, mut tests: Vec<Test>) -> TestSummary {
    let start_instant = Instant::now();

    // always sort tests first to ensure deterministic behaviour in all cases
    tests.sort_unstable_by(|a, b| a.name.cmp(&b.name));

    // Apply filtering
    let filtered = if args.filter.is_some() || !args.skip.is_empty() || args.ignored {
        let len_before = tests.len();
        tests.retain(|test| !args.is_filtered_out(test));
        (len_before - tests.len()) as u32
    } else {
        0
    };

    let pretty = args.pretty();

    // If `--list` is specified, just print the list and return.
    if args.list {
        print_tests(&tests, pretty, args.ignored);
        return TestSummary::default();
    }

    let name_width = if pretty {
        tests.iter().map(|test| test.name.chars().count()).max().unwrap_or(0)
    } else {
        0
    };

    // Print number of tests
    println!("\nrunning {} tests", tests.len());
    let mut ignored = 0;

    let mut failed_tests = Vec::new();

    let num_tests = tests.len();
    // Execute all tests.
    // Run test sequentially in main thread
    for test in tests {
        if pretty {
            print!("test {: <name_width$} ... ", &test.name);
        }
        if args.is_ignored(&test) {
            ignored += 1;
            if pretty {
                println!("ignored\n");
            } else {
                print!("i")
            }
        } else {
            match Test::run(test.runner) {
                Ok(_) => {
                    if pretty {
                        println!("ok")
                    } else {
                        print!(".")
                    }
                }
                Err(err) => {
                    println!("{err}");
                    if pretty {
                        println!("FAILED");
                    } else {
                        print!("F");
                    }
                    failed_tests.push(test.name)
                }
            }
        };
    }

    // Print failures if there were any, and the final summary.
    if !failed_tests.is_empty() {
        println!("\n failures:\n");

        for test in &failed_tests {
            println!("    {test}");
        }
    }
    let res = TestSummary {
        passed: num_tests as u32 - failed_tests.len() as u32 - ignored,
        failed: failed_tests,
        ignored,
        filtered,
        elapsed: start_instant.elapsed(),
    };

    println!("{res}");

    res
}

impl Arguments {
    /// Returns `true` if the given test should be ignored.
    fn is_ignored(&self, test: &Test) -> bool {
        test.ignored && !self.ignored && !self.include_ignored
    }

    fn is_filtered_out(&self, test: &Test) -> bool {
        let test_name = &test.name;

        if let Some(filter) = &self.filter {
            match self.exact {
                true if test_name != filter => return true,
                false if !test_name.contains(filter) => return true,
                _ => {}
            };
        }

        for skip_filter in &self.skip {
            match self.exact {
                true if test_name == skip_filter => return true,
                false if test_name.contains(skip_filter) => return true,
                _ => {}
            }
        }

        self.ignored != test.ignored
    }

    fn pretty(&self) -> bool {
        self.format.map_or(true, |it| it == Format::Pretty)
    }
}

fn print_tests(tests: &[Test], pretty: bool, ignored: bool) {
    let mut i = 0;
    for test in tests {
        if ignored == test.ignored {
            println!("{}: test", test.name,);
            i += 1;
        }
    }
    if pretty {
        println!();
        println!("{i} tests");
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Format {
    Pretty,
    Terse,
}

impl FromStr for Format {
    type Err = &'static str;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text {
            "pretty" => Ok(Format::Pretty),
            "terse" => Ok(Format::Terse),
            _ => Err("Unknown format"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
#[must_use = "Call `exit()` or `exit_if_failed()` to set the correct return code"]
pub struct TestSummary {
    pub failed: Vec<String>,
    pub passed: u32,
    pub ignored: u32,
    pub filtered: u32,
    pub elapsed: Duration,
}

impl TestSummary {
    /// Exits the application with an appropriate error code (0 if all tests
    /// have passed, 101 if there have been failures).
    pub fn exit(&self) -> ! {
        self.exit_if_failed();
        process::exit(0);
    }

    /// Exits the application with error code 101 if there were any failures.
    /// Otherwise, returns normally.
    pub fn exit_if_failed(&self) {
        if self.has_failed() {
            process::exit(101)
        }
    }

    /// Returns whether there have been any failures.
    pub fn has_failed(&self) -> bool {
        !self.failed.is_empty()
    }
}

impl Display for TestSummary {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { failed, passed, ignored, filtered, elapsed } = self;
        let succ = if failed.is_empty() { "ok" } else { "FAILED" };
        let failed = failed.len();
        let elapsed = elapsed.as_secs_f64();
        write!(f, "test result: {succ}. {passed} passed; {failed} failed; {ignored} ignored; {filtered} filtered out; finished in {elapsed:.2}s")
    }
}

/// This macro generates a main function for a libtest compatible test harness
/// It requires a list of tests available to the testsuite as a list of expressions
/// For examples to run two tests "foo" and "bar":
///
/// ```
/// # use direst::{harness, Failed, Test};
/// # fn foo() -> Result{ Ok(()) }
/// # fn bar() -> Result{ Ok(()) }
/// harness![
///     Test::new("foo", &foo),
///     Test::new("bar",  &bar)
/// ];
/// ```
///
/// These expressions are not constant and can run arbitrary code so tests can be generated programmatically.
/// Furthermore these expressions can return a iterator of tests instead of just a single test.
/// For example, datatests are supported with the `Test::from_dir` function this way:
///
/// ```
/// # use direst::{harness, Failed, Test};
/// # use std::path::Path;
/// fn data_test(path: &Path) -> Result {
/// /* run test on testfile path */
/// # Ok(())    
/// }
///
/// harness! [
///     Test::from_dir("data_test", &data_test, Path::new("foo_dir")),
///     Test::from_dir("data_test", &data_test, Path::new("bar_dir")),
///     Test::new("data_test::hardcoded", &|| data_test(Path::new("hardcoded_file")))
/// ];
/// ```

#[macro_export]
macro_rules! harness {
    ($($tests: expr),*) => {
        fn main() {
            use $crate::Test;
            let args = $crate::Arguments::parse_cli();
            let mut tests = ::std::vec::Vec::new();
            $($crate::TestOrTestList::push_to_list($tests, &mut tests);)*
            $crate::run_harness(&args, tests).exit()
        }
    };

    ($test_1: expr, $($tests: expr),*) => {
        $crate:harness{

        }
    };
}

pub trait TestOrTestList<'a> {
    fn push_to_list(self, dst: &mut Vec<Test<'a>>);
}

impl<'a> TestOrTestList<'a> for Test<'a> {
    fn push_to_list(self, dst: &mut Vec<Test<'a>>) {
        dst.push(self)
    }
}

impl<'a, I: IntoIterator<Item = Test<'a>>> TestOrTestList<'a> for I {
    fn push_to_list(self, dst: &mut Vec<Test<'a>>) {
        dst.extend(self)
    }
}
