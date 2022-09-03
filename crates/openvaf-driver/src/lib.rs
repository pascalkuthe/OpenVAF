use std::fs;
use std::io::Write;
use std::path::Path;
use std::time::Instant;

use anyhow::Context;
pub use anyhow::Result;
use clap::ArgMatches;
pub use cli_def::main_command;
pub use cli_process::Opts;
use linker::link;
use mir_llvm::LLVMBackend;
use paths::AbsPathBuf;
use sim_back::CompilationDB;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

pub mod cache;
pub mod cli_def;
mod cli_process;

const DATA_ERROR: i32 = 65;

pub fn run(matches: ArgMatches) -> Result<i32> {
    let start = Instant::now();
    let opts = Opts::new(matches)?;

    let path = AbsPathBuf::assert(opts.input.canonicalize()?);
    let db = CompilationDB::new(path, &opts.include, &opts.defines, &opts.lints)?;
    let modules = if let Some(modules) = db.collect_modules() {
        modules
    } else {
        return Ok(DATA_ERROR);
    };

    let out = if opts.batch {
        let (path, exists) = cache::lookup(&db, &opts);
        if exists {
            print_path(path.as_ref());
            return Ok(0);
        }
        fs::create_dir_all(opts.cache_dir).context("failed to create cache directory")?;
        path
    } else {
        opts.output
    };

    let back = LLVMBackend::new(&opts.codegen_opts, &opts.target, opts.target_cpu, &[]);
    let paths =
        osdi::compile(&db, &modules, opts.input.as_ref(), &opts.target, &back, true, opts.opt_lvl);
    link(&opts.target, out.as_ref(), |linker| {
        for path in &paths {
            linker.add_object(path);
        }
    })?;

    for obj_file in paths {
        std::fs::remove_file(obj_file).context("failed to delete intermediate compile artifact")?;
    }

    let seconds = Instant::elapsed(&start).as_secs_f64();
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    stderr.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    write!(&mut stderr, "Finished")?;
    stderr.set_color(&ColorSpec::new())?;
    writeln!(&mut stderr, " building {} in {:.2}s", opts.input.file_name().unwrap(), seconds)?;

    if opts.batch {
        print_path(out.as_ref());
    }

    Ok(0)
}

#[cfg(unix)]
fn print_path(path: &Path) {
    use std::io::stdout;
    use std::os::unix::ffi::OsStrExt;

    let path = path.as_os_str();
    let raw = path.as_bytes();
    stdout().write_all(raw).unwrap();
    println!()
}

#[cfg(not(unix))]
fn print_path(path: &Path) {
    // appearntly this is good enoug for ripgrep so its probably good enough for me aswell
    println!("{}", path.to_string_lossy());
}
