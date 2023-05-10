use std::fs::{create_dir_all, remove_file};
use std::io::Write;
use std::time::Instant;

use anyhow::Context;
use anyhow::Result;
use basedb::diagnostics::{Chars, ConsoleSink, DiagnosticSink};
use basedb::{diagnostics, BaseDB};
use camino::Utf8PathBuf;
use linker::link;
use mir_llvm::LLVMBackend;
use sim_back::CompilationDB;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

pub use basedb::lints::builtin as builtin_lints;
pub use basedb::lints::LintLevel;
pub use llvm::OptLevel;
pub use paths::AbsPathBuf;
pub use target::host_triple;
pub use target::spec::{get_target_names, Target};

mod cache;

#[derive(Debug, Clone)]
pub enum CompilationDestination {
    Path { lib_file: Utf8PathBuf },
    Cache { cache_dir: Utf8PathBuf },
}

pub enum CompilationTermination {
    Compiled { lib_file: Utf8PathBuf },
    FatalDiagnostic,
}

#[derive(Debug, Clone)]
pub struct Opts {
    pub defines: Vec<String>,
    pub codegen_opts: Vec<String>,
    pub lints: Vec<(String, LintLevel)>,
    pub input: Utf8PathBuf,
    pub output: CompilationDestination,
    pub include: Vec<AbsPathBuf>,
    pub opt_lvl: OptLevel,
    pub target: Target,
    pub target_cpu: String,
}

pub fn expand(opts: &Opts) -> Result<CompilationTermination> {
    let start = Instant::now();

    let input =
        opts.input.canonicalize().with_context(|| format!("failed to resolve {}", opts.input))?;
    let input = AbsPathBuf::assert(input);
    let db = CompilationDB::new(input, &opts.include, &opts.defines, &opts.lints)?;

    let preprocess = db.preprocess(db.root_file);

    for token in preprocess.ts.iter() {
        let span = token.span.to_file_span(&preprocess.sm);
        let text = db.file_text(span.file).unwrap();
        print!("{}", &text[span.range]);
    }
    println!();

    let mut config =
        diagnostics::Config { chars: Chars::ascii(), ..diagnostics::Config::default() };
    config.styles.header_error.set_intense(false);
    config.styles.header_warning.set_intense(false);
    config.styles.header_help.set_intense(false);
    config.styles.header_bug.set_intense(false);
    config.styles.header_note.set_intense(false);

    config.styles.note_bullet.set_bold(true).set_intense(true);
    config.styles.line_number.set_bold(true).set_intense(true);
    config.styles.source_border.set_bold(true).set_intense(true);
    config.styles.primary_label_bug.set_bold(true);
    config.styles.primary_label_note.set_bold(true);
    config.styles.primary_label_help.set_bold(true);
    config.styles.primary_label_error.set_bold(true);
    config.styles.primary_label_warning.set_bold(true);
    config.styles.secondary_label.set_bold(true);
    let mut sink = ConsoleSink::new(config, &db);
    sink.add_diagnostics(&*preprocess.diagnostics, db.root_file, &db);

    if sink.summary(&opts.input.file_name().unwrap()) {
        return Ok(CompilationTermination::FatalDiagnostic);
    }

    let seconds = Instant::elapsed(&start).as_secs_f64();
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    stderr.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    write!(&mut stderr, "Finished")?;
    stderr.set_color(&ColorSpec::new())?;
    writeln!(&mut stderr, " preprocessing {} in {:.2}s", opts.input.file_name().unwrap(), seconds)?;

    Ok(CompilationTermination::Compiled { lib_file: Utf8PathBuf::default() })
}

pub fn compile(opts: &Opts) -> Result<CompilationTermination> {
    let start = Instant::now();

    let input =
        opts.input.canonicalize().with_context(|| format!("failed to resolve {}", opts.input))?;
    let input = AbsPathBuf::assert(input);
    let db = CompilationDB::new(input, &opts.include, &opts.defines, &opts.lints)?;

    let preprocess = db.preprocess(db.root_file);

    if !preprocess.diagnostics.is_empty() {
        db.collect_modules();
        return Ok(CompilationTermination::FatalDiagnostic);
    }

    let lib_file = match &opts.output {
        CompilationDestination::Cache { cache_dir } => {
            let file_name = cache::file_name(&db, opts);
            let lib_file = cache_dir.join(file_name);
            if cfg!(not(debug_assertions)) && lib_file.exists() {
                return Ok(CompilationTermination::Compiled { lib_file });
            }
            create_dir_all(cache_dir).context("failed to create cache directory")?;
            lib_file
        }
        CompilationDestination::Path { lib_file } => lib_file.clone(),
    };

    let modules = if let Some(modules) = db.collect_modules() {
        modules
    } else {
        return Ok(CompilationTermination::FatalDiagnostic);
    };

    let back = LLVMBackend::new(&opts.codegen_opts, &opts.target, opts.target_cpu.clone(), &[]);
    let paths = osdi::compile(&db, &modules, &lib_file, &opts.target, &back, true, opts.opt_lvl);
    // TODO configure linker
    link(None, &opts.target, lib_file.as_ref(), |linker| {
        for path in &paths {
            linker.add_object(path);
        }
    })?;

    for obj_file in paths {
        remove_file(obj_file).context("failed to delete intermediate compile artifact")?;
    }

    let seconds = Instant::elapsed(&start).as_secs_f64();
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    stderr.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    write!(&mut stderr, "Finished")?;
    stderr.set_color(&ColorSpec::new())?;
    writeln!(&mut stderr, " building {} in {:.2}s", opts.input.file_name().unwrap(), seconds)?;

    Ok(CompilationTermination::Compiled { lib_file })
}
