use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use anyhow::{bail, Context, Result};
use backend::compile_to_cfg;
use basedb::{BaseDB, VfsStorage};
use codegen_llvm::LLVMBackend;
use hir_lower::PlaceKind;
use lasso::Rodeo;
use linker::link;
use program_dependence::{AssigmentInterner, ProgramDependenGraph};
use salsa::ParallelDatabase;
use stdx::iter::zip;
use stdx::pretty;
use termcolor::ColorChoice::Auto;
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

use crate::api::{Opts, VfsEntry};
use crate::compiler_db::{CompilationDB, ModelInfo};
use crate::opts::abs_path;
pub use llvm::OptLevel;

#[cfg(windows)]
use libloading::os::windows::Library;

#[cfg(unix)]
use libloading::os::unix::Library;

pub mod api;
mod back;
mod cache;
mod compiler_db;
mod middle;
mod opts;

pub fn export_vfs(path: &Path, opts: &Opts) -> Result<Box<[VfsEntry]>> {
    let db = CompilationDB::new(path, opts)?;
    db.preprocess(db.root_file);
    let vfs = db.vfs().read();
    let path = abs_path(path)?;
    let (vfs_export, unresolved_files) = match vfs.export_native_paths_to_virt(&path) {
        Ok(res) => res,
        Err(err) => bail!("{}", err),
    };

    if !unresolved_files.is_empty() {
        bail!("Failed to crates vfs for {:?}\nThe following files are not contained within the same directoy {:?}", path, pretty::List::with_final_seperator(&unresolved_files, ", "));
    }

    let res = vfs_export
        .into_iter()
        .map(|(path, contents)| VfsEntry { name: path.into(), data: contents.into() })
        .collect();

    Ok(res)
}

pub fn load(path: &Path, full_compile: bool, opts: &Opts) -> Result<Library> {
    let lib = build_local_model(path, full_compile, opts)?;
    let lib = unsafe { Library::new(lib).expect("failed to open lib") };
    Ok(lib)
}

fn build_local_model(path: &Path, full_compile: bool, opts: &Opts) -> Result<PathBuf> {
    let db = CompilationDB::new(path, opts)?;
    let (file, found) = cache::lookup(&db, full_compile, opts)?;
    if found {
        return Ok(file);
    }

    db.build_model(path, full_compile, true, opts, &file).map(|_| file)
}

impl CompilationDB {
    fn build_model(
        self,
        path: &Path,
        full_compile: bool,
        local: bool,
        opts: &Opts,
        dst: &Path,
    ) -> Result<()> {
        let start = Instant::now();
        let db = self;

        let file = path.file_name().to_owned().unwrap().to_string_lossy();

        // FIXME display osstring directly instead of wrapping in a path? wtf does osstr not have a
        // display method
        let info = ModelInfo::collect(&db, &file, opts.module_name()?)?;

        let db_snap = db.snapshot();
        let target = opts.target(local)?;
        let cg_opts: Vec<_> = opts.cg_flags().map(str::to_owned).collect();
        let backend = LLVMBackend::new(&cg_opts, &target, opts.opt_lvl.into());
        let cache_dir = opts.cache_dir()?;

        std::fs::create_dir_all(&cache_dir).unwrap();
        if let Some(parent) = dst.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }

        let mut object_files = vec![cache_dir.join(format!("{}_modelinfo.o", file))];

        if full_compile {
            let (mut cfg, intern, mut literals) =
                compile_to_cfg(&*db_snap, info.module, false, false, false, &mut |places, dst| {
                    for fun in &info.functions {
                        dst.insert(places.ensure(PlaceKind::Var(fun.var)).0);
                    }
                });

            back::compile_model_info(
                &db_snap,
                Some(&intern),
                &info,
                &mut literals,
                &backend,
                &object_files[0],
            );

            let dst_name = dst.file_name().to_owned().unwrap().to_string_lossy();
            object_files.extend(
                info.functions
                    .iter()
                    .map(|fun| cache_dir.join(format!("{}{}.o", dst_name, fun.prefix))),
            );
            cfg.cannonicalize_ret();
            let assignments = AssigmentInterner::new(&cfg);
            let pdg = ProgramDependenGraph::build(&assignments, &cfg);

            rayon_core::scope(|s| {
                let db = db;
                for (fun, file) in zip(&info.functions, &object_files[1..]) {
                    let db_snap = db.snapshot();
                    s.spawn(|_| {
                        let db_snap = db_snap;
                        let (mut intern, cfg) =
                            middle::create_slice(&cfg, &pdg, &intern, fun, true);
                        back::compile_fun(
                            &backend,
                            &db_snap,
                            &info,
                            &mut intern,
                            &cfg,
                            &literals,
                            file,
                            fun,
                            &fun.prefix,
                        )
                    })
                }
            })
        } else {
            let mut literals = Rodeo::default();
            back::compile_model_info(
                &db_snap,
                None,
                &info,
                &mut literals,
                &backend,
                &object_files[0],
            );
        }

        link(&target, None, None, dst, |linker| {
            for obj in &object_files {
                linker.add_object(obj)
            }
        })
        .context("linking failed!")?;

        #[allow(unused_must_use)]
        for file in object_files {
            fs::remove_file(file);
        }

        let seconds = Instant::elapsed(&start).as_secs_f64();
        let mut stderr = StandardStream::stderr(Auto);
        stderr.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
        write!(&mut stderr, "Finished")?;
        stderr.set_color(&ColorSpec::new())?;
        writeln!(&mut stderr, " building {} in {:.2}s", file, seconds)?;

        Ok(())
    }
}
