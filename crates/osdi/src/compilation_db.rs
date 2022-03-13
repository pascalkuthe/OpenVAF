use std::fs;
use std::intrinsics::transmute;
use std::iter::once;
use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result};
use basedb::lints::{Lint, LintLevel};
use basedb::{BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsPath, VfsStorage, STANDARD_FLAGS};
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_ty::db::HirTyDatabase;
use parking_lot::RwLock;
use paths::AbsPathBuf;
use salsa::ParallelDatabase;
use typed_index_collections::TiSlice;

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase, HirTyDatabase)]
pub struct CompilationDB {
    storage: salsa::Storage<CompilationDB>,
    pub vfs: Arc<RwLock<Vfs>>,
    pub root_file: FileId,
}

impl Upcast<dyn HirDefDB> for CompilationDB {
    fn upcast(&self) -> &(dyn HirDefDB + 'static) {
        &*self
    }
}

impl Upcast<dyn BaseDB> for CompilationDB {
    fn upcast(&self) -> &(dyn BaseDB + 'static) {
        self
    }
}

impl CompilationDB {
    pub fn new(root_file: &std::path::Path) -> Result<Self> {
        let mut vfs = Vfs::default();
        vfs.insert_std_lib();

        let root_file = abs_path(root_file)?;
        let contents = fs::read(&root_file);
        let root_file = vfs.ensure_file_id(root_file.into());
        vfs.set_file_contents(root_file, contents.into());

        let mut res =
            Self { storage: salsa::Storage::default(), vfs: Arc::new(RwLock::new(vfs)), root_file };

        let include_dirs: Result<Arc<[_]>> = once(Ok(VfsPath::new_virtual_path("/std".to_owned())))
            // .chain(opts.include_dirs().map(|it| Ok(VfsPath::from(it?))))
            .collect();
        res.set_include_dirs(root_file, include_dirs?);

        let macro_flags: Vec<_> = STANDARD_FLAGS
            .into_iter()
            // .chain(opts.macro_flags())
            .map(Arc::from)
            .collect();
        res.set_macro_flags(root_file, Arc::from(macro_flags));

        res.set_plugin_lints(&[]);
        let overwrites = res.empty_global_lint_overwrites();
        // let registry = res.lint_registry();

        // let allow_lints = zip(opts.allow_lints(), repeat(LintLevel::Allow));
        // let warn_lints = zip(opts.warn_lints(), repeat(LintLevel::Warn));
        // let deny_lints = zip(opts.deny_lints(), repeat(LintLevel::Deny));

        //         let mut sink = ConsoleSink::new(Config::default(), &res);
        //         for (lint, lvl) in allow_lints.chain(warn_lints).chain(deny_lints) {
        //             if let Some(lint) = registry.lint_from_name(lint) {
        //                 overwrites[lint] = Some(lvl)
        //             } else {
        //                 sink.print_simple_message(
        //                     Severity::Warning,
        //                     format!("no lint named '{}' was found!", lint),
        //                 )
        //             }
        //         }
        //         drop(sink);

        let overwrites: Arc<[_]> = Arc::from(overwrites.as_ref());
        let overwrites = unsafe {
            transmute::<Arc<[Option<LintLevel>]>, Arc<TiSlice<Lint, Option<LintLevel>>>>(overwrites)
        };

        res.set_global_lint_overwrites(root_file, overwrites);
        Ok(res)
    }
}

impl ParallelDatabase for CompilationDB {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        let db = CompilationDB {
            storage: self.storage.snapshot(),
            vfs: self.vfs.clone(),
            root_file: self.root_file,
        };

        salsa::Snapshot::new(db)
    }
}

/// This impl tells salsa where to find the salsa runtime.
impl salsa::Database for CompilationDB {}
impl VfsStorage for CompilationDB {
    fn vfs(&self) -> &RwLock<Vfs> {
        &self.vfs
    }
}

pub fn abs_path(path: &Path) -> Result<AbsPathBuf> {
    let path = path.canonicalize().with_context(|| format!("failed to read {}", path.display()))?;
    let path = AbsPathBuf::assert(path);
    Ok(path.normalize())
}
