use std::intrinsics::transmute;
use std::iter::once;
use std::ops::Deref;
use std::sync::Arc;
use std::{fs, io};

use anyhow::{bail, Result};
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
        self
    }
}

impl Upcast<dyn BaseDB> for CompilationDB {
    fn upcast(&self) -> &(dyn BaseDB + 'static) {
        self
    }
}

impl CompilationDB {
    pub fn new(
        root_file: AbsPathBuf,
        include_dirs: &[AbsPathBuf],
        macro_flags: &[String],
        lints: &[(String, LintLevel)],
    ) -> Result<Self> {
        let contents = fs::read(&root_file);
        CompilationDB::new_(root_file.into(), contents, include_dirs, macro_flags, lints)
    }

    pub fn new_(
        root_file: VfsPath,
        contents: Result<Vec<u8>, io::Error>,
        include_dirs: &[AbsPathBuf],
        macro_flags: &[String],
        lints: &[(String, LintLevel)],
    ) -> Result<Self> {
        let mut vfs = Vfs::default();
        vfs.insert_std_lib();

        let root_file = vfs.ensure_file_id(root_file);
        vfs.set_file_contents(root_file, contents.into());

        let mut res =
            Self { storage: salsa::Storage::default(), vfs: Arc::new(RwLock::new(vfs)), root_file };

        let include_dirs: Result<Arc<[_]>> = once(Ok(VfsPath::new_virtual_path("/std".to_owned())))
            .chain(include_dirs.iter().map(|it| Ok(VfsPath::from(it.clone()))))
            .collect();
        res.set_include_dirs(root_file, include_dirs?);

        let macro_flags: Vec<_> = STANDARD_FLAGS
            .into_iter()
            .chain(macro_flags.iter().map(String::deref))
            .map(Arc::from)
            .collect();
        res.set_macro_flags(root_file, Arc::from(macro_flags));

        res.set_plugin_lints(&[]);
        let mut overwrites = res.empty_global_lint_overwrites();
        let registry = res.lint_registry();

        fn replace_lvl(
            overwrites: &mut TiSlice<Lint, Option<LintLevel>>,
            registry: &basedb::lints::LintRegistry,
            replaced_lvl: LintLevel,
            new_lvl: LintLevel,
        ) {
            for (lint, dst) in overwrites.iter_mut_enumerated() {
                let old_lvl = dst.unwrap_or_else(|| registry.lint_data(lint).default_lvl);
                if old_lvl == replaced_lvl {
                    *dst = Some(new_lvl)
                }
            }
        }

        for (lint, lvl) in lints {
            match &**lint {
                "all" => overwrites.raw.fill(Some(*lvl)),
                "warnings" => replace_lvl(&mut overwrites, &registry, LintLevel::Warn, *lvl),
                "errors" => replace_lvl(&mut overwrites, &registry, LintLevel::Deny, *lvl),
                lint => {
                    if let Some(lint) = registry.lint_from_name(lint) {
                        overwrites[lint] = Some(*lvl)
                    } else {
                        bail!("unknown lint {lint}")
                    }
                }
            }
        }

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
