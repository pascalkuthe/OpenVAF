use std::intrinsics::transmute;
use std::iter::{self, once};
use std::ops::Deref;
use std::sync::Arc;
use std::{fs, io};

use anyhow::{bail, Result};
use basedb::lints::{Lint, LintLevel};
use basedb::AbsPathBuf;
use basedb::{BaseDB, BaseDatabase, FileId, Vfs, VfsPath, VfsStorage, STANDARD_FLAGS};
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_ty::db::HirTyDatabase;
use parking_lot::RwLock;
use salsa::ParallelDatabase;
use stdx::Upcast;
use typed_index_collections::TiSlice;

use crate::CompilationUnit;

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase, HirTyDatabase)]
pub struct CompilationDB {
    storage: salsa::Storage<CompilationDB>,
    vfs: Arc<RwLock<Vfs>>,
    root_file: FileId,
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
    pub fn new_fs(
        root_file: AbsPathBuf,
        include_dirs: &[AbsPathBuf],
        macro_flags: &[String],
        lints: &[(String, LintLevel)],
    ) -> Result<Self> {
        let contents = fs::read(&root_file);
        CompilationDB::new(
            root_file.into(),
            contents,
            include_dirs.iter().map(|path| Ok(VfsPath::from(path.clone()))),
            macro_flags.iter().map(String::deref),
            lints.iter().map(|(name, lvl)| (&**name, *lvl)),
        )
    }

    /// Utility function to create a database with default settings
    /// and a single virtual root file
    pub fn new_virtual(contents: &str) -> Result<Self> {
        CompilationDB::new(
            VfsPath::new_virtual_path("/root.va".to_owned()),
            Ok(contents.as_bytes().to_owned()),
            iter::empty(),
            iter::empty(),
            iter::empty(),
        )
    }

    pub fn compilation_unit(&self) -> CompilationUnit {
        CompilationUnit { root_file: self.root_file }
    }

    pub fn new<'a>(
        root_file: VfsPath,
        contents: Result<Vec<u8>, io::Error>,
        include_dirs: impl Iterator<Item = Result<VfsPath>>,
        macro_flags: impl Iterator<Item = &'a str>,
        lints: impl Iterator<Item = (&'a str, LintLevel)>,
    ) -> Result<Self> {
        let mut vfs = Vfs::default();
        vfs.insert_std_lib();

        let root_file = vfs.ensure_file_id(root_file);
        vfs.set_file_contents(root_file, contents.into());

        let mut res =
            Self { storage: salsa::Storage::default(), vfs: Arc::new(RwLock::new(vfs)), root_file };

        let include_dirs: Result<Arc<[_]>> =
            once(Ok(VfsPath::new_virtual_path("/std".to_owned()))).chain(include_dirs).collect();
        res.set_include_dirs(root_file, include_dirs?);

        let macro_flags: Arc<[_]> =
            STANDARD_FLAGS.into_iter().chain(macro_flags).map(Arc::from).collect();
        res.set_macro_flags(root_file, macro_flags);

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
            match lint {
                "all" => overwrites.raw.fill(Some(lvl)),
                "warnings" => replace_lvl(&mut overwrites, &registry, LintLevel::Warn, lvl),
                "errors" => replace_lvl(&mut overwrites, &registry, LintLevel::Deny, lvl),
                lint => {
                    if let Some(lint) = registry.lint_from_name(lint) {
                        overwrites[lint] = Some(lvl)
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
