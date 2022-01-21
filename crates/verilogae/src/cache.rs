use core::slice;
use std::mem::{size_of, size_of_val};
use std::path::PathBuf;

use anyhow::Result;
use basedb::lints::LintLevel;
use basedb::BaseDB;

use crate::compiler_db::CompilationDB;
use crate::Opts;

fn hash(db: &CompilationDB, module: Option<&str>) -> md5::Digest {
    let mut hash_builder = md5::Context::new();

    // hash settings
    hash_builder.consume(db.root_file.0.to_ne_bytes());
    if let Some(module) = module {
        hash_builder.consume(module);
    }

    hash_builder.consume(env!("CARGO_PKG_VERSION"));
    let lints = db.global_lint_overwrites(db.root_file);
    if cfg!(debug_assertions) && !lints.is_empty() {
        assert_eq!(size_of::<Option<LintLevel>>(), size_of_val(&lints.raw[0]));
    }
    let lints = unsafe {
        slice::from_raw_parts(
            lints.as_ptr() as *const u8,
            size_of::<Option<LintLevel>>() * lints.len(),
        )
    };
    hash_builder.consume(lints);

    // Hash the full preprocessor result
    let preprocess = db.preprocess(db.root_file);
    let vfs = db.vfs.read();
    for token in &*preprocess.ts {
        if !token.kind.is_trivia() {
            let filespan = token.span.to_file_span(&preprocess.sm);
            let src = vfs.file_contents_unchecked(filespan.file);
            hash_builder.consume(&src[filespan.range]);
            hash_builder.consume(" ");
        }
    }

    hash_builder.compute()
}

pub(crate) fn lookup(
    db: &CompilationDB,
    full_compile: bool,
    opts: &Opts,
) -> Result<(PathBuf, bool)> {
    let hash = u128::from_ne_bytes(*hash(db, opts.module_name()?));
    let hash = base_n::encode(hash, base_n::CASE_INSENSITIVE);
    let extension = if full_compile { "mod" } else { "modinfo" };
    let path = opts.cache_dir()?.join(format!("{}.{}", hash, extension));
    let exists = !cfg!(debug_assertions) && path.exists();
    Ok((path, exists))
}
