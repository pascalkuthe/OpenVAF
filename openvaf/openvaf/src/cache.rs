use core::slice;
use std::mem::{size_of, size_of_val};

use basedb::lints::LintLevel;
use basedb::BaseDB;
use sim_back::CompilationDB;

use crate::Opts;

fn hash(db: &CompilationDB, defines: &[String]) -> md5::Digest {
    let mut hash_builder = md5::Context::new();

    // hash settings
    hash_builder.consume(db.root_file.0.to_ne_bytes());

    hash_builder.consume(defines.len().to_ne_bytes());
    for def in defines {
        hash_builder.consume(def)
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

pub fn file_name(db: &CompilationDB, opts: &Opts) -> String {
    let hash = u128::from_ne_bytes(*hash(db, &opts.defines));
    let hash = base_n::encode(hash, base_n::CASE_INSENSITIVE);
    format!("{}.osdi", hash)
}
