use std::path::Path;

use anyhow::{bail, Result};
use xshell::{cmd, read_file, rm_rf, write_file};

use crate::flags;

/// Extract dependencies from Cargo.lock and create deterministic hash
pub fn lockfile_hash() -> Result<String> {
    let mut lock = Vec::new();
    let lock_file = read_file("Cargo.lock")?;
    // Only hash the dependencies not the local crates
    for package in lock_file.split("\n[[package]]") {
        if !package.contains("source") {
            continue;
        }
        let mut name = None;
        let mut version = None;
        for line in package.split('\n') {
            if line.contains("name ") {
                name = line.split('\"').nth(1)
            }
            if line.contains("version ") {
                version = line.split('\"').nth(1)
            }
        }

        let (name, version) = (name.unwrap(), version.unwrap());
        lock.push((name, version))
    }
    lock.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));
    lock.dedup(); // Should not be necessary but better be save
    let mut hash = md5::Context::new();
    for (name, version) in lock {
        hash.consume(format!("{}:{}\n", name, version))
    }

    let hash = u128::from_le_bytes(hash.compute().into());
    Ok(base_n::encode(hash, base_n::CASE_INSENSITIVE))
}

fn vendor_file() -> Result<String> {
    Ok(format!("vendor-{}.tar.zst", lockfile_hash()?))
}

impl flags::Vendor {
    pub fn run(self) -> Result<()> {
        let vendor_file = vendor_file()?;
        let old_file = read_file(".vendor/hash")?;
        if old_file == vendor_file && !self.force {
            return Ok(());
        }

        if self.check {
            bail!("Cargo Dependency hash has changed! Please run cargo xtask vendor to update it!");
        }

        write_file(".vendor/hash", &vendor_file)?;

        let vendor_config = cmd!("cargo vendor --locked").read()?;
        let vendor_pull = format!(
            r#"#!/usr/bin/env bash 
if s3cmd get s3://openva/{file} {file}; then
    tar --zstd -xf "{file}"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${{found+x}} ]; then
    echo -e {config:?} >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi"#,
            file = vendor_file,
            config = vendor_config
        );

        write_file(".vendor/pull.sh", vendor_pull)?;

        cmd!("tar --zstd -cf {vendor_file} ./vendor").run()?;

        if Path::new("./.maintainer").exists() {
            if !self.no_upload {
                cmd!("s3cmd put --acl-public {vendor_file} s3://openva/{vendor_file}").run()?;
            }
        } else {
            eprintln!(
    "\x1b[33;1mwarning\x1b[0m: The cargo dependency hash has changed!
    You are not marked as maintainer and therefore can't update the build caches.
    This will result in longer CI runtimes.
    When your patch is submitted a maintainer will update the caches after auditing the changes
    If you are a maintainer please add the .maintainer file and try uploading again (cargo xtask vendor --force)");
        }

        eprintln!("\x1b[33;1mwarning\x1b[0m: .vendor has changed! Please make sure to commit these changes before any changes that require these changed dependenceis");
        // cleanup
        rm_rf("vendor")?;
        rm_rf(vendor_file)?;

        Ok(())
    }
}
