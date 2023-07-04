mod prepare;

use std::path::Path;

use anyhow::{bail, Result};
use xshell::{cmd, read_file, write_file};

use crate::flags::{self, Prepare};
use crate::vendor::lockfile_hash;

impl flags::Cache {
    pub(crate) fn run(self) -> Result<()> {
        match self.subcommand {
            flags::CacheCmd::Prepare(cmd) => cmd.run(),
            flags::CacheCmd::Update(cmd) => cmd.run(),
            flags::CacheCmd::Fetch(cmd) => cmd.run(),
            flags::CacheCmd::Upload(cmd) => cmd.run(),
            flags::CacheCmd::Create(cmd) => cmd.run(),
        }
    }
}

impl flags::Create {
    fn run(self) -> Result<()> {
        Prepare.run()?;
        let name = cache_name()?;
        cmd!("tar --zstd -cf {name} ./target").run()?;
        Ok(())
    }
}

impl flags::Update {
    fn run(self) -> Result<()> {
        if Path::new("./target/.cache_cookie").exists() {
            return Ok(());
        }

        if std::env::var("CI").is_ok() {
            let home = std::env::var("HOME")?;
            if !Path::new(&format!("{}/.s3cfg", home)).exists() {
                eprintln!("\x1b[33;1mwarning\x1b[0m: can't update caches without access token\n\tAfter auditing you patch a maintainer should do this");
                return Ok(());
            }
        }

        flags::Upload.run()
    }
}

impl flags::Upload {
    pub fn run(self) -> Result<()> {
        flags::Create.run()?;
        let name = cache_name()?;
        cmd!("s3cmd put --acl-public {name} s3://openva/{name}").run()?;
        Ok(())
    }
}

impl flags::Fetch {
    pub(crate) fn run(self) -> Result<()> {
        let name = cache_name()?;
        if let Err(err) = cmd!("s3cmd get s3://openva/{name} {name}").run() {
            eprintln!("\x1b[33;1mwarning\x1b[0m: failed to retrieve cache: {}", err);
            // target dir will be generated on build
        } else {
            cmd!("tar --zstd -xf {name}").run()?;
            // place a cookie so that we know that caches are already present
            write_file("./target/.cache_cookie", ".")?;
        }

        // update config to include vendor directory
        let config = read_file("./.cargo/config")?;
        write_file(
            "./.cargo/config",
            format!(
                r#"{}

        "#,
                config
            ),
        )?;
        Ok(())
    }
}

fn cache_name() -> Result<String> {
    let lockfile = lockfile_hash()?;
    let rust_version = cmd!("rustc --version").read()?;
    if rust_version.contains("nightly") {
        bail!("caches are not provided for nightly releases: {}", rust_version)
    }
    let rust_version_components: Vec<_> = rust_version.split(' ').collect();

    let rust_version_stem = match rust_version_components.as_slice() {
        [rustc, version, ..] => {
            assert_eq!(*rustc, "rustc");
            version
        }
        _ => bail!("rustc --version returned invalid result {}", rust_version),
    };

    Ok(format!("target-{}-{}.tar.zst", rust_version_stem, lockfile))
}
