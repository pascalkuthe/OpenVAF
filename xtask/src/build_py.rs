use std::process::Command;

use anyhow::{bail, Result};
use xshell::{cmd, Shell};

use crate::flags::Verilogae;

impl Verilogae {
    pub fn run(self, sh: &mut Shell) -> Result<()> {
        match self.subcommand {
            crate::flags::VerilogaeCmd::Build(cmd) => cmd.run(sh),
            crate::flags::VerilogaeCmd::Test(cmd) => cmd.run(sh),
            crate::flags::VerilogaeCmd::Publish(cmd) => cmd.run(sh),
        }
    }
}

impl crate::flags::Build {
    fn run(self, sh: &mut Shell) -> Result<()> {
        let _env = sh.push_env("RUSTFLAGS", "-C strip=symbols");
        cmd!(sh, "cargo build --release -p verilogae").run()?;
        if self.force {
            sh.remove_path("wheels")?;
        } else if sh.read_dir("wheels").map_or(false, |dir| !dir.is_empty()) {
            bail!("wheels folder must be empty")
        }

        let pythons = find_py();
        let ld_lib_path = std::env::var("LD_LIBRARY_PATH").unwrap_or_default();
        let ld_lib_path =
            format!("{}:{}/target/release", ld_lib_path, sh.current_dir().to_str().unwrap());
        for (py, _tag) in &pythons {
            cmd!(sh, "{py} -m pip wheel . -w ./wheels --no-deps")
                .env("PYO3_PYTHON", &py)
                .env("LD_LIBRARY_PATH", &ld_lib_path)
                .run()?;
            // cmd!("auditwheel repair "$whl" -w /io/dist/")
        }

        if self.manylinux {
            for file in sh.read_dir("wheels")? {
                cmd!(sh, "auditwheel repair {file} -w wheels")
                    .env("LD_LIBRARY_PATH", &ld_lib_path)
                    .run()?;
                std::fs::remove_file(file)?;
            }
        }

        if self.install {
            for file in sh.read_dir("wheels")? {
                for (py, tag) in &pythons {
                    if file.to_str().unwrap().contains(&*tag) {
                        cmd!(sh, "{py} -m pip install --force-reinstall {file}").run()?;
                        break;
                    }
                }
            }
        }

        Ok(())
    }
}

impl crate::flags::Test {
    pub fn run(self, sh: &mut Shell) -> Result<()> {
        for (py, _tag) in find_py() {
            let _dir1 = sh.push_dir("verilogae");
            let _dir2 = sh.push_dir("tests");
            cmd!(sh, "{py} test_hicum.py").run()?;
        }

        Ok(())
    }
}

impl crate::flags::Publish {
    pub fn run(self, sh: &mut Shell) -> Result<()> {
        crate::flags::Build { force: true, manylinux: true, install: true }.run(sh)?;
        let files = sh.read_dir("wheels")?;
        cmd!(sh, "twine upload {files...}").run()?;
        Ok(())
    }
}

const MIN_PYTHON_MINOR: u32 = 7;
const MAX_PYTHON_MINOR: u32 = 10;
// const MAX_PYPY_MINOR: u32 = 8;

fn find_py() -> Vec<(String, String)> {
    (MIN_PYTHON_MINOR..=MAX_PYTHON_MINOR)
        .map(|minor| (format!("python3.{}", minor), format!("cp3{}", minor)))
        // .chain( TODO pypy  support
        //     (MIN_PYTHON_MINOR..=MAX_PYPY_MINOR)
        //         .map(|minor| (format!("pypy3.{}", minor), format!("pp3{}", minor))),
        // )
        .filter(|(exe, _tag)| Command::new(exe).output().is_ok())
        .collect()
}
