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
        let target =
            if self.windows { "x86_64-pc-windows-msvc" } else { "x86_64-unknown-linux-gnu" };
        cmd!(sh, "cargo build --release -p verilogae --target {target}").run()?;
        if self.force {
            sh.remove_path("wheels")?;
        } else if sh.read_dir("wheels").map_or(false, |dir| !dir.is_empty()) {
            bail!("wheels folder must be empty")
        }

        let pythons = find_py(self.windows);
        for (version, py, _tag) in &pythons {
            if self.windows {
                cmd!(sh, "{py} -m pip wheel . -w ./wheels --no-deps")
                    .env("PYO3_CROSS_PYTHON_VERSION", &version)
                    .env("CARGO_BUILD_TARGET", "x86_64-pc-windows-msvc")
                    .run()?;
            } else {
                cmd!(sh, "{py} -m pip install setuptools_rust").run()?;
                cmd!(sh, "{py} -m pip wheel . -w ./wheels --no-deps")
                    .env("PYO3_PYTHON", &py)
                    .run()?;
            }
            // cmd!("auditwheel repair "$whl" -w /io/dist/")
        }

        if self.manylinux {
            for file in sh.read_dir("wheels")? {
                cmd!(sh, "auditwheel repair {file}  -w wheels").run()?;
                std::fs::remove_file(file)?;
            }
        } else if !self.windows {
            for file in sh.read_dir("wheels")? {
                cmd!(sh, "auditwheel repair {file}  -w wheels --plat linux_x86_64").run()?;
            }
        }

        if self.install {
            for file in sh.read_dir("wheels")? {
                println!("{file:?}");
                for (_, py, tag) in &pythons {
                    if file.to_str().unwrap().contains(tag) {
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
        for (_, py, _tag) in find_py(false) {
            cmd!(sh, "{py} -m pip install numpy").run()?;
            let _dir1 = sh.push_dir("verilogae");
            let _dir2 = sh.push_dir("tests");
            cmd!(sh, "{py} test_hicum.py").run()?;
        }

        Ok(())
    }
}

impl crate::flags::Publish {
    pub fn run(self, sh: &mut Shell) -> Result<()> {
        crate::flags::Build { force: true, manylinux: true, install: true, windows: self.windows }
            .run(sh)?;
        if !self.windows {
            crate::flags::Test.run(sh)?;
        }
        let files = sh.read_dir("wheels")?;
        cmd!(sh, "twine upload {files...}").run()?;
        Ok(())
    }
}

const MIN_PYTHON_MINOR: u32 = 8;
const MAX_PYTHON_MINOR: u32 = 11;
// const MAX_PYPY_MINOR: u32 = 8;

fn find_py(windows: bool) -> Vec<(String, String, String)> {
    (MIN_PYTHON_MINOR..=MAX_PYTHON_MINOR)
        .map(|minor| {
            (format!("3.{}", minor), format!("python3.{}", minor), format!("cp3{}", minor))
        })
        .filter(|(_version, exe, _tag)| windows || Command::new(exe).output().is_ok())
        .collect()
}
