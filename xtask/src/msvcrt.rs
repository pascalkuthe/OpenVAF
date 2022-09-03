use std::path::Path;

use crate::flags::GenMsvcrt;

use anyhow::Result;
use xshell::{cmd, Shell};
const MINGW_URL: &str = "https://github.com/mirror/mingw-w64.git";

const UCRT_FILES: &[&str] = &[
    "api-ms-win-crt-conio-l1-1-0",
    "api-ms-win-crt-convert-l1-1-0",
    "api-ms-win-crt-environment-l1-1-0",
    "api-ms-win-crt-filesystem-l1-1-0",
    "api-ms-win-crt-heap-l1-1-0",
    "api-ms-win-crt-locale-l1-1-0",
    "api-ms-win-crt-math-l1-1-0",
    "api-ms-win-crt-multibyte-l1-1-0",
    "api-ms-win-crt-private-l1-1-0",
    "api-ms-win-crt-process-l1-1-0",
    "api-ms-win-crt-runtime-l1-1-0",
    "api-ms-win-crt-stdio-l1-1-0",
    "api-ms-win-crt-string-l1-1-0",
    "api-ms-win-crt-time-l1-1-0",
    "api-ms-win-crt-utility-l1-1-0",
];

impl GenMsvcrt {
    pub fn run(&self, sh: &Shell) -> Result<()> {
        let tag = "v10.0.0";
        let src_dir = sh.current_dir().join("mingw");

        cmd!(sh, "git clone --depth 1 --single-branch --branch {tag} {MINGW_URL} {src_dir}")
            .run()?;
        let crt_src_dir = src_dir.join("mingw-w64-crt");
        let dst_dir = sh.current_dir().join("crates").join("target").join("ucrt");

        self.copy_ucrt_defs(sh, &crt_src_dir, &dst_dir)?;
        sh.remove_path(src_dir)?;

        Ok(())
    }

    fn copy_ucrt_defs(&self, sh: &Shell, crt_src_dir: &Path, dst_dir: &Path) -> Result<()> {
        let def_dst_dir = dst_dir.join("defs");
        let include_dir = crt_src_dir.join("def-include");
        let lib_dir = crt_src_dir.join("lib-common");
        sh.create_dir(&def_dst_dir)?;
        for (arch, _dir) in &[("X64", "lib64"), ("ARM64", "libarm64")] {
            let dst_dir = def_dst_dir.join(arch.to_lowercase());
            sh.create_dir(&dst_dir)?;
            for name in UCRT_FILES {
                let src_def_file = lib_dir.join(name).with_extension("def");
                let dst_def_file = dst_dir.join(name).with_extension("def");
                if src_def_file.exists() {
                    let src = sh.read_file(src_def_file)?;
                    let src = self.sanitize_def(&src);
                    sh.write_file(dst_def_file, src)?;
                } else {
                    let src_def_file = src_def_file.with_extension("def.in");
                    if src_def_file.exists() {
                        self.gen_def(
                            sh,
                            &src_def_file,
                            &dst_def_file,
                            &include_dir,
                            &format!("DEF_{arch}"),
                        )?
                    } else {
                        eprintln!(
                            "\x1b[33;1mwarning\x1b[0m: UCRT file {name} was not found at {}",
                            src_def_file.display()
                        );
                    }
                }
            }
        }
        Ok(())
    }

    // fn copy_defs(
    //     &self,
    //     sh: &Shell,
    //     src: &Path,
    //     dst: &Path,
    //     include_dir: &Path,
    //     arch_def: &str,
    // ) -> Result<()> {
    //     for entry in read_dir(src)? {
    //         let entry = entry?;
    //         if !entry.file_type()?.is_file() {
    //             continue;
    //         }
    //         let name = entry.file_name();
    //         let name = if let Some(name) = name.to_str() { name } else { continue };
    //         let mut base = name.trim_end_matches(".def.in");
    //         if base != name {
    //             if base == "vcruntime140_app" {
    //                 base = "vcruntime140";
    //             }
    //             let dst = dst.join(format!("{base}.def"));
    //             self.gen_def(sh, &entry.path(), &dst, include_dir, arch_def)?;
    //         } else if name.ends_with(".def") {
    //             sh.copy_file(entry.path(), dst)?;
    //         }
    //     }
    //     Ok(())
    // }

    fn sanitize_def(&self, src: &str) -> String {
        let mut res = String::new();
        for mut line in src.lines() {
            line = line.trim();
            if line.trim() == "; strnlen replaced by emu" {
                res.push_str("strnlen\n");
                continue;
            }

            if line.contains("==") || line.ends_with(" DATA") || line.ends_with("\tDATA") {
                continue;
            }

            if line.is_empty() || line.starts_with(&[';', '#']) {
                continue;
            }
            res.push_str(line);
            res.push('\n');
        }
        res
    }

    fn gen_def(
        &self,
        sh: &Shell,
        src: &Path,
        dst: &Path,
        include_dir: &Path,
        arch_def: &str,
    ) -> Result<()> {
        // The MinGW-w64 .def.in files use 'F_X86_ANY(DATA)' to hide functions
        // overridden by the MinGW runtime, primarily math functions.
        let src = sh.read_file(src)?.replace(" F_X86_ANY(DATA)", "");
        let def = cmd!(sh, "clang -xc -E -nostdinc -D {arch_def} -I {include_dir} -")
            .stdin(src)
            .read()?;
        let def = self.sanitize_def(&def);
        sh.write_file(dst, def)?;
        Ok(())
    }
}
