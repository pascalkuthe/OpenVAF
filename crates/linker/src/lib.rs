use anyhow::{bail, Result};
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::{ascii, io, mem};
use target::spec::{LinkerFlavor, Target};

pub fn link(
    target: &Target,
    linker: Option<PathBuf>,
    flavor: Option<LinkerFlavor>,
    out_filename: &Path,
    add_objects: impl FnOnce(&mut dyn Linker),
) -> Result<()> {
    let (linker, flavor) = linker_and_flavor(target, linker, flavor);
    let mut cmd = linker_with_args(&linker, flavor, target, out_filename, add_objects);

    disable_localization(&mut cmd);

    for &(ref k, ref v) in &target.options.link_env {
        cmd.env(k, v);
    }
    for k in &target.options.link_env_remove {
        cmd.env_remove(k);
    }

    match exec_linker(cmd, out_filename) {
        Ok(prog) if !prog.status.success() => {
            let mut output = prog.stderr.clone();
            output.extend_from_slice(&prog.stdout);
            let escaped_output = escape_stdout_stderr_string(&output);
            eprintln!("{}", escaped_output);
            bail!("linking failed (see linker output for details)")
        }
        Ok(_) => Ok(()),
        Err(err) => bail!("linker not found: {}", err),
    }
}

/// Disables non-English messages from localized linkers.
/// Such messages may cause issues with text encoding on Windows (#35785)
/// and prevent inspection of linker output in case of errors, which we occasionally do.
/// This should be acceptable because other messages from rustc are in English anyway,
/// and may also be desirable to improve searchability of the linker diagnostics.
fn disable_localization(linker: &mut Command) {
    // No harm in setting both env vars simultaneously.
    // Unix-style linkers.
    linker.env("LC_ALL", "C");
    // MSVC's `link.exe`.
    linker.env("VSLANG", "1033");
}

/// Produce the linker command line containing linker path and arguments.
///
/// When comments in the function say "order-(in)dependent" they mean order-dependence between
/// options and libraries/object files. For example `--whole-archive` (order-dependent) applies
/// to specific libraries passed after it, and `-o` (output file, order-independent) applies
/// to the linking process as a whole.
/// Order-independent options may still override each other in order-dependent fashion,
/// e.g `--foo=yes --foo=no` may be equivalent to `--foo=no`.
fn linker_with_args(
    path: &Path,
    flavor: LinkerFlavor,
    target: &Target,
    out_filename: &Path,
    add_objects: impl FnOnce(&mut dyn Linker),
) -> Command {
    let cmd = &mut get_linker(path, flavor, target);

    // ------------ Early order-dependent options ------------

    // TODO required?
    // // If we're building something like a dynamic library then some platforms
    // // need to make sure that all symbols are exported correctly from the
    // // dynamic library.
    // // Must be passed before any libraries to prevent the symbols to export from being thrown away,
    // // at least on some platforms (e.g. windows-gnu).
    // cmd.export_symbols(
    //     tmpdir,
    //     crate_type,
    //     &codegen_results.crate_info.exported_symbols[&crate_type],
    // );

    // Can be used for adding custom CRT objects or overriding order-dependent options above.
    // FIXME: In practice built-in target specs use this for arbitrary order-independent options,
    // introduce a target spec option for order-independent linker options and migrate built-in
    // specs to it.
    cmd.add_pre_link_args(target, flavor);

    add_objects(&mut **cmd);
    cmd.output_filename(out_filename);

    // FIXME: Built-in target specs occasionally use this for linking system libraries,
    // eliminate all such uses by migrating them to `#[link]` attributes in `lib(std,c,unwind)`
    // and remove the option.
    cmd.add_post_link_args(target, flavor);

    cmd.take_cmd()
}

fn escape_stdout_stderr_string(s: &[u8]) -> String {
    std::str::from_utf8(s).map(|s| s.to_owned()).unwrap_or_else(|_| {
        let mut x = "Non-UTF-8 output: ".to_string();
        x.extend(s.iter().flat_map(|&b| ascii::escape_default(b)).map(char::from));
        x
    })
}

//// The third parameter is for env vars, used on windows to set up the
//// path for MSVC to find its DLLs, and gcc to find its bundled
//// toolchain
fn get_linker<'a>(linker: &Path, flavor: LinkerFlavor, target: &'a Target) -> Box<dyn Linker + 'a> {
    let cmd = Command::new(linker);

    // TODO neccessarry?
    // let msvc_tool = windows_registry::find_tool(&target.llvm_target, "link.exe");
    // let mut new_path = sess.get_tools_search_paths(self_contained);
    // // The compiler's sysroot often has some bundled tools, so add it to the
    // // PATH for the child.
    // let mut msvc_changed_path = false;
    // if target.options.is_like_msvc {
    //     if let Some(ref tool) = msvc_tool {
    //         // cmd.args(tool.args());
    //         for &(ref k, ref v) in tool.env() {
    //             if k == "PATH" {
    //                 new_path.extend(env::split_paths(v));
    //                 msvc_changed_path = true;
    //             } else {
    //                 cmd.env(k, v);
    //             }
    //         }
    //     }
    // }

    // if !msvc_changed_path {
    //     if let Some(path) = env::var_os("PATH") {
    //         new_path.extend(env::split_paths(&path));
    //     }
    // }
    // cmd.env("PATH", env::join_paths(new_path).unwrap())

    match flavor {
        LinkerFlavor::Msvc => Box::new(MsvcLinker { cmd }) as Box<dyn Linker>,
        LinkerFlavor::Gcc => Box::new(GccLinker { cmd, target, is_ld: false }) as Box<dyn Linker>,
        LinkerFlavor::Ld => Box::new(GccLinker { cmd, is_ld: true, target }) as Box<dyn Linker>,
    }
}

fn exec_linker(
    mut cmd: Command,
    // target: &Target,
    out_filename: &Path,
    // tmpdir: &Path,
) -> io::Result<Output> {
    // TODO required?
    // When attempting to spawn the linker we run a risk of blowing out the
    // size limits for spawning a new process with respect to the arguments
    // we pass on the command line.
    //
    // Here we attempt to handle errors from the OS saying "your list of
    // arguments is too big" by reinvoking the linker again with an `@`-file
    // that contains all the arguments. The theory is that this is then
    // accepted on all linkers and the linker will read all its options out of
    // there instead of looking at the command line.
    // if !cmd.very_likely_to_exceed_some_spawn_limit() {
    match cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).spawn() {
        Ok(child) => {
            let output = child.wait_with_output();
            flush_linked_file(&output, out_filename)?;
            return output;
        }
        // Err(ref e) if command_line_too_big(e) => {},
        Err(e) => return Err(e),
    }
    // }

    //let mut cmd2 = cmd.clone();
    //let mut args = String::new();
    //for arg in cmd2.take_args() {
    //    args.push_str(
    //        &Escape { arg: arg.to_str().unwrap(), is_like_msvc: target.is_like_msvc }
    //            .to_string(),
    //    );
    //    args.push('\n');
    //}
    //let file = tmpdir.join("linker-arguments");
    //let bytes = if target.is_like_msvc {
    //    let mut out = Vec::with_capacity((1 + args.len()) * 2);
    //    // start the stream with a UTF-16 BOM
    //    for c in std::iter::once(0xFEFF).chain(args.encode_utf16()) {
    //        // encode in little endian
    //        out.push(c as u8);
    //        out.push((c >> 8) as u8);
    //    }
    //    out
    //} else {
    //    args.into_bytes()
    //};
    //fs::write(&file, &bytes)?;
    //cmd2.arg(format!("@{}", file.display()));
    //let output = cmd2.output();
    //flush_linked_file(&output, out_filename)?;
    //return output;

    #[cfg(not(windows))]
    fn flush_linked_file(_: &io::Result<Output>, _: &Path) -> io::Result<()> {
        Ok(())
    }

    #[cfg(windows)]
    fn flush_linked_file(
        command_output: &io::Result<Output>,
        out_filename: &Path,
    ) -> io::Result<()> {
        // On Windows, under high I/O load, output buffers are sometimes not flushed,
        // even long after process exit, causing nasty, non-reproducible output bugs.
        //
        // File::sync_all() calls FlushFileBuffers() down the line, which solves the problem.
        //
        // А full writeup of the original Chrome bug can be found at
        // randomascii.wordpress.com/2018/02/25/compiler-bug-linker-bug-windows-kernel-bug/amp

        if let &Ok(ref out) = command_output {
            if out.status.success() {
                if let Ok(of) = std::fs::OpenOptions::new().write(true).open(out_filename) {
                    of.sync_all()?;
                }
            }
        }

        Ok(())
    }

    //#[cfg(unix)]
    //fn command_line_too_big(err: &io::Error) -> bool {
    //    err.raw_os_error() == Some(::libc::E2BIG)
    //}

    //#[cfg(windows)]
    //fn command_line_too_big(err: &io::Error) -> bool {
    //    const ERROR_FILENAME_EXCED_RANGE: i32 = 206;
    //    err.raw_os_error() == Some(ERROR_FILENAME_EXCED_RANGE)
    //}

    //#[cfg(not(any(unix, windows)))]
    //fn command_line_too_big(_: &io::Error) -> bool {
    //    false
    //}

    //struct Escape<'a> {
    //    arg: &'a str,
    //    is_like_msvc: bool,
    //}

    //impl<'a> fmt::Display for Escape<'a> {
    //    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //        if self.is_like_msvc {
    //            // This is "documented" at
    //            // https://docs.microsoft.com/en-us/cpp/build/reference/at-specify-a-linker-response-file
    //            //
    //            // Unfortunately there's not a great specification of the
    //            // syntax I could find online (at least) but some local
    //            // testing showed that this seemed sufficient-ish to catch
    //            // at least a few edge cases.
    //            write!(f, "\"")?;
    //            for c in self.arg.chars() {
    //                match c {
    //                    '"' => write!(f, "\\{}", c)?,
    //                    c => write!(f, "{}", c)?,
    //                }
    //            }
    //            write!(f, "\"")?;
    //        } else {
    //            // This is documented at https://linux.die.net/man/1/ld, namely:
    //            //
    //            // > Options in file are separated by whitespace. A whitespace
    //            // > character may be included in an option by surrounding the
    //            // > entire option in either single or double quotes. Any
    //            // > character (including a backslash) may be included by
    //            // > prefixing the character to be included with a backslash.
    //            //
    //            // We put an argument on each line, so all we need to do is
    //            // ensure the line is interpreted as one whole argument.
    //            for c in self.arg.chars() {
    //                match c {
    //                    '\\' | ' ' => write!(f, "\\{}", c)?,
    //                    c => write!(f, "{}", c)?,
    //                }
    //            }
    //        }
    //        Ok(())
    //    }
    //}
}

// This functions tries to determine the appropriate linker (and corresponding LinkerFlavor) to use
fn linker_and_flavor(
    target: &Target,
    linker: Option<PathBuf>,
    flavor: Option<LinkerFlavor>,
) -> (PathBuf, LinkerFlavor) {
    fn infer_from(
        linker: Option<PathBuf>,
        flavor: Option<LinkerFlavor>,
    ) -> Option<(PathBuf, LinkerFlavor)> {
        match (linker, flavor) {
            (Some(linker), Some(flavor)) => Some((linker, flavor)),
            // only the linker flavor is known; use the default linker for the selected flavor
            (None, Some(flavor)) => Some((
                PathBuf::from(match flavor {
                    LinkerFlavor::Gcc => "cc",
                    LinkerFlavor::Ld => "ld",
                    LinkerFlavor::Msvc => "link.exe",
                }),
                flavor,
            )),
            (Some(linker), None) => {
                let stem = linker.file_stem().and_then(|stem| stem.to_str()).unwrap_or_else(|| {
                    unreachable!("couldn't extract file stem from specified linker")
                });

                let flavor = if stem == "gcc"
                    || stem.ends_with("-gcc")
                    || stem == "clang"
                    || stem.ends_with("-clang")
                {
                    LinkerFlavor::Gcc
                } else if stem == "ld" || stem.ends_with("-ld") {
                    LinkerFlavor::Ld
                } else if stem == "link" || stem == "lld-link" {
                    LinkerFlavor::Msvc
                } else {
                    return None;
                };

                Some((linker, flavor))
            }
            (None, None) => None,
        }
    }

    // linker and linker flavor specified via command line have precedence over what the target
    // specification specifies
    if let Some(ret) = infer_from(linker, flavor) {
        return ret;
    }

    if let Some(ret) = infer_from(
        target.options.linker.clone().map(PathBuf::from),
        Some(target.options.linker_flavor),
    ) {
        return ret;
    }

    unreachable!("Not enough information provided to determine how to invoke the linker");
}

/// Linker abstraction used by `link` to build up the command to invoke a
/// linker.
///
/// This trait is the total list of requirements needed by `back::link` and
/// represents the meaning of each option being passed down. This trait is then
/// used to dispatch on whether a GNU-like linker (generally `ld.exe`) or an
/// MSVC linker (e.g., `link.exe`) is being used.
pub trait Linker {
    fn cmd(&mut self) -> &mut Command;
    fn output_filename(&mut self, path: &Path);
    fn add_object(&mut self, path: &Path);
}

impl dyn Linker + '_ {
    // pub fn arg(&mut self, arg: impl AsRef<OsStr>) {
    //     self.cmd().arg(arg);
    // }

    pub fn args<I: AsRef<OsStr>>(&mut self, args: impl IntoIterator<Item = I>) {
        self.cmd().args(args);
    }

    /// Add arbitrary "pre-link" args defined by the target spec or from command line.
    pub fn add_pre_link_args(&mut self, target: &Target, flavor: LinkerFlavor) {
        if let Some(args) = target.options.pre_link_args.get(&flavor) {
            self.args(args);
        }
    }

    pub fn add_post_link_args(&mut self, target: &Target, flavor: LinkerFlavor) {
        if let Some(args) = target.options.post_link_args.get(&flavor) {
            self.args(args);
        }
    }

    pub fn take_cmd(&mut self) -> Command {
        mem::replace(self.cmd(), Command::new(""))
    }
}

pub struct GccLinker<'a> {
    cmd: Command,
    target: &'a Target,
    // Link as ld
    is_ld: bool,
}

impl<'a> GccLinker<'a> {
    /// Passes an argument directly to the linker.
    ///
    /// When the linker is not ld-like such as when using a compiler as a linker, the argument is
    /// prepended by `-Wl,`.
    fn linker_arg(&mut self, arg: impl AsRef<OsStr>) -> &mut Self {
        self.linker_args(&[arg]);
        self
    }

    /// Passes a series of arguments directly to the linker.
    ///
    /// When the linker is ld-like, the arguments are simply appended to the command. When the
    /// linker is not ld-like such as when using a compiler as a linker, the arguments are joined by
    /// commas to form an argument that is then prepended with `-Wl`. In this situation, only a
    /// single argument is appended to the command to ensure that the order of the arguments is
    /// preserved by the compiler.
    fn linker_args(&mut self, args: &[impl AsRef<OsStr>]) -> &mut Self {
        if self.is_ld {
            args.iter().for_each(|a| {
                self.cmd.arg(a);
            });
        } else if !args.is_empty() {
            let mut s = OsString::from("-Wl");
            for a in args {
                s.push(",");
                s.push(a);
            }
            self.cmd.arg(s);
        }

        self
    }

    fn build_dylib(&mut self) {
        // On mac we need to tell the linker to let this library be rpathed
        if self.target.options.is_like_osx {
            if !self.is_ld {
                self.cmd.arg("-dynamiclib");
            }

            self.linker_arg("-dylib");
        } else {
            self.cmd.arg("-shared");
        }
    }
}

impl<'a> Linker for GccLinker<'a> {
    fn cmd(&mut self) -> &mut Command {
        self.build_dylib();
        &mut self.cmd
    }

    fn output_filename(&mut self, path: &Path) {
        self.cmd.arg("-o").arg(path);
    }
    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }
}

pub struct MsvcLinker {
    cmd: Command,
}

impl Linker for MsvcLinker {
    fn cmd(&mut self) -> &mut Command {
        self.cmd.arg("/DLL");
        // let mut arg: OsString = "/IMPLIB:".into();
        // arg.push(out_filename.with_extension("dll.lib"));
        // self.cmd.arg(arg);
        &mut self.cmd
    }

    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn output_filename(&mut self, path: &Path) {
        let mut arg = OsString::from("/OUT:");
        arg.push(path);
        self.cmd.arg(&arg);
    }
}
