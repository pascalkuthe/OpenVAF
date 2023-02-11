use anyhow::{bail, Context, Result};
use camino::{Utf8Path, Utf8PathBuf};
use cc::windows_registry;

use std::ffi::{OsStr, OsString};
use std::fs::{remove_file, File};
use std::io::Write;
use std::mem::take;
use std::path::{Path, PathBuf};
use std::process::{Output, Stdio};
use std::{ascii, env, io};
use target::spec::{LinkerFlavor, Target};

pub fn link(
    path: Option<Utf8PathBuf>,
    target: &Target,
    out_filename: &Utf8Path,
    add_objects: impl FnOnce(&mut dyn Linker),
) -> Result<()> {
    let mut linker = linker_with_args(path, target, out_filename, add_objects);

    let import_lib_path = out_filename.with_file_name("__openvaf__import.lib");
    if !target.options.import_lib.is_empty() {
        let mut file = File::create(&import_lib_path).context("failed to create importlib")?;
        file.write_all(target.options.import_lib).context("failed to write importlib")?;
        linker.add_object(&import_lib_path);
    }
    let res = exec_linker(linker.take_cmd(), out_filename);
    if !target.options.import_lib.is_empty() {
        remove_file(import_lib_path).context("failed to delete importlib")?;
    }
    match res {
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

fn escape_stdout_stderr_string(s: &[u8]) -> String {
    std::str::from_utf8(s).map(|s| s.to_owned()).unwrap_or_else(|_| {
        let mut x = "Non-UTF-8 output: ".to_string();
        x.extend(s.iter().flat_map(|&b| ascii::escape_default(b)).map(char::from));
        x
    })
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
fn linker_with_args<'a>(
    path: Option<Utf8PathBuf>,
    target: &'a Target,
    out_filename: &Utf8Path,
    add_objects: impl FnOnce(&mut dyn Linker),
) -> Box<dyn Linker + 'a> {
    let flavor = target.options.linker_flavor;
    let mut cmd = get_linker(path.map(|path| path.into_std_path_buf()), flavor, target);
    disable_localization(cmd.cmd());
    // This environment variable is pretty magical but is intended for
    // producing deterministic builds. This was first discovered to be used
    // by the `ar` tool as a way to control whether or not mtime entries in
    // the archive headers were set to zero or not. It appears that
    // eventually the linker got updated to do the same thing and now reads
    // this environment variable too in recent versions.
    cmd.cmd().env("ZERO_AR_DATE", "1");

    cmd.add_pre_link_args(target, flavor);

    add_objects(&mut *cmd);
    cmd.output_filename(out_filename);
    cmd.set_output_kind();

    cmd.add_post_link_args(target, flavor);

    cmd
}

//// The third parameter is for env vars, used on windows to set up the
//// path for MSVC to find its DLLs, and gcc to find its bundled
//// toolchain
fn get_linker<'a>(
    path: Option<PathBuf>,
    flavor: LinkerFlavor,
    target: &'a Target,
) -> Box<dyn Linker + 'a> {
    match flavor {
        LinkerFlavor::Msvc => {
            let msvc_tool = windows_registry::find_tool(&target.llvm_target, "link.exe");
            let path = match path {
                Some(path) => path,
                None => match msvc_tool {
                    Some(ref tool) => tool.path().to_owned(),
                    None => Path::new("link.exe").to_owned(),
                },
            };
            let mut cmd = Command::new(path);
            let mut new_path = Vec::new();
            // The compiler's sysroot often has some bundled tools, so add it to the
            // PATH for the child.
            let mut msvc_changed_path = false;
            if let Some(ref tool) = msvc_tool {
                cmd.args(tool.args());
                for &(ref k, ref v) in tool.env() {
                    if k == "PATH" {
                        new_path.extend(env::split_paths(v));
                        msvc_changed_path = true;
                    } else {
                        cmd.env(k, v);
                    }
                }
            }

            if !msvc_changed_path {
                if let Some(path) = env::var_os("PATH") {
                    new_path.extend(env::split_paths(&path));
                }
            }
            cmd.env("PATH", env::join_paths(new_path).unwrap());
            Box::new(MsvcLinker { cmd }) as Box<dyn Linker>
        }
        LinkerFlavor::Ld => {
            Box::new(LdLinker { cmd: Command::new(path.unwrap_or_else(|| "ld".into())), target })
                as Box<dyn Linker>
        }
        LinkerFlavor::Ld64 => {
            Box::new(LdLinker { cmd: Command::new(path.unwrap_or_else(|| "ld".into())), target })
                as Box<dyn Linker>
        }
    }
}

fn exec_linker(mut cmd: std::process::Command, _out_filename: &Utf8Path) -> io::Result<Output> {
    match cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).spawn() {
        #[allow(clippy::let_and_return)]
        Ok(child) => {
            let output = child.wait_with_output();
            // On Windows, under high I/O load, output buffers are sometimes not flushed,
            // even long after process exit, causing nasty, non-reproducible output bugs.
            //
            // File::sync_all() calls FlushFileBuffers() down the line, which solves the problem.
            //
            // А full writeup of the original Chrome bug can be found at
            // randomascii.wordpress.com/2018/02/25/compiler-bug-linker-bug-windows-kernel-bug/amp

            #[cfg(windows)]
            if let Ok(of) = std::fs::OpenOptions::new().write(true).open(_out_filename) {
                of.sync_all()?;
            }

            output
        }
        // Err(ref e) if command_line_too_big(e) => {},
        Err(e) => Err(e),
    }
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
    fn output_filename(&mut self, path: &Utf8Path);
    fn add_object(&mut self, path: &Utf8Path);
    fn set_output_kind(&mut self);
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
        if let Ok(flags) = std::env::var("OPENVAF_LDFLAGS") {
            let flags = flags
                .split(' ')
                .filter(|flag| flag.is_empty() && !flag.chars().all(|c| c.is_whitespace()));
            self.args(flags)
        }
    }

    pub fn take_cmd(&mut self) -> std::process::Command {
        let cmd = self.cmd();
        let mut res = std::process::Command::new(cmd.command.as_os_str());
        res.args(cmd.args.iter()).envs(take(&mut cmd.env));
        res
    }
}

pub struct LdLinker<'a> {
    cmd: Command,
    target: &'a Target,
}

impl<'a> LdLinker<'a> {
    /// Passes an argument directly to the linker.
    ///
    /// When the linker is not ld-like such as when using a compiler as a linker, the argument is
    /// prepended by `-Wl,`.
    fn linker_arg(&mut self, arg: &str) -> &mut Self {
        self.cmd.arg(arg);
        self
    }

    fn build_dylib(&mut self) {
        // On mac we need to tell the linker to let this library be rpathed
        if self.target.options.is_like_osx {
            self.linker_arg("-dylib");
        } else {
            self.linker_arg("-shared");
        }
    }
}

impl<'a> Linker for LdLinker<'a> {
    fn cmd(&mut self) -> &mut Command {
        &mut self.cmd
    }

    fn output_filename(&mut self, path: &Utf8Path) {
        self.cmd.arg("-o").arg(path.as_str());
    }

    fn add_object(&mut self, path: &Utf8Path) {
        self.cmd.arg(path.as_str());
    }

    fn set_output_kind(&mut self) {
        self.build_dylib();
    }
}

pub struct MsvcLinker {
    cmd: Command,
}

impl Linker for MsvcLinker {
    fn cmd(&mut self) -> &mut Command {
        // let mut arg: OsString = "/IMPLIB:".into();
        // arg.push(out_filename.with_extension("dll.lib"));
        // self.cmd.arg(arg);
        &mut self.cmd
    }

    fn output_filename(&mut self, path: &Utf8Path) {
        let arg = format!("/OUT:{path}");
        self.cmd.arg(&arg);
    }

    fn add_object(&mut self, path: &Utf8Path) {
        self.cmd.arg(path.as_str());
    }

    fn set_output_kind(&mut self) {
        self.cmd.arg("/DLL");
    }
}

pub struct Command {
    command: PathBuf,
    args: Vec<OsString>,
    env: Vec<(OsString, OsString)>,
}

impl Command {
    fn new(command: PathBuf) -> Command {
        Command { command, args: Vec::new(), env: Vec::new() }
    }
    fn args<I: AsRef<OsStr>>(&mut self, args: impl IntoIterator<Item = I>) {
        self.args.extend(args.into_iter().map(|arg| arg.as_ref().to_owned()))
    }

    fn arg(&mut self, arg: impl AsRef<OsStr>) -> &mut Self {
        self.args.push(arg.as_ref().to_owned());
        self
    }

    fn env(&mut self, env: impl AsRef<OsStr>, val: impl AsRef<OsStr>) -> &mut Self {
        self.env.push((env.as_ref().to_owned(), val.as_ref().to_owned()));
        self
    }

    // fn run(self) -> Result<()> {
    //     if res.retcode != 0 {
    //         let flavor = format!("{:?}", self.flavor).to_lowercase();
    //         eprintln!("lld-{} {}", flavor, self.args.join(" "));
    //         eprintln!("{}", res.messages);
    //         bail!("linking failed (see linker output for details)")
    //     }
    //     Ok(())
    // }
}
