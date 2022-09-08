use anyhow::{bail, Context, Result};
use camino::Utf8Path;
use llvm::lld;

use std::fs::File;
use std::io::Write;
use std::mem;
use target::spec::{LinkerFlavor, Target};

pub fn link(
    target: &Target,
    out_filename: &Utf8Path,
    add_objects: impl FnOnce(&mut dyn Linker),
) -> Result<()> {
    let mut cmd = linker_with_args(target, out_filename, add_objects);

    let import_lib_path = out_filename.with_file_name("__openvaf__import.lib");
    if !target.options.import_lib.is_empty() {
        let mut file = File::create(&import_lib_path).context("failed to create importlib")?;
        file.write_all(target.options.import_lib).context("failed to write importlib")?;
        cmd.add_object(&import_lib_path);
    }
    let res = exec_linker(cmd.take_cmd(), out_filename);
    if !target.options.import_lib.is_empty() {
        std::fs::remove_file(import_lib_path).context("failed to delete importlib")?;
    }
    res
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
    target: &'a Target,
    out_filename: &Utf8Path,
    add_objects: impl FnOnce(&mut dyn Linker),
) -> Box<dyn Linker + 'a> {
    let flavor = target.options.linker_flavor;
    let mut cmd = get_linker(flavor, target);

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
fn get_linker<'a>(flavor: LinkerFlavor, target: &'a Target) -> Box<dyn Linker + 'a> {
    match flavor {
        LinkerFlavor::Msvc => {
            Box::new(MsvcLinker { cmd: Command::new(lld::Flavor::Link) }) as Box<dyn Linker>
        }
        LinkerFlavor::Ld => {
            Box::new(LdLinker { cmd: Command::new(lld::Flavor::Ld), target }) as Box<dyn Linker>
        }
        LinkerFlavor::Ld64 => {
            Box::new(LdLinker { cmd: Command::new(lld::Flavor::Ld64), target }) as Box<dyn Linker>
        }
    }
}

fn exec_linker(cmd: Command, _out_filename: &Utf8Path) -> Result<()> {
    cmd.run()?;

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

    Ok(())
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

    pub fn args<I: AsRef<str>>(&mut self, args: impl IntoIterator<Item = I>) {
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

    pub fn take_cmd(&mut self) -> Command {
        let cmd = self.cmd();

        let flavor = cmd.flavor;
        mem::replace(cmd, Command::new(flavor))
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
    flavor: lld::Flavor,
    args: Vec<String>,
}

impl Command {
    fn new(flavor: lld::Flavor) -> Command {
        Command { flavor, args: Vec::new() }
    }
    fn args<I: AsRef<str>>(&mut self, args: impl IntoIterator<Item = I>) {
        self.args.extend(args.into_iter().map(|arg| arg.as_ref().to_string()))
    }

    fn arg(&mut self, arg: &str) -> &mut Self {
        self.args.push(arg.to_owned());
        self
    }

    fn run(self) -> Result<()> {
        let res = lld::link(self.flavor, &self.args);
        if res.retcode != 0 {
            let flavor = format!("{:?}", self.flavor).to_lowercase();
            eprintln!("lld-{} {}", flavor, self.args.join(" "));
            eprintln!("{}", res.messages);
            bail!("linking failed (see linker output for details)")
        }
        Ok(())
    }
}
