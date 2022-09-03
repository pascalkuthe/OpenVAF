use core::slice;
use std::str;

use anyhow::{bail, Context, Result};
use camino::{Utf8Path, Utf8PathBuf};
use paths::AbsPathBuf;
use target::spec::Target;

use crate::api::{OptLevel, Opts, Slice};

impl From<OptLevel> for llvm::OptLevel {
    fn from(lvl: OptLevel) -> Self {
        match lvl {
            OptLevel::None => llvm::OptLevel::None,
            OptLevel::Less => llvm::OptLevel::Less,
            OptLevel::Default => llvm::OptLevel::Default,
            OptLevel::Aggressive => llvm::OptLevel::Aggressive,
        }
    }
}

impl Opts {
    pub(crate) fn module_name(&self) -> Result<Option<&str>> {
        if self.model.ptr.is_null() {
            Ok(None)
        } else {
            let raw = unsafe { slice::from_raw_parts(self.model.ptr, self.model.len) };
            Ok(Some(str::from_utf8(raw).context("model name must be valid utf8!")?))
        }
    }

    pub(crate) fn target_cpu(&self) -> Result<Option<&str>> {
        if self.target_cpu.ptr.is_null() {
            Ok(None)
        } else {
            let raw = unsafe { slice::from_raw_parts(self.target_cpu.ptr, self.target_cpu.len) };
            Ok(Some(str::from_utf8(raw).context("target_cpu must be valid utf8!")?))
        }
    }

    pub(crate) fn target(&self) -> Result<Target> {
        let target = if self.target_cpu.ptr.is_null() {
            Target::host_target().unwrap()
        } else {
            let raw = unsafe { slice::from_raw_parts(self.target.ptr, self.target.len) };
            let name = str::from_utf8(raw).context("target must be valid utf8")?;
            match Target::search(name) {
                Some(target) => target,
                None => bail!("specified target not found"),
            }
        };

        Ok(target)
    }

    pub(crate) fn cache_dir(&self) -> Result<Utf8PathBuf> {
        let res = if self.cache_dir.ptr.is_null() {
            let path = directories_next::ProjectDirs::from("com", "semimod", "verilogae")
                .context("failed to find cache directory\nhelp: consider setting it manually")?
                .cache_dir()
                .to_owned();

            if let Ok(path) = Utf8PathBuf::from_path_buf(path) {
                path
            } else {
                bail!("failed to find cache directory\nhelp: consider setting it manually")
            }
        } else {
            unsafe { self.cache_dir.to_path() }
        };
        Ok(res)
    }

    pub(crate) fn cg_flags(&self) -> impl Iterator<Item = &str> {
        Self::str_list_iter(&self.cg_flags)
    }

    pub(crate) fn allow_lints(&self) -> impl Iterator<Item = &str> {
        Self::str_list_iter(&self.allow_lints)
    }

    pub(crate) fn warn_lints(&self) -> impl Iterator<Item = &str> {
        Self::str_list_iter(&self.warn_lints)
    }

    pub(crate) fn deny_lints(&self) -> impl Iterator<Item = &str> {
        Self::str_list_iter(&self.deny_lints)
    }

    pub(crate) fn macro_flags(&self) -> impl Iterator<Item = &str> {
        Self::str_list_iter(&self.macro_flags)
    }

    #[allow(clippy::type_complexity)]
    pub(crate) fn vfs(&self) -> Result<Option<Vec<(&str, &[u8])>>> {
        if self.vfs.ptr.is_null() {
            Ok(None)
        } else {
            let iter: Result<_> = unsafe {
                self.vfs
                    .read()
                    .iter()
                    .map(move |entry| {
                        let path = entry.name.read();
                        let path = str::from_utf8(path).context("vfs-paths must be valid utf8")?;

                        let contents = entry.data.read();
                        Ok((path, contents))
                    })
                    .collect()
            };
            Ok(Some(iter?))
        }
    }

    fn str_list_iter(data: &Slice<Slice<u8>>) -> impl Iterator<Item = &str> {
        let data = unsafe { data.read() };
        data.iter().map(move |slice| {
            let raw = unsafe { slice.read() };
            let res = str::from_utf8(raw).expect("lint names must be valid utf8!");
            res
        })
    }

    pub(crate) fn include_dirs(&self) -> impl Iterator<Item = Result<AbsPathBuf>> + '_ {
        let data = &self.include_dirs;
        let data = unsafe { data.read() };
        data.iter().map(|slice| unsafe { abs_path(&slice.to_path()) })
    }
}

impl Slice<u8> {
    /// # Safety
    ///
    /// `self.ptr` must be valid for `self.len` reads
    pub unsafe fn to_path(&self) -> Utf8PathBuf {
        let path: &str = str::from_utf8(self.read()).expect("all paths must be utf-8");
        Utf8Path::new(path).to_owned()
    }
}

pub(crate) fn abs_path(path: &Utf8Path) -> Result<AbsPathBuf> {
    let path = path.canonicalize().with_context(|| format!("failed to read {}", path))?;
    let path = AbsPathBuf::assert(path);
    Ok(path.normalize())
}
