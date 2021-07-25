/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::Config;
#[cfg(feature = "cli")]
use color_eyre::Section;
use eyre::{Result, WrapErr};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::warn;

use super::TestSession;

impl Model {
    pub fn new(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let name = path.file_name().expect("Not a directory");
        let name =
            Box::leak(name.to_string_lossy().to_string().to_ascii_lowercase().into_boxed_str());
        let mut mainfile = path.join(&name);
        mainfile.set_extension("va");
        #[cfg(feature = "cli")]
        mainfile
            .canonicalize()
            .wrap_err_with(|| format!("{}: Failed to find model root file", path.display()))
            .with_note(|| {
                format!(
                    "The OpenVAF test suite expects every model to contain\
                   a root file named the same as the directory.\nIn this case {}.va",
                    name
                )
            })?;

        #[cfg(not(feature = "cli"))]
        mainfile
            .canonicalize()
            .wrap_err_with(|| format!("{}: Failed to find model root file", path.display()))?;
        Ok(Self(name, path.to_owned()))
    }

    pub fn src_directory(&self) -> &Path {
        &self.1
    }

    pub fn mainfile(&self) -> PathBuf {
        self.1.join(format!("{}.va", self.0))
    }

    pub fn read_files_to_vfs(&self, dst: &mut TestSession) -> Result<()> {
        for entry in fs::read_dir(&self.1)? {
            let entry = entry?;
            if entry.file_type()?.is_file() {
                dst.add_file(
                    &entry
                        .file_name()
                        .to_str()
                        .expect("File names of integration tests must be representable with UTF-8"),
                    &fs::read_to_string(entry.path())?,
                );
            } else {
                warn!("Test Suite currently doesn't read nested directory and symlinks for integration tests!")
            }
        }

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Model(pub &'static str, PathBuf);

pub fn find_models(config: &Config) -> Result<&'static [Model]> {
    let base_dir = &config.src_dirs;
    let models: Result<Vec<_>> = fs::read_dir(base_dir)?
        .filter_map(|x| match x {
            Err(err) => Some(Err(err.into())),
            Ok(x) => {
                let path = x.path();
                if path.is_dir() {
                    Some(Model::new(path))
                } else {
                    None
                }
            }
        })
        .collect();

    Ok(Box::leak(models?.into_boxed_slice()))
}
