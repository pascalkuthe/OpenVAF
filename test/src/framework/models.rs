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
use color_eyre::Section;
use eyre::{Result, WrapErr};
use std::fs;
use std::path::{Path, PathBuf};

impl Model {
    pub fn new(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let name = path.file_name().expect("Not a directory");
        let name_str = Box::leak(name.to_string_lossy().to_string().into_boxed_str());
        let res = Self(name_str);
        let mut path = path.join(name_str.to_ascii_lowercase());
        path.set_extension("va");
        path.canonicalize().wrap_err_with(||format!("{}: Failed to find model root file",path.display())).with_note(||format!("The OpenVAF test suite expects every model to contain a root file named the same as the directory.\nIn this case {}.va",res.0))?;
        Ok(res)
    }

    pub fn src_directory(&self, config: &Config) -> PathBuf {
        config.src_dirs.join("models").join(self.0)
    }

    pub fn mainfile_absolute(&self, config: &Config) -> PathBuf {
        self.src_directory(config)
            .join(format!("{}.va", self.0.to_ascii_lowercase()))
    }

    pub fn mainfile(&self) -> PathBuf {
        Path::new("models")
            .join(self.0)
            .join(format!("{}.va", self.0.to_ascii_lowercase()))
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Model(pub &'static str);

pub fn find_models(config: &Config) -> Result<Box<[Model]>> {
    let base_dir = config.src_dirs.join("models");
    let models: Result<Vec<_>> = fs::read_dir(base_dir)?
        .filter_map(|x| match x {
            Err(err) => Some(Err(err.into())),
            Ok(x) => {
                let path = x.path();
                if path.is_dir() {
                    Some(Model::new(path).into())
                } else {
                    None
                }
            }
        })
        .collect();

    Ok(models?.into_boxed_slice())
}
