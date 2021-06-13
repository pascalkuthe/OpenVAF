/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::ModelInfoStore;
use bincode::ErrorKind;
use once_cell::sync::OnceCell;
use semver::Version;
use std::io::Write;

static VERSION: OnceCell<Version> = OnceCell::new();

pub fn with_semantic_version<T>(f: impl FnOnce(&Version) -> T) -> T {
    let version = VERSION.get_or_init(|| {
        Version::parse(env!("CARGO_PKG_VERSION"))
            .expect("Crate VERSION does not follow semantic versioning!")
    });
    f(version)
}

pub fn is_compatible(serialized_version: &Version) -> bool {
    with_semantic_version(|version| {
        serialized_version > version
            && (serialized_version.major != version.major || serialized_version.major == 0)
    })
}

impl ModelInfoStore {
    pub fn to_bytes<W: Write>(&self) -> Result<Vec<u8>, Box<ErrorKind>> {
        let mut res = Vec::new();
        with_semantic_version(|version| bincode::serialize_into(&mut res, version))?;
        bincode::serialize_into(&mut res, &self)?;
        Ok(res)
    }

    #[cfg(feature = "simulator")]
    pub fn load(mut bytes: &[u8]) -> Result<Self, Box<ErrorKind>> {
        let version: Version = bincode::deserialize_from(&mut bytes)?;
        if is_compatible(&version) {
            bincode::deserialize_from(&mut bytes)
        } else {
            let err = with_semantic_version(|osdi_version| {
                Box::new(ErrorKind::Custom(format!(
                    "Model OSDI Version {} is incompatible with the Simulator OSDI Version {} ",
                    version, osdi_version
                )))
            });
            Err(err)
        }
    }
}
