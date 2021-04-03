//  * ******************************************************************************************
//  *  Copyright (c) 2020 Pascal Kuthe. This file is part of the OSDI project.
//  *  It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/osdi/blob/master/LICENSE.
//  *  No part of OpenVAF-Types, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************


use crate::OsdiModel;
use bincode::ErrorKind;
use once_cell::sync::OnceCell;
use semver::Version;
use std::io::Write;

use std::fs::File;
use std::path::Path;

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

impl OsdiModel {
    pub fn save<W: Write>(&self, mut writer: W) -> Result<(), Box<ErrorKind>> {
        with_semantic_version(|version| bincode::serialize_into(&mut writer, version))?;
        bincode::serialize_into(writer, &self.info_store)
    }

    #[cfg(feature = "simulator")]
    pub fn load(path: impl AsRef<Path>) -> Result<Self, Box<ErrorKind>> {
        use crate::runtime::OsdiModelRuntime;

        let file = File::open(path.as_ref().join(".mis"))?;
        let version: Version = bincode::deserialize_from(&file)?;
        if is_compatible(&version) {
            let info_store = bincode::deserialize_from(&file)?;
            let runtime = OsdiModelRuntime::init(path).unwrap();

            let res = Self {
                info_store,
                runtime,
            };

            Ok(res)
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
