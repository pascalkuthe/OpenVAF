/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::spec::TargetOptions;
use std::env;

pub fn opts() -> TargetOptions {
    TargetOptions::default()
}

fn macos_deployment_target() -> (u32, u32) {
    let deployment_target = env::var("MACOSX_DEPLOYMENT_TARGET").ok();
    let version = deployment_target
        .as_ref()
        .and_then(|s| {
            let mut i = s.splitn(2, '.');
            i.next().and_then(|a| i.next().map(|b| (a, b)))
        })
        .and_then(|(a, b)| a.parse::<u32>().and_then(|a| b.parse::<u32>().map(|b| (a, b))).ok());

    version.unwrap_or((10, 7))
}

pub fn macos_llvm_target(arch: &str) -> String {
    let (major, minor) = macos_deployment_target();
    format!("{}-apple-macosx{}.{}.0", arch, major, minor)
}
