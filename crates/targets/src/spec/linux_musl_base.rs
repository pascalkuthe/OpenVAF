/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::spec::crt_objects::{self, CrtObjectsFallback};
use crate::spec::TargetOptions;

pub fn opts() -> TargetOptions {
    let mut base = super::linux_base::opts();

    base.env = "musl".to_string();
    base.pre_link_objects_fallback = crt_objects::pre_musl_fallback();
    base.post_link_objects_fallback = crt_objects::post_musl_fallback();
    base.crt_objects_fallback = Some(CrtObjectsFallback::Musl);

    // These targets statically link libc by default
    base.crt_static_default = true;

    base
}
