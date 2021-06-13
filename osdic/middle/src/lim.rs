/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LimFunction {
    Native(osdic_target::sim::LimFunction),
    // VerilogA TODO VerilogA Function Type
}

impl Display for LimFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LimFunction::Native(native_limfn_) => Display::fmt(native_limfn_, f),
        }
    }
}
