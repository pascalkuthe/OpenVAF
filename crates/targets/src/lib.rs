/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

//! This crate contains specifications on how to build native code with OpenVAF for native targets
//!
//! It is heavily inspired by the
//! [librustc_target](https://github.com/rust-lang/rust/tree/master/src/librustc_target) and
//! [mun_target](https://github.com/mun-lang/mun/tree/master/crates/mun_target) crates.
//!

// pub mod abi;
pub mod spec;

/// Returns the target triple of the host machine. This can be used as a default target.
pub fn host_triple() -> &'static str {
    // Get the host triple out of the build environment. This ensures that our
    // idea of the host triple is the same as for the set of libraries we've
    // actually built.  We can't just take LLVM's host triple because they
    // normalize all ix86 architectures to i386.
    //
    // Instead of grabbing the host triple (for the current host), we grab (at
    // compile time) the target triple that this rustc is built with and
    // calling that (at runtime) the host triple.
    (option_env!("CFG_COMPILER_HOST_TRIPLE")).expect("CFG_COMPILER_HOST_TRIPLE")
}
