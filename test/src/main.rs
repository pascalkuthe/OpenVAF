/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use clap::Clap;
use color_eyre::Section;
use openvaf_test::framework::{run_tests, Config};

pub fn main() {
    color_eyre::install().expect("Failed to install error and panic handeling hook");
    match run_tests(&Config::parse()) {
        Ok(true) => std::process::exit(exitcode::OK),
        Ok(false) => std::process::exit(exitcode::SOFTWARE),

        Err(err) => {
            let err = err.section("The OpenVAF test suite crashed! This does not imply a test failure but rather a bug within the testsuite itself!");
            println!("{:?}", err);
            std::process::exit(exitcode::SOFTWARE)
        }
    }
}
