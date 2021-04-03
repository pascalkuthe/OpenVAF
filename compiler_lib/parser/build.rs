/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
        .log_verbose()
        .use_cargo_dir_conventions()
        .emit_rerun_directives(true)
        .process()
        .unwrap();
}
