/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::TestSession;
use eyre::Result;
use sourcegen::sourcegen_ast;

test! {
    stage: "Source Control",
    name: SOUREGEN,
    run: run,
    test_cases: ["ast"],
}

fn run(sess: &mut TestSession) -> Result<()> {
    assert_eq!(sess.test_case().unwrap().name, "ast");
    sourcegen_ast();
    Ok(())
}

#[doc(hidden)]
pub(crate) fn __keep_alive() {}
