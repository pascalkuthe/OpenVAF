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
use crate::parsing_tests::parsing_tests_src;
use color_eyre::Result;
use openvaf_parser::Token::{Comma, Ident, OpDiv, OpMul, Plus};
use openvaf_session::symbols::Symbol;

pub fn macros(sess: &TestSession) -> Result<()> {
    let ts = sess.run_preprocessor(parsing_tests_src("macros"))?;
    let ts: Vec<_> = ts.into_iter().map(|(token, _)| token).collect(); // remove spans we dont check these

    assert_eq!(
        ts,
        [
            Ident(Symbol::intern("OK1")),
            Comma,
            Ident(Symbol::intern("OK2")),
            Comma,
            Ident(Symbol::intern("SMS__")),
            Ident(Symbol::intern("OK3")),
            Ident(Symbol::intern("OK3L")),
            Comma,
            Ident(Symbol::intern("OK4")),
            Ident(Symbol::intern("Sum1")),
            Plus,
            Ident(Symbol::intern("Sum2")),
            Ident(Symbol::intern("Fac1")),
            OpMul,
            Ident(Symbol::intern("Fac2")),
            Plus,
            Ident(Symbol::intern("Fac1")),
            OpDiv,
            Ident(Symbol::intern("Fac2")),
        ]
    );
    Ok(())
}
