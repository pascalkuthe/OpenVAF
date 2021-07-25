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
use basedb::BaseDB;
use data_structures::iter::Itertools;
use preprocessor::{
    Preprocess,
    TokenKind::{
        self, Comma, Comment, Div, Modulus, Mul, ParenClose, ParenOpen, Plus, Identifier,
        WhiteSpace,
    },
    // db::SourceDB
};

#[cfg(feature = "cli")]
use pretty_assertions::assert_eq;

pub(super) const SRC: &str = r#"
`define test5(x,y) (x)+(y)
`ifdef test1 ERROR
`endif
`define test2
`ifdef test2 OK1
`endif,
    `ifdef test2 OK2,`define test3 OK3\
OK3L
    `endif
`ifdef test4 ERROR
`else
`define test7(x,y,z) \
/* foo */ \
x*(y%z)\
/* bar */
SMS__
`endif
`ifndef test4
`define test4 OK4
                                            `endif
`test3

,

`ifndef test4
ERROR
`else
`test4
`endif
`test5(Sum1,Sum2)
`define test6(x,y,z) `test5(`test7(x,y,z),f(x/y)*z)
`test6(a,b,c)
"#;

pub fn macros(sess: &mut TestSession) -> Result<()> {
    let db = sess.db();
    let Preprocess { ts, diagnostics, sm: _ } = db.preprocess(db.root_file);
    sess.assert_empty_diagnostics(&diagnostics)?;

    assert_eq!(
        ts.iter().map(|t| t.kind).collect_vec(),
        [
            WhiteSpace,
            Identifier,
            WhiteSpace,
            Comma,
            WhiteSpace,
            Identifier,
            Comma,
            WhiteSpace,
            Identifier,
            WhiteSpace,
            Identifier,
            WhiteSpace,
            Identifier,
            WhiteSpace,
            Comma,
            WhiteSpace,
            Identifier,
            WhiteSpace,
            ParenOpen,
            Identifier,
            ParenClose,
            Plus,
            ParenOpen,
            Identifier,
            ParenClose,
            WhiteSpace,
            ParenOpen,
            WhiteSpace,
            Comment,
            WhiteSpace,
            Identifier,
            Mul,
            ParenOpen,
            Identifier,
            Modulus,
            Identifier,
            ParenClose,
            WhiteSpace,
            Comment,
            ParenClose,
            Plus,
            ParenOpen,
            Identifier,
            ParenOpen,
            Identifier,
            Div,
            Identifier,
            ParenClose,
            Mul,
            Identifier,
            ParenClose,
            WhiteSpace,
        ]
    );
    Ok(())
}
