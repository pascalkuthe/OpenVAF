/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use super::*;
use crate::grammar::expressions::expr;

pub(super) fn call(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    arg_list(p);
    m.complete(p, CALL)
}

pub(super) fn sys_fun_call(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // let m2 = p.start();
    p.bump(SYSFUN);
    // m2.complete(p, FUNCTION_REF);
    if p.at(T!('(')) {
        arg_list(p);
    }
    m.complete(p, CALL)
}

pub(super) fn arg_list(p: &mut Parser) {
    let m = p.start();
    p.eat(T!['(']);
    while !p.at(T![')']) && !p.at(EOF) {
        if expr(p).is_none() {
            break;
        }
        if !p.at(T![')']) && !p.expect(T![,]) {
            break;
        }
    }
    p.eat(T![')']);
    m.complete(p, ARG_LIST);
}
