use super::*;
use crate::grammar::stmts::{STMT_RECOVER, STMT_TS};

const MODULE_ITEM_RECOVERY: TokenSet = DIRECTION_TS.union(TokenSet::new(&[
    NET_TYPE,
    ANALOG_KW,
    BRANCH_KW,
    STRING_KW,
    REAL_KW,
    INTEGER_KW,
    PARAMETER_KW,
    ENDMODULE_KW,
]));
pub(super) const MODULE_ITEM_OR_ATTR_RECOVERY: TokenSet =
    MODULE_ITEM_RECOVERY.union(TokenSet::unique(T!["(*"]));

pub(crate) fn module(p: &mut Parser, m: Marker) {
    p.bump(T![module]);
    name_r(p, TokenSet::new(&[T!['('], T![;]]));
    if p.at(T!['(']) {
        let m = p.start();
        p.bump(T!['(']);
        module_ports(p);
        m.complete(p, MODULE_PORTS);
    }
    p.expect(T![;]);
    while !p.at_ts(ITEM_RECOVERY_SET.union(TokenSet::unique(ENDMODULE_KW))) {
        module_item(p);
    }

    p.expect(ENDMODULE_KW);

    m.complete(p, MODULE_DECL);
}

const MODULE_PORTS_RECOVERY: TokenSet = TokenSet::new(&[T![;], T![')'], ENDMODULE_KW, EOF]);

fn module_ports(p: &mut Parser) {
    while !p.at_ts(MODULE_PORTS_RECOVERY) {
        let m = p.start();
        if !eat_name(p) {
            let m = p.start();
            attrs(p, MODULE_PORTS_RECOVERY.union(DIRECTION_TS));
            port_decl::<true>(p, m)
        }
        m.complete(p, MODULE_PORT);
        if !p.at(T![')']) {
            p.expect_with(T![,], &[T![,], T![')']]);
        }
    }
    p.expect(T![')']);
}

const DIRECTION_TS: TokenSet = TokenSet::new(&[T![inout], T![output], T![input]]);
const MODULE_PORT_RECOVERY: TokenSet =
    MODULE_PORTS_RECOVERY.union(DIRECTION_TS).union(TokenSet::unique(T!["(*"]));
const NET_RECOVERY: TokenSet = TokenSet::new(&[EOF, ENDMODULE_KW, T![;]]);

fn port_decl<const MODULE_HEAD: bool>(p: &mut Parser, m: Marker) {
    let direction = p.start();
    p.bump_ts(DIRECTION_TS);
    direction.complete(p, DIRECTION);

    //direction and type are both optional since only one is required
    if !p.nth_at_ts(1, MODULE_PORT_RECOVERY.union(TokenSet::unique(T![,]))) {
        eat_name_ref(p);
    }
    p.eat(NET_TYPE);

    if MODULE_HEAD {
        decl_list(p, T![')'], module_port, MODULE_PORT_RECOVERY);
    } else {
        net_dec_list(p);
    }

    let finished = m.complete(p, PORT_DECL);
    if !MODULE_HEAD {
        let m = finished.precede(p);
        p.eat(T![;]);
        m.complete(p, BODY_PORT_DECL);
    }
}

fn module_port(p: &mut Parser) -> bool {
    name_r(p, MODULE_PORT_RECOVERY.union(TokenSet::unique(T![,])));
    !(p.at(T![,]) && p.nth_at_ts(1, MODULE_PORT_RECOVERY))
}

fn module_item(p: &mut Parser) {
    let m = p.start();
    attrs(p, MODULE_ITEM_RECOVERY);
    match p.current() {
        ANALOG_KW if p.nth(1) == FUNCTION_KW => func_decl(p, m),
        ANALOG_KW => {
            p.bump(ANALOG_KW);
            stmt_with_attrs(p);
            m.complete(p, ANALOG_BEHAVIOUR);
        }
        NET_TYPE => {
            net_decl::<true>(p, m);
        }
        IDENT => {
            net_decl::<false>(p, m);
        }
        PARAMETER_KW => {
            parameter_decl(p, m);
        }
        BRANCH_KW => {
            branch_decl(p, m);
        }
        INTEGER_KW | REAL_KW | STRING_KW => var_decl(p, m),
        INPUT_KW | OUTPUT_KW | INOUT_KW => port_decl::<false>(p, m),
        _ => {
            m.abandon(p);
            let err =
                p.unexpected_tokens_msg(vec![FUNCTION, PORT_DECL, NET_DECL, ANALOG_BEHAVIOUR]);
            p.err_recover(err, MODULE_ITEM_RECOVERY);
        }
    }
}

fn net_decl<const NET_TYPE_FIRST: bool>(p: &mut Parser, m: Marker) {
    //direction and type ar both optional since only one is required
    if NET_TYPE_FIRST {
        p.bump(NET_TYPE);
        if !p.nth_at(1, T![,]) {
            eat_name_ref(p);
        }
    } else {
        name_ref_r(p, MODULE_ITEM_OR_ATTR_RECOVERY.union(TokenSet::unique(T![;])))
    }

    net_dec_list(p);
    p.eat(T![;]);
    m.complete(p, NET_DECL);
}

fn net_dec_list(p: &mut Parser) {
    decl_list(p, T![;], decl_name, NET_RECOVERY);
}

const FUNCTION_RECOVER: TokenSet = TokenSet::new(&[EOF, ENDMODULE_KW, ENDFUNCTION_KW]);
const FUN_ITEM_TS: TokenSet = TokenSet::unique(PARAMETER_KW)
    .union(TYPE_TS)
    .union(STMT_RECOVER)
    .union(DIRECTION_TS)
    .union(STMT_TS);

fn func_decl(p: &mut Parser, m: Marker) {
    p.bump(T![analog]);
    p.bump(T![function]);
    eat_ty(p);
    name_r(p, TokenSet::unique(T![;]));
    p.expect(T![;]);

    while !p.at_ts(FUNCTION_RECOVER) {
        let m = p.start();
        attrs(p, FUN_ITEM_TS.union(FUNCTION_RECOVER));
        if p.at_ts(TYPE_TS) {
            var_decl(p, m)
        } else if p.at(PARAMETER_KW) {
            parameter_decl(p, m)
        } else if p.at_ts(DIRECTION_TS) {
            func_arg(p, m);
        } else {
            stmt(p, m, FUN_ITEM_TS, FUNCTION_RECOVER)
        }
    }
    p.expect(ENDFUNCTION_KW);
    m.complete(p, FUNCTION);
}

const FUNC_ARG_RECOVER: TokenSet = TokenSet::new(&[EOF, ENDMODULE_KW]);
fn func_arg(p: &mut Parser, m: Marker) {
    let direction = p.start();
    p.bump_ts(DIRECTION_TS);
    direction.complete(p, DIRECTION);

    decl_list(p, T![;], decl_name, FUNC_ARG_RECOVER);
    p.eat(T![;]);
    m.complete(p, FUNCTION_ARG);
}

fn branch_decl(p: &mut Parser, m: Marker) {
    p.bump(BRANCH_KW);
    if !p.at(T!['(']) {
        p.error(p.unexpected_token_msg(T!['(']))
    }
    arg_list(p);
    decl_list(p, T![;], decl_name, MODULE_ITEM_OR_ATTR_RECOVERY);
    p.eat(T![;]);
    m.complete(p, BRANCH_DECL);
}
