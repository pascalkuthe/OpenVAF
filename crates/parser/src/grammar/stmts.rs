use super::*;

pub(super) const STMT_TS: TokenSet =
    TokenSet::new(&[IF_KW, WHILE_KW, FOR_KW, CASE_KW, BEGIN_KW, T![;], IDENT, SYSFUN, T![@]]);
pub(super) const STMT_RECOVER: TokenSet = TokenSet::new(&[EOF, ENDMODULE_KW, T![;]]);

pub(super) const STMT_ATTR_RECOVER: TokenSet =
    TokenSet::new(&[IF_KW, WHILE_KW, FOR_KW, CASE_KW, BEGIN_KW, T![;]]).union(STMT_RECOVER);

pub(super) fn stmt_with_attrs(p: &mut Parser) {
    let m = p.start();
    attrs(p, STMT_ATTR_RECOVER);
    stmt(p, m, STMT_TS, STMT_RECOVER)
}
pub(super) fn stmt(p: &mut Parser, m: Marker, expected: TokenSet, recover: TokenSet) {
    match p.current() {
        T![;] => empty_stmt(p, m),
        IF_KW => if_stmt(p, m),
        WHILE_KW => while_stmt(p, m),
        FOR_KW => for_stmt(p, m),
        CASE_KW => case_stmt(p, m),
        BEGIN_KW => block_stmt(p, m),
        T![@] => event_stmt(p, m),
        IDENT | SYSFUN => expr_or_assign_stmt::<true>(p, m),
        _ => {
            m.abandon(p);
            let err = p.unexpected_tokens_msg(expected.iter().collect());
            p.err_recover(err, recover.union(recover));
        }
    }
}

fn empty_stmt(p: &mut Parser, m: Marker) {
    p.bump(T![;]);
    m.complete(p, EMPTY_STMT);
}

fn expr_or_assign_stmt<const SEMICOLON: bool>(p: &mut Parser, m: Marker) {
    let kind = if assign_or_expr(p) { ASSIGN_STMT } else { EXPR_STMT };

    if SEMICOLON {
        p.expect(T![;]);
    }
    m.complete(p, kind);
}

fn assign_or_expr(p: &mut Parser) -> bool {
    let m = p.start();
    expr(p);
    if p.eat_ts(TokenSet::new(&[T![<+], T![=]])) {
        expr(p);
        m.complete(p, ASSIGN);
        true
    } else {
        m.abandon(p);
        false
    }
}

fn event_stmt(p: &mut Parser, m: Marker) {
    p.bump(T![@]);
    p.expect(T!['(']);
    p.expect_ts_r(
        TokenSet::new(&[INITIAL_STEP_KW, FINAL_STEP_KW]),
        TokenSet::new(&[T![')'], T!['(']]),
    );
    if p.eat(T!['(']) {
        while !p.at_ts(TokenSet::new(&[T![')'], T![begin], ENDMODULE_KW])) {
            p.expect(STR_LIT);
            if !p.at(T![')']) {
                p.expect_with(T![,], &[T![')'], T![,]]);
            }
        }
        p.eat(T![')']);
    }
    p.expect(T![')']);
    stmt_with_attrs(p);
    m.complete(p, EVENT_STMT);
}

fn if_stmt(p: &mut Parser, m: Marker) {
    p.bump(IF_KW);
    p.expect(T!['(']);
    expr(p);
    p.expect(T![')']);
    stmt_with_attrs(p);
    if p.eat(ELSE_KW) {
        stmt_with_attrs(p)
    }
    m.complete(p, IF_STMT);
}

fn while_stmt(p: &mut Parser, m: Marker) {
    p.bump(WHILE_KW);
    p.expect(T!['(']);
    expr(p);
    p.expect(T![')']);
    stmt_with_attrs(p);
    m.complete(p, WHILE_STMT);
}

fn for_stmt(p: &mut Parser, m: Marker) {
    p.bump(FOR_KW);

    p.expect(T!['(']);

    // init
    let stmt = p.start();
    attrs(p, STMT_RECOVER.union(TokenSet::new(&[IDENT])));
    expr_or_assign_stmt::<true>(p, stmt);

    // condition
    expr(p);
    p.expect(T![;]);

    // incr
    let stmt = p.start();
    attrs(p, STMT_RECOVER.union(TokenSet::new(&[IDENT])));
    expr_or_assign_stmt::<false>(p, stmt);

    p.expect(T![')']);

    stmt_with_attrs(p);

    m.complete(p, FOR_STMT);
}

const CASE_ITEM_RECOVERY: TokenSet = TokenSet::new(&[EOF, ENDCASE_KW, ENDMODULE_KW]);
fn case_stmt(p: &mut Parser, m: Marker) {
    p.bump(CASE_KW);
    p.expect(T!['(']);
    expr(p);
    p.expect(T![')']);

    while !p.at_ts(CASE_ITEM_RECOVERY) {
        case_item(p)
    }

    p.expect(ENDCASE_KW);

    m.complete(p, CASE_STMT);
}

const CASE_COND_RECOVERY: TokenSet = TokenSet::new(&[ENDCASE_KW, EOF, T![:], ENDMODULE_KW]);

fn case_item(p: &mut Parser) {
    let m = p.start();
    vals_or_default(p);
    stmt_with_attrs(p);
    m.complete(p, CASE);
}

fn vals_or_default(p: &mut Parser) {
    if p.eat(DEFAULT_KW) {
        p.eat(T![:]);
    } else {
        while !p.at_ts(CASE_COND_RECOVERY) {
            expr(p);
            if !p.at(T![:]) {
                p.expect_with(T![,], &[T![:], T![,]]);
            }
        }
        p.expect(T![:]);
    }
}

const BLOCK_RECOVER: TokenSet = TokenSet::new(&[END_KW, EOF, ENDMODULE_KW]);
const BLOCK_STMT_TS: TokenSet = STMT_TS.union(TYPE_TS).union(TokenSet::unique(PARAMETER_KW));
const BLOCK_ATTR_RECOVER: TokenSet =
    STMT_ATTR_RECOVER.union(TYPE_TS).union(TokenSet::unique(PARAMETER_KW));
fn block_stmt(p: &mut Parser, m: Marker) {
    p.bump(BEGIN_KW);
    if p.at(T![:]) {
        let m = p.start();
        p.bump(T![:]);
        name(p);
        m.complete(p, BLOCK_SCOPE);
    }

    while !p.at_ts(BLOCK_RECOVER) {
        let m = p.start();
        attrs(p, BLOCK_RECOVER.union(BLOCK_ATTR_RECOVER));
        if p.at_ts(TYPE_TS) {
            var_decl(p, m);
        } else if p.at(PARAMETER_KW) {
            parameter_decl(p, m);
        } else {
            stmt(p, m, BLOCK_STMT_TS, BLOCK_RECOVER)
        }
    }
    p.expect(END_KW);
    m.complete(p, BLOCK_STMT);
}
