use crate::grammar::paths::path;

use super::*;
mod module;
use module::{module, MODULE_ITEM_OR_ATTR_RECOVERY};

const ITEM_RECOVERY_SET: TokenSet = TokenSet::new(&[DISCIPLINE_KW, NATURE_KW, MODULE_KW, EOF]);
const ITEM_OR_ATTR_RECOVERY_SET: TokenSet = ITEM_RECOVERY_SET.union(TokenSet::new(&[T!["(*"]]));

pub(super) fn root_item(p: &mut Parser) {
    let m = p.start();
    attrs(p, ITEM_RECOVERY_SET);
    match p.current() {
        DISCIPLINE_KW => discipline(p, m),
        NATURE_KW => nature(p, m),
        MODULE_KW => module(p, m),
        _ => {
            m.abandon(p);
            let err = p.unexpected_tokens_msg(vec![DISCIPLINE_KW, NATURE_KW, MODULE_KW]);
            p.err_recover(err, ITEM_OR_ATTR_RECOVERY_SET);
        }
    }
}

const DISCIPLINE_RECOVERY_SET: TokenSet =
    ITEM_RECOVERY_SET.union(TokenSet::unique(ENDDISCIPLINE_KW));

fn discipline(p: &mut Parser, m: Marker) {
    p.bump(T![discipline]);
    name_r(p, TokenSet::new(&[T![;]]));
    p.expect(T![;]);
    while !p.at_ts(DISCIPLINE_RECOVERY_SET) {
        let m = p.start();
        path(p);
        p.eat(T![=]);
        expr(p);
        if !p.eat(T![;]) {
            let err = p.unexpected_token_msg(T![;]);
            p.err_recover(err, DISCIPLINE_RECOVERY_SET.union(TokenSet::unique(IDENT)));
        }
        m.complete(p, DISCIPLINE_ATTR);
    }
    p.expect(ENDDISCIPLINE_KW);
    m.complete(p, DISCIPLINE_DECL);
}

const NATURE_RECOVERY_SET: TokenSet = ITEM_RECOVERY_SET.union(TokenSet::unique(ENDNATURE_KW));

fn nature(p: &mut Parser, m: Marker) {
    p.bump(T![nature]);
    name_r(p, TokenSet::new(&[T![;], T![:]]));
    if p.eat(T![:]) {
        name_ref_r(p, TokenSet::unique(T![;]));
    }
    p.expect(T![;]);
    while !p.at_ts(NATURE_RECOVERY_SET) {
        let m = p.start();

        name_r(p, TokenSet::unique(T![=]));
        p.expect(T![=]);
        expr(p);
        if !p.eat(T![;]) {
            let err = p.unexpected_token_msg(T![;]);
            p.err_recover(err, NATURE_RECOVERY_SET.union(TokenSet::unique(IDENT)));
        }
        m.complete(p, NATURE_ATTR);
    }
    p.expect(ENDNATURE_KW);
    m.complete(p, NATURE_DECL);
}

pub(super) fn decl_list(
    p: &mut Parser,
    terminator: SyntaxKind,
    mut parse_entry: impl FnMut(&mut Parser) -> bool,
    recovery: TokenSet,
) {
    let recovery = recovery.union(TokenSet::new(&[terminator]));
    if p.at_ts(recovery) {
        p.error(p.unexpected_token_msg(IDENT));
    } else {
        while !p.at_ts(recovery) && parse_entry(p) {
            if !p.at(terminator) {
                p.expect_with(T![,], &[T![,], terminator]);
            }
        }
    }
}

pub(super) fn decl_name(p: &mut Parser) -> bool {
    name_r(p, TokenSet::new(&[T![,], T![;]]));
    true
}

pub(super) fn var_decl(p: &mut Parser, m: Marker) {
    ty(p);
    decl_list(p, T![;], var, MODULE_ITEM_OR_ATTR_RECOVERY);
    p.eat(T![;]);
    m.complete(p, VAR_DECL);
}

fn var(p: &mut Parser) -> bool {
    let m = p.start();
    name_r(p, TokenSet::new(&[T![,], T![=], T![;]]));
    if p.eat(T![=]) {
        expr(p);
    }
    m.complete(p, VAR);
    true
}

pub(super) fn parameter_decl(p: &mut Parser, m: Marker) {
    p.bump(PARAMETER_KW);
    ty(p);
    decl_list(p, T![;], parameter, MODULE_ITEM_OR_ATTR_RECOVERY);
    p.eat(T![;]);
    m.complete(p, PARAM_DECL);
}

const PARAM_RECOVER: TokenSet = MODULE_ITEM_OR_ATTR_RECOVERY.union(TokenSet::new(&[T![,], T![;]]));
fn parameter(p: &mut Parser) -> bool {
    let m = p.start();
    name_r(p, TokenSet::new(&[T![,], T![;]]));
    p.expect(T![=]);
    expr(p);
    while !p.at_ts(PARAM_RECOVER) {
        constraint(p)
    }
    m.complete(p, PARAM);
    true
}

fn constraint(p: &mut Parser) {
    let m = p.start();
    if !p.expect_ts_r(TokenSet::new(&[FROM_KW, EXCLUDE_KW]), PARAM_RECOVER) {
        m.abandon(p);
        return;
    }
    range_or_expr(p);
    m.complete(p, CONSTRAINT);
}

fn range_or_expr(p: &mut Parser) {
    let m = p.start();

    // while all branches parse an expr they need to eat [/( or nothing first
    #[allow(clippy::branches_sharing_code)]
    if p.eat(T!['(']) {
        expr(p);
        if !p.at(T![:]) {
            p.expect(T![')']);
            m.complete(p, PAREN_EXPR);
            return;
        }
    } else if p.eat(T!['[']) {
        expr(p);
    } else {
        expr(p);
        m.abandon(p);
        return;
    }

    p.expect(T![:]);
    expr(p);
    p.expect_ts(TokenSet::new(&[T![')'], T![']']]));
    m.complete(p, RANGE);
}
