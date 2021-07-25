use super::*;
use crate::grammar::expressions::expr;

pub(super) fn attrs(p: &mut Parser, recovery: TokenSet) {
    while p.at(T!["(*"]) {
        attr_list(p, recovery)
    }
}

fn attr_list(p: &mut Parser, recovery: TokenSet) {
    let m = p.start();
    p.bump(T!["(*"]);

    while !p.at_ts(TokenSet::new(&[EOF,T!["*)"]])) {
        attr(p, recovery);

        if !p.at(T!["*)"]) {
            p.expect_with(T![,],vec![T![,],T!["*)"]]);
        }
    }

    p.eat(T!["*)"]);
    m.complete(p, ATTR_LIST);
}

const ATTR_RECOVERY_SET: TokenSet = TokenSet::new(&[T!["(*"], T!["*)"], T![=], T![,]]);

fn attr(p: &mut Parser, recovery: TokenSet) {
    let m = p.start();

    name_r(p, ATTR_RECOVERY_SET.union(recovery));
    if p.eat(T![=]) {
        expr(p);
    }
    m.complete(p, ATTR);
}
