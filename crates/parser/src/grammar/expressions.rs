use super::*;
use crate::grammar::call::{call, sys_fun_call};
use crate::grammar::paths::path;

const EXPR_EXPECTED: &[SyntaxKind] =
    &[T!['('], T!["'{"], SYSFUN, NAME, LITERAL, T![~], T![!], T![+], T![-]];

pub(super) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 1)
}

/// Binding powers of operators for a Pratt parser.
///
/// See <https://www.oilshell.org/blog/2016/11/03.html>
#[rustfmt::skip]
fn current_op(p: &Parser) -> (u8, SyntaxKind) {
    const NOT_AN_OP: (u8, SyntaxKind) = (0, T![@]);
    match p.current() {
        T![?]  => (1,   T![?]),
        T![||]  => (2,   T![||]),
        
        T![&&]  => (3,   T![&&]),
        
        T![|]   => (4,   T![|]),
        
        T![^]   => (5,   T![^]),
        
        T![~^]  => (6,   T![~^]),
        T![^~]  => (6,   T![^~]),
        
        T![&]   => (7,   T![&]),
        
        T![==]  => (8,   T![==]),
        T![!=]  => (8,   T![!=]),

        T![>=]  => (9,   T![>=]),
        T![>]   => (9,   T![>]),
        T![<=]  => (9,   T![<=]),
        T![<]   => (9,   T![<]),
        
        T![<<]   => (10,  T![<<]),
        T![>>]   => (10,  T![>>]),
        T![>>>]   => (10,  T![>>>]),
        T![<<<]   => (10,  T![<<<]),

        T![+]    => (11,  T![+]),
        T![-]    => (11,  T![-]),
        
        T![*]    => (12,  T![*]),
        T![/]    => (12,  T![/]),

        T![%]    => (13, T![%]),

        T![**]   => (14, T![**]),

        _        => NOT_AN_OP
    }
}

// Parses expression with binding power of at least bp.
fn expr_bp(p: &mut Parser, bp: u8) -> Option<CompletedMarker> {
    let mut lhs = atom_expr(p)?;

    loop {
        let (op_bp, op) = current_op(p);

        if op_bp < bp {
            break;
        }

        if op == T![?] {
            let m = lhs.precede(p);
            p.bump(T![?]);
            expr(p);
            p.expect(T![:]);
            expr(p);
            return Some(m.complete(p, SELECT_EXPR));
        }

        let m = lhs.precede(p);
        p.bump(op);

        expr_bp(p, op_bp + 1);
        lhs = m.complete(p, BIN_EXPR);
    }
    Some(lhs)
}

pub(crate) const EXPR_RECOVERY_SET: TokenSet = TokenSet::new(&[
    T![;],
    T![endmodule],
    T![endfunction],
    T![endnature],
    T![enddiscipline],
    T![endcase],
    T![end],
]);

fn atom_expr(p: &mut Parser) -> Option<CompletedMarker> {
    // if let Some(m) = literal(p) {
    //     return Some(m);
    // }

    let done = match p.current() {
        T!['('] => paren_expr(p),
        T!["'{"] => array_expr(p),
        T![~] | T![!] | T![-] | T![+] => {
            let m = p.start();
            p.bump_ts(TokenSet::new(&[T![~], T![!], T![-], T![+]]));
            atom_expr(p);
            m.complete(p, PREFIX_EXPR)
        }
        IDENT | ROOT_KW => {
            let m = path(p);
            if p.at(T!('(')) {
                call(p, m)
            } else {
                let m = m.precede(p);
                m.complete(p, PATH_EXPR)
            }
        }
        SYSFUN => sys_fun_call(p),
        T![<] => port_flow(p),
        INT_NUMBER | SI_REAL_NUMBER | STD_REAL_NUMBER | STR_LIT | INF_KW => {
            let m = p.start();
            p.bump_any();
            m.complete(p, LITERAL)
        }
        _ => {
            p.err_recover(p.unexpected_tokens_msg(EXPR_EXPECTED.to_owned()), EXPR_RECOVERY_SET);
            return None;
        }
    };
    Some(done)
}

fn port_flow(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(T![<]);
    path(p);
    p.expect(T![>]);
    m.complete(p, PORT_FLOW)
}

fn paren_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(T!['(']);

    while !p.at(EOF) && !p.at(T![')']) {
        // test tuple_attrs
        // const A: (i64, i64) = (1, #[cfg(test)] 2);
        if expr(p).is_none() {
            break;
        }

        if !p.at(T![')']) {
            p.expect(T![,]);
        }
    }
    p.expect(T![')']);
    m.complete(p, PAREN_EXPR)
}

fn array_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(T!["'{"]);
    while !p.at(EOF) && !p.at(T![']']) {
        // test array_attrs
        // const A: &[i64] = &[1, #[cfg(test)] 2];
        if expr(p).is_none() {
            break;
        }

        if !p.at(T!['}']) && !p.expect(T![,]) {
            break;
        }
    }
    p.expect(T!['}']);

    m.complete(p, ARRAY_EXPR)
}
