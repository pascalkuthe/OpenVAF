use super::*;
use crate::error::Location;

#[test]
fn digits() {
    assert_eq!(trailing_digits(""), 0);
    assert_eq!(trailing_digits("x"), 0);
    assert_eq!(trailing_digits("0x"), 0);
    assert_eq!(trailing_digits("x1"), 1);
    assert_eq!(trailing_digits("1x1"), 1);
    assert_eq!(trailing_digits("1x01"), 2);
}

#[test]
fn entity_name() {
    assert_eq!(split_entity_name(""), None);
    assert_eq!(split_entity_name("x"), None);
    assert_eq!(split_entity_name("x+"), None);
    assert_eq!(split_entity_name("x+1"), Some(("x+", 1)));
    assert_eq!(split_entity_name("x-1"), Some(("x-", 1)));
    assert_eq!(split_entity_name("1"), Some(("", 1)));
    assert_eq!(split_entity_name("x1"), Some(("x", 1)));
    assert_eq!(split_entity_name("xy0"), Some(("xy", 0)));
    // Reject this non-canonical form.
    assert_eq!(split_entity_name("inst01"), None);
}

fn token(token: Token, line: usize) -> Option<Result<LocatedToken, LocatedError>> {
    Some(super::token(token, Location { line_number: line }))
}

fn error<'a>(error: LexError, line: usize) -> Option<Result<LocatedToken<'a>, LocatedError>> {
    Some(super::error(error, Location { line_number: line }))
}

#[test]
fn make_lexer() {
    let mut l1 = Lexer::new("");
    let mut l2 = Lexer::new(" ");
    let mut l3 = Lexer::new("\n ");

    assert_eq!(l1.next(), None);
    assert_eq!(l2.next(), None);
    assert_eq!(l3.next(), None);
}

#[test]
fn lex_comment() {
    let mut lex = Lexer::new("; hello");
    assert_eq!(lex.next(), token(Token::Comment("; hello"), 1));
    assert_eq!(lex.next(), None);

    lex = Lexer::new("\n  ;hello\n;foo");
    assert_eq!(lex.next(), token(Token::Comment(";hello"), 2));
    assert_eq!(lex.next(), token(Token::Comment(";foo"), 3));
    assert_eq!(lex.next(), None);

    // Scan a comment after an invalid char.
    let mut lex = Lexer::new("$; hello");
    assert_eq!(lex.next(), error(LexError::InvalidChar, 1));
    assert_eq!(lex.next(), token(Token::Comment("; hello"), 1));
    assert_eq!(lex.next(), None);
}

#[test]
fn lex_chars() {
    let mut lex = Lexer::new("(); hello\n = :{, }.");
    assert_eq!(lex.next(), token(Token::LPar, 1));
    assert_eq!(lex.next(), token(Token::RPar, 1));
    assert_eq!(lex.next(), token(Token::Comment("; hello"), 1));
    assert_eq!(lex.next(), token(Token::Equal, 2));
    assert_eq!(lex.next(), token(Token::Colon, 2));
    assert_eq!(lex.next(), token(Token::LBrace, 2));
    assert_eq!(lex.next(), token(Token::Comma, 2));
    assert_eq!(lex.next(), token(Token::RBrace, 2));
    assert_eq!(lex.next(), token(Token::Dot, 2));
    assert_eq!(lex.next(), None);
}

#[test]
fn lex_numbers() {
    let mut lex = Lexer::new(" 0 2_000 -1,0xf -0x0 0.0 0x0.4p-34 NaN +5");
    assert_eq!(lex.next(), token(Token::Integer("0"), 1));
    assert_eq!(lex.next(), token(Token::Integer("2_000"), 1));
    assert_eq!(lex.next(), token(Token::Integer("-1"), 1));
    assert_eq!(lex.next(), token(Token::Comma, 1));
    assert_eq!(lex.next(), token(Token::Integer("0xf"), 1));
    assert_eq!(lex.next(), token(Token::Integer("-0x0"), 1));
    assert_eq!(lex.next(), token(Token::Float("0.0"), 1));
    assert_eq!(lex.next(), token(Token::Float("0x0.4p-34"), 1));
    assert_eq!(lex.next(), token(Token::Float("NaN"), 1));
    assert_eq!(lex.next(), token(Token::Integer("+5"), 1));
    assert_eq!(lex.next(), None);
}

#[test]
fn lex_identifiers() {
    let mut lex = Lexer::new(
        "v0 v00 vx01 block1234567890 block5234567890 v1x vx1 vxvx4 \
             function0 function b1 i32x4 f32x5 \
             iflags fflags iflagss",
    );
    assert_eq!(lex.next(), token(Token::Value(Value::with_number(0).unwrap()), 1));
    assert_eq!(lex.next(), token(Token::Identifier("v00"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("vx01"), 1));
    assert_eq!(lex.next(), token(Token::Block(Block::with_number(1234567890).unwrap()), 1));
    assert_eq!(lex.next(), token(Token::Identifier("block5234567890"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("v1x"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("vx1"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("vxvx4"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("function0"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("function"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("b1"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("i32x4"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("f32x5"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("iflags"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("fflags"), 1));
    assert_eq!(lex.next(), token(Token::Identifier("iflagss"), 1));
    assert_eq!(lex.next(), None);
}

#[test]
fn lex_hex_sequences() {
    let mut lex = Lexer::new("#0 #DEADbeef123 #789");

    assert_eq!(lex.next(), token(Token::HexSequence("0"), 1));
    assert_eq!(lex.next(), token(Token::HexSequence("DEADbeef123"), 1));
    assert_eq!(lex.next(), token(Token::HexSequence("789"), 1));
}

#[test]
fn lex_strings() {
    let mut lex = Lexer::new(
        r#"""  "0" "x3""function" "123 abc" "\\" "start
                    and end on
                    different lines" "#,
    );

    assert_eq!(lex.next(), token(Token::String(""), 1));
    assert_eq!(lex.next(), token(Token::String("0"), 1));
    assert_eq!(lex.next(), token(Token::String("x3"), 1));
    assert_eq!(lex.next(), token(Token::String("function"), 1));
    assert_eq!(lex.next(), token(Token::String("123 abc"), 1));
    assert_eq!(lex.next(), token(Token::String(r#"\\"#), 1));
    assert_eq!(
        lex.next(),
        token(
            Token::String(
                r#"start
                    and end on
                    different lines"#
            ),
            1
        )
    );
}
