use super::*;
use expect_test::expect;

#[test]
fn number_of_blocks() {
    let ParseError { location, message, is_warning } = Parser::new(
        "function %a() {
                block100000:",
    )
    .parse_function()
    .unwrap_err();

    assert_eq!(message, "too many blocks");
    assert_eq!(location.line_number, 2);
    assert!(!is_warning);
}

#[test]
fn i32_as_hex() {
    fn parse_as_imm32(text: &str) -> ParseResult<i32> {
        Parser::new(text).match_imm("unable to parse i32")
    }

    assert_eq!(parse_as_imm32("0x80000000").unwrap(), -2147483648);
    assert_eq!(parse_as_imm32("0xffffffff").unwrap(), -1);
    assert_eq!(parse_as_imm32("0").unwrap(), 0);
    assert_eq!(parse_as_imm32("0x7fffffff").unwrap(), 2147483647);
    assert_eq!(parse_as_imm32("-0x00000001").unwrap(), parse_as_imm32("0xffffffff").unwrap());
    assert_eq!(parse_as_imm32("-0x7fffffff").unwrap(), parse_as_imm32("0x80000001").unwrap());
    assert!(parse_as_imm32("0xffffffffa").is_err());
}

#[test]
fn roundtrip() {
    let expected = expect![[r#"
        function %bar(v4, v8, v9, v10) {
            v5 = iconst 42
            v6 = iconst 23
        block0:
            v7 = iadd v5, v8
            v11 = iadd v6, v9
            v12 = ilt v8, v10
            br v12, block1, block2

        block1:
            v13 = isub v7, v10
            jmp block3

        block2:
            jmp block3

        block3:
            v14 = phi [v13, block1], [v11, block2]
        }
    "#]];

    let (fun, interner) = parse_function(expected.data).unwrap();
    let printed = fun.print(&interner).to_string();
    expected.assert_eq(&printed)
}
