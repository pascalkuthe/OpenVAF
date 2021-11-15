use expect_test::{expect, Expect};
use text_size::{TextRange, TextSize};

use crate::tokenize;

fn check_lexing(src: &str, expect: Expect) {
    let mut offset = TextSize::from(0);
    let actual: String = tokenize(src)
        .into_iter()
        .map(|token| {
            let content = &src[TextRange::at(offset, token.len)];
            offset += token.len;
            format!("{:?}\n{:?}\n", token, content)
        })
        .collect();
    expect.assert_eq(&actual)
}

#[test]
fn smoke_test() {
    check_lexing(
        "/* my source file */ analog begin x = y; $test(\"zebra\"); end\n",
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 20 }
            "/* my source file */"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 6 }
            "analog"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 5 }
            "begin"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 1 }
            "x"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: Eq, len: 1 }
            "="
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 1 }
            "y"
            Token { kind: Semi, len: 1 }
            ";"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SystemCallIdent, len: 5 }
            "$test"
            Token { kind: OpenParen, len: 1 }
            "("
            Token { kind: Literal { kind: Str { terminated: true } }, len: 7 }
            "\"zebra\""
            Token { kind: CloseParen, len: 1 }
            ")"
            Token { kind: Semi, len: 1 }
            ";"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 3 }
            "end"
            Token { kind: Whitespace, len: 1 }
            "\n"
        "#]],
    )
}

#[test]
fn comment_flavors() {
    check_lexing(
        r"
// line
// line\
/* block */
/**/
/*** also block */
/** outer doc block */
/*! inner doc block */
/* foo //
",
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: LineComment, len: 7 }
            "// line"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: LineComment, len: 7 }
            "// line"
            Token { kind: Whitespace, len: 2 }
            "\\\n"
            Token { kind: BlockComment { terminated: true }, len: 11 }
            "/* block */"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: BlockComment { terminated: true }, len: 4 }
            "/**/"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: BlockComment { terminated: true }, len: 18 }
            "/*** also block */"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: BlockComment { terminated: true }, len: 22 }
            "/** outer doc block */"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: BlockComment { terminated: true }, len: 22 }
            "/*! inner doc block */"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: BlockComment { terminated: false }, len: 10 }
            "/* foo //\n"
        "#]],
    )
}

#[test]
fn numbers() {
    check_lexing(
        r"
27_195_000
3.141
10e9
10G
3.5m
1.2
0.1
2394.26331
1.2E12
// the exponent symbol can be e or E
1.30e-2
0.1e-0
23E10
29E-2
236.123_763_e-12 // underscores are ignored
1.3u
7k
",
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Int }, len: 10 }
            "27_195_000"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 5 }
            "3.141"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 4 }
            "10e9"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: true } }, len: 3 }
            "10G"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: true } }, len: 4 }
            "3.5m"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 3 }
            "1.2"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 3 }
            "0.1"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 10 }
            "2394.26331"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 6 }
            "1.2E12"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: LineComment, len: 36 }
            "// the exponent symbol can be e or E"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 7 }
            "1.30e-2"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 6 }
            "0.1e-0"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 5 }
            "23E10"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 5 }
            "29E-2"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: false } }, len: 16 }
            "236.123_763_e-12"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: LineComment, len: 26 }
            "// underscores are ignored"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: true } }, len: 4 }
            "1.3u"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Literal { kind: Float { has_scale_char: true } }, len: 2 }
            "7k"
            Token { kind: Whitespace, len: 1 }
            "\n"
        "#]],
    )
}

#[test]
fn idents() {
    check_lexing(
        r"
$fo2o
`_bar
bar$foo
$foo$bar
\busa+index \-clock
\***error-condition***
\net1/\net2
\{a,b}
\a*(b+c)
",
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: SystemCallIdent, len: 5 }
            "$fo2o"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: CompilerDirective, len: 5 }
            "`_bar"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: SimpleIdent, len: 7 }
            "bar$foo"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: SystemCallIdent, len: 8 }
            "$foo$bar"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: EscapedIdent, len: 11 }
            "\\busa+index"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: EscapedIdent, len: 7 }
            "\\-clock"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: EscapedIdent, len: 22 }
            "\\***error-condition***"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: EscapedIdent, len: 11 }
            "\\net1/\\net2"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: EscapedIdent, len: 6 }
            "\\{a,b}"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: EscapedIdent, len: 8 }
            "\\a*(b+c)"
            Token { kind: Whitespace, len: 1 }
            "\n"
        "#]],
    )
}

// Compared to rust VAMS explicitly disallows nested block comments so this is only one block
// comment
#[test]
fn nested_block_comments() {
    check_lexing(
        "/* /* */ */ \"foo\"",
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 8 }
            "/* /* */"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: Star, len: 1 }
            "*"
            Token { kind: Slash, len: 1 }
            "/"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: Literal { kind: Str { terminated: true } }, len: 5 }
            "\"foo\""
        "#]],
    )
}

#[test]
fn compiler_directives() {
    check_lexing(
        "`foo `bar `if `include `define `defines\\
        `defin `define
`define(x,y) (x)+(y)
`define",
        expect![[r#"
            Token { kind: CompilerDirective, len: 4 }
            "`foo"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: CompilerDirective, len: 4 }
            "`bar"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: CompilerDirective, len: 3 }
            "`if"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: CompilerDirective, len: 8 }
            "`include"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: Define { end: 15 }, len: 7 }
            "`define"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: CompilerDirective, len: 8 }
            "`defines"
            Token { kind: Whitespace, len: 10 }
            "\\\n        "
            Token { kind: CompilerDirective, len: 6 }
            "`defin"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: IllegalDefine, len: 7 }
            "`define"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Define { end: 30 }, len: 7 }
            "`define"
            Token { kind: OpenParen, len: 1 }
            "("
            Token { kind: SimpleIdent, len: 1 }
            "x"
            Token { kind: Comma, len: 1 }
            ","
            Token { kind: SimpleIdent, len: 1 }
            "y"
            Token { kind: CloseParen, len: 1 }
            ")"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: OpenParen, len: 1 }
            "("
            Token { kind: SimpleIdent, len: 1 }
            "x"
            Token { kind: CloseParen, len: 1 }
            ")"
            Token { kind: Plus, len: 1 }
            "+"
            Token { kind: OpenParen, len: 1 }
            "("
            Token { kind: SimpleIdent, len: 1 }
            "y"
            Token { kind: CloseParen, len: 1 }
            ")"
            Token { kind: Whitespace, len: 1 }
            "\n"
            Token { kind: Define { end: 32 }, len: 7 }
            "`define"
        "#]],
    );
}

#[test]
fn comment_in_macro() {
    check_lexing(
        "`define FOO\\
        // This is a comment inside the macro\\
        BAR\\
        //This comment is still inside the macro!
        `define NOT_INSIDE_FOO
        // yay
        ",
        expect![[r#"
            Token { kind: Define { end: 9 }, len: 7 }
            "`define"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 3 }
            "FOO"
            Token { kind: Whitespace, len: 10 }
            "\\\n        "
            Token { kind: LineComment, len: 37 }
            "// This is a comment inside the macro"
            Token { kind: Whitespace, len: 10 }
            "\\\n        "
            Token { kind: SimpleIdent, len: 3 }
            "BAR"
            Token { kind: Whitespace, len: 10 }
            "\\\n        "
            Token { kind: LineComment, len: 41 }
            "//This comment is still inside the macro!"
            Token { kind: Whitespace, len: 9 }
            "\n        "
            Token { kind: Define { end: 13 }, len: 7 }
            "`define"
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 14 }
            "NOT_INSIDE_FOO"
            Token { kind: Whitespace, len: 9 }
            "\n        "
            Token { kind: LineComment, len: 6 }
            "// yay"
            Token { kind: Whitespace, len: 9 }
            "\n        "
        "#]],
    );
}

#[test]
fn array() {
    check_lexing(
        "'{a, b, c}",
        expect![[r#"
            Token { kind: ArrStart, len: 2 }
            "'{"
            Token { kind: SimpleIdent, len: 1 }
            "a"
            Token { kind: Comma, len: 1 }
            ","
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 1 }
            "b"
            Token { kind: Comma, len: 1 }
            ","
            Token { kind: Whitespace, len: 1 }
            " "
            Token { kind: SimpleIdent, len: 1 }
            "c"
            Token { kind: CloseBrace, len: 1 }
            "}"
        "#]],
    );
}
