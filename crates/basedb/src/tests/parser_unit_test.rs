use expect_test::{expect, Expect};
use syntax::AstNode;

use crate::tests::TestDataBase;
use crate::BaseDB;

fn check(src: &str, expected: Expect) {
    let (parse, diagnostics) =
        TestDataBase::new("/root.va", src.to_owned().into()).parse_and_check();
    if !diagnostics.is_empty() {
        eprintln!("{}", diagnostics);
        panic!("encountered unexpected diagnostics")
    }
    let actual = format!("{:#?}", parse.tree().syntax());
    expected.assert_eq(&actual)
}

#[test]
fn source_mapping() {
    let src = r#"
`define foo(result, x);

`define y_fv(fv,y);

`define expLin(result, x);

foo
    "#;
    let db = TestDataBase::new("/root.va", src.to_owned().into());
    eprintln!("{:?}", db.preprocess(db.root_file()).ts);
    let (parse, diagnostics) = db.parse_and_check();
    eprintln!("{:?}", parse.errors());
    expect![[r#"
        error: unexpected token 'an identifier'; expected 'discipline', 'nature' or 'module'
          ┌─ /root.va:8:1
          │
        8 │ foo
          │ ^^^ expected 'discipline', 'nature' or 'module'

    "#]]
    .assert_eq(&diagnostics)

    // foo
}

#[test]
fn branches() {
    check(
        r#"
module test (output a,input b);
    branch (a,b) ab1,ab2;
    branch (a) a_gnd;
    branch (<$root.test.b>) port_flow_b;
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..132
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..131
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                WHITESPACE@12..13 " "
                MODULE_PORTS@13..31
                  L_PAREN@13..14 "("
                  MODULE_PORT@14..22
                    PORT_DECL@14..22
                      DIRECTION@14..20
                        OUTPUT_KW@14..20 "output"
                      WHITESPACE@20..21 " "
                      NAME@21..22
                        IDENT@21..22 "a"
                  COMMA@22..23 ","
                  MODULE_PORT@23..30
                    PORT_DECL@23..30
                      DIRECTION@23..28
                        INPUT_KW@23..28 "input"
                      WHITESPACE@28..29 " "
                      NAME@29..30
                        IDENT@29..30 "b"
                  R_PAREN@30..31 ")"
                SEMICOLON@31..32 ";"
                WHITESPACE@32..37 "\n    "
                BRANCH_DECL@37..58
                  BRANCH_KW@37..43 "branch"
                  WHITESPACE@43..44 " "
                  ARG_LIST@44..49
                    L_PAREN@44..45 "("
                    PATH_EXPR@45..46
                      PATH@45..46
                        IDENT@45..46 "a"
                    COMMA@46..47 ","
                    PATH_EXPR@47..48
                      PATH@47..48
                        IDENT@47..48 "b"
                    R_PAREN@48..49 ")"
                  WHITESPACE@49..50 " "
                  NAME@50..53
                    IDENT@50..53 "ab1"
                  COMMA@53..54 ","
                  NAME@54..57
                    IDENT@54..57 "ab2"
                  SEMICOLON@57..58 ";"
                WHITESPACE@58..63 "\n    "
                BRANCH_DECL@63..80
                  BRANCH_KW@63..69 "branch"
                  WHITESPACE@69..70 " "
                  ARG_LIST@70..73
                    L_PAREN@70..71 "("
                    PATH_EXPR@71..72
                      PATH@71..72
                        IDENT@71..72 "a"
                    R_PAREN@72..73 ")"
                  WHITESPACE@73..74 " "
                  NAME@74..79
                    IDENT@74..79 "a_gnd"
                  SEMICOLON@79..80 ";"
                WHITESPACE@80..85 "\n    "
                BRANCH_DECL@85..121
                  BRANCH_KW@85..91 "branch"
                  WHITESPACE@91..92 " "
                  ARG_LIST@92..108
                    L_PAREN@92..93 "("
                    PORT_FLOW@93..107
                      L_ANGLE@93..94 "<"
                      PATH@94..106
                        PATH@94..104
                          PATH@94..99
                            ROOT_KW@94..99 "$root"
                          DOT@99..100 "."
                          IDENT@100..104 "test"
                        DOT@104..105 "."
                        IDENT@105..106 "b"
                      R_ANGLE@106..107 ">"
                    R_PAREN@107..108 ")"
                  WHITESPACE@108..109 " "
                  NAME@109..120
                    IDENT@109..120 "port_flow_b"
                  SEMICOLON@120..121 ";"
                WHITESPACE@121..122 "\n"
                ENDMODULE_KW@122..131 "endmodule"
              WHITESPACE@131..132 "\n"
        "#]],
    )
}

#[test]
fn contribute() {
    check(
        r#"
module test;
    analog begin
        I(x)<+(V(x)-V0)**2/(R1R2);
    end
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..84
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..83
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                SEMICOLON@12..13 ";"
                WHITESPACE@13..18 "\n    "
                ANALOG_BEHAVIOUR@18..73
                  ANALOG_KW@18..24 "analog"
                  WHITESPACE@24..25 " "
                  BLOCK_STMT@25..73
                    BEGIN_KW@25..30 "begin"
                    WHITESPACE@30..39 "\n        "
                    ASSIGN_STMT@39..65
                      ASSIGN@39..64
                        CALL@39..43
                          PATH@39..40
                            IDENT@39..40 "I"
                          ARG_LIST@40..43
                            L_PAREN@40..41 "("
                            PATH_EXPR@41..42
                              PATH@41..42
                                IDENT@41..42 "x"
                            R_PAREN@42..43 ")"
                        CONTR@43..45 "<+"
                        BIN_EXPR@45..64
                          BIN_EXPR@45..57
                            PAREN_EXPR@45..54
                              L_PAREN@45..46 "("
                              BIN_EXPR@46..53
                                CALL@46..50
                                  PATH@46..47
                                    IDENT@46..47 "V"
                                  ARG_LIST@47..50
                                    L_PAREN@47..48 "("
                                    PATH_EXPR@48..49
                                      PATH@48..49
                                        IDENT@48..49 "x"
                                    R_PAREN@49..50 ")"
                                MINUS@50..51 "-"
                                PATH_EXPR@51..53
                                  PATH@51..53
                                    IDENT@51..53 "V0"
                              R_PAREN@53..54 ")"
                            POW@54..56 "**"
                            LITERAL@56..57
                              INT_NUMBER@56..57 "2"
                          SLASH@57..58 "/"
                          PAREN_EXPR@58..64
                            L_PAREN@58..59 "("
                            PATH_EXPR@59..63
                              PATH@59..63
                                IDENT@59..63 "R1R2"
                            R_PAREN@63..64 ")"
                      SEMICOLON@64..65 ";"
                    WHITESPACE@65..70 "\n    "
                    END_KW@70..73 "end"
                WHITESPACE@73..74 "\n"
                ENDMODULE_KW@74..83 "endmodule"
              WHITESPACE@83..84 "\n"
        "#]],
    )
}

#[test]
fn ports() {
    check(
        r#"
module test1;
endmodule

module test2 (a,b,c,d,e,f,g);
    output a;
    input b;
    inout ground c;
    inout electrical d;
    output electrical e,f;
    inout electrical ground g;
endmodule

module test3 (output a,b, input electrical c,d,e, (*foo*) inout electrical ground f);
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..292
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..24
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..13
                  IDENT@8..13 "test1"
                SEMICOLON@13..14 ";"
                WHITESPACE@14..15 "\n"
                ENDMODULE_KW@15..24 "endmodule"
              WHITESPACE@24..26 "\n\n"
              MODULE_DECL@26..194
                MODULE_KW@26..32 "module"
                WHITESPACE@32..33 " "
                NAME@33..38
                  IDENT@33..38 "test2"
                WHITESPACE@38..39 " "
                MODULE_PORTS@39..54
                  L_PAREN@39..40 "("
                  MODULE_PORT@40..41
                    NAME@40..41
                      IDENT@40..41 "a"
                  COMMA@41..42 ","
                  MODULE_PORT@42..43
                    NAME@42..43
                      IDENT@42..43 "b"
                  COMMA@43..44 ","
                  MODULE_PORT@44..45
                    NAME@44..45
                      IDENT@44..45 "c"
                  COMMA@45..46 ","
                  MODULE_PORT@46..47
                    NAME@46..47
                      IDENT@46..47 "d"
                  COMMA@47..48 ","
                  MODULE_PORT@48..49
                    NAME@48..49
                      IDENT@48..49 "e"
                  COMMA@49..50 ","
                  MODULE_PORT@50..51
                    NAME@50..51
                      IDENT@50..51 "f"
                  COMMA@51..52 ","
                  MODULE_PORT@52..53
                    NAME@52..53
                      IDENT@52..53 "g"
                  R_PAREN@53..54 ")"
                SEMICOLON@54..55 ";"
                WHITESPACE@55..60 "\n    "
                BODY_PORT_DECL@60..69
                  PORT_DECL@60..68
                    DIRECTION@60..66
                      OUTPUT_KW@60..66 "output"
                    WHITESPACE@66..67 " "
                    NAME@67..68
                      IDENT@67..68 "a"
                  SEMICOLON@68..69 ";"
                WHITESPACE@69..74 "\n    "
                BODY_PORT_DECL@74..82
                  PORT_DECL@74..81
                    DIRECTION@74..79
                      INPUT_KW@74..79 "input"
                    WHITESPACE@79..80 " "
                    NAME@80..81
                      IDENT@80..81 "b"
                  SEMICOLON@81..82 ";"
                WHITESPACE@82..87 "\n    "
                BODY_PORT_DECL@87..102
                  PORT_DECL@87..101
                    DIRECTION@87..92
                      INOUT_KW@87..92 "inout"
                    WHITESPACE@92..93 " "
                    NET_TYPE@93..99 "ground"
                    WHITESPACE@99..100 " "
                    NAME@100..101
                      IDENT@100..101 "c"
                  SEMICOLON@101..102 ";"
                WHITESPACE@102..107 "\n    "
                BODY_PORT_DECL@107..126
                  PORT_DECL@107..125
                    DIRECTION@107..112
                      INOUT_KW@107..112 "inout"
                    WHITESPACE@112..113 " "
                    NAME_REF@113..123
                      IDENT@113..123 "electrical"
                    WHITESPACE@123..124 " "
                    NAME@124..125
                      IDENT@124..125 "d"
                  SEMICOLON@125..126 ";"
                WHITESPACE@126..131 "\n    "
                BODY_PORT_DECL@131..153
                  PORT_DECL@131..152
                    DIRECTION@131..137
                      OUTPUT_KW@131..137 "output"
                    WHITESPACE@137..138 " "
                    NAME_REF@138..148
                      IDENT@138..148 "electrical"
                    WHITESPACE@148..149 " "
                    NAME@149..150
                      IDENT@149..150 "e"
                    COMMA@150..151 ","
                    NAME@151..152
                      IDENT@151..152 "f"
                  SEMICOLON@152..153 ";"
                WHITESPACE@153..158 "\n    "
                BODY_PORT_DECL@158..184
                  PORT_DECL@158..183
                    DIRECTION@158..163
                      INOUT_KW@158..163 "inout"
                    WHITESPACE@163..164 " "
                    NAME_REF@164..174
                      IDENT@164..174 "electrical"
                    WHITESPACE@174..175 " "
                    NET_TYPE@175..181 "ground"
                    WHITESPACE@181..182 " "
                    NAME@182..183
                      IDENT@182..183 "g"
                  SEMICOLON@183..184 ";"
                WHITESPACE@184..185 "\n"
                ENDMODULE_KW@185..194 "endmodule"
              WHITESPACE@194..196 "\n\n"
              MODULE_DECL@196..291
                MODULE_KW@196..202 "module"
                WHITESPACE@202..203 " "
                NAME@203..208
                  IDENT@203..208 "test3"
                WHITESPACE@208..209 " "
                MODULE_PORTS@209..280
                  L_PAREN@209..210 "("
                  MODULE_PORT@210..220
                    PORT_DECL@210..220
                      DIRECTION@210..216
                        OUTPUT_KW@210..216 "output"
                      WHITESPACE@216..217 " "
                      NAME@217..218
                        IDENT@217..218 "a"
                      COMMA@218..219 ","
                      NAME@219..220
                        IDENT@219..220 "b"
                  COMMA@220..221 ","
                  WHITESPACE@221..222 " "
                  MODULE_PORT@222..244
                    PORT_DECL@222..244
                      DIRECTION@222..227
                        INPUT_KW@222..227 "input"
                      WHITESPACE@227..228 " "
                      NAME_REF@228..238
                        IDENT@228..238 "electrical"
                      WHITESPACE@238..239 " "
                      NAME@239..240
                        IDENT@239..240 "c"
                      COMMA@240..241 ","
                      NAME@241..242
                        IDENT@241..242 "d"
                      COMMA@242..243 ","
                      NAME@243..244
                        IDENT@243..244 "e"
                  COMMA@244..245 ","
                  WHITESPACE@245..246 " "
                  MODULE_PORT@246..279
                    PORT_DECL@246..279
                      ATTR_LIST@246..253
                        L_ATTR_PAREN@246..248 "(*"
                        ATTR@248..251
                          NAME@248..251
                            IDENT@248..251 "foo"
                        R_ATTR_PAREN@251..253 "*)"
                      WHITESPACE@253..254 " "
                      DIRECTION@254..259
                        INOUT_KW@254..259 "inout"
                      WHITESPACE@259..260 " "
                      NAME_REF@260..270
                        IDENT@260..270 "electrical"
                      WHITESPACE@270..271 " "
                      NET_TYPE@271..277 "ground"
                      WHITESPACE@277..278 " "
                      NAME@278..279
                        IDENT@278..279 "f"
                  R_PAREN@279..280 ")"
                SEMICOLON@280..281 ";"
                WHITESPACE@281..282 "\n"
                ENDMODULE_KW@282..291 "endmodule"
              WHITESPACE@291..292 "\n"
        "#]],
    )
}

#[test]
fn net_decl() {
    check(
        r#"
module test ();
    ground x,y;
    electrical z;
    ground electrical l;

endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..87
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..86
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                WHITESPACE@12..13 " "
                MODULE_PORTS@13..15
                  L_PAREN@13..14 "("
                  R_PAREN@14..15 ")"
                SEMICOLON@15..16 ";"
                WHITESPACE@16..21 "\n    "
                NET_DECL@21..32
                  NET_TYPE@21..27 "ground"
                  WHITESPACE@27..28 " "
                  NAME@28..29
                    IDENT@28..29 "x"
                  COMMA@29..30 ","
                  NAME@30..31
                    IDENT@30..31 "y"
                  SEMICOLON@31..32 ";"
                WHITESPACE@32..37 "\n    "
                NET_DECL@37..50
                  NAME_REF@37..47
                    IDENT@37..47 "electrical"
                  WHITESPACE@47..48 " "
                  NAME@48..49
                    IDENT@48..49 "z"
                  SEMICOLON@49..50 ";"
                WHITESPACE@50..55 "\n    "
                NET_DECL@55..75
                  NET_TYPE@55..61 "ground"
                  WHITESPACE@61..62 " "
                  NAME_REF@62..72
                    IDENT@62..72 "electrical"
                  WHITESPACE@72..73 " "
                  NAME@73..74
                    IDENT@73..74 "l"
                  SEMICOLON@74..75 ";"
                WHITESPACE@75..77 "\n\n"
                ENDMODULE_KW@77..86 "endmodule"
              WHITESPACE@86..87 "\n"
        "#]],
    )
}

#[test]
fn var_declarations() {
    check(
        r#"
module test ();
    real x=1.0;
    integer y=0,z;
    real t;
    int rt;
    analog x = 2==y;
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..107
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..106
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                WHITESPACE@12..13 " "
                MODULE_PORTS@13..15
                  L_PAREN@13..14 "("
                  R_PAREN@14..15 ")"
                SEMICOLON@15..16 ";"
                WHITESPACE@16..21 "\n    "
                VAR_DECL@21..32
                  TYPE@21..25
                    REAL_KW@21..25 "real"
                  WHITESPACE@25..26 " "
                  VAR@26..31
                    NAME@26..27
                      IDENT@26..27 "x"
                    EQ@27..28 "="
                    LITERAL@28..31
                      STD_REAL_NUMBER@28..31 "1.0"
                  SEMICOLON@31..32 ";"
                WHITESPACE@32..37 "\n    "
                VAR_DECL@37..51
                  TYPE@37..44
                    INTEGER_KW@37..44 "integer"
                  WHITESPACE@44..45 " "
                  VAR@45..48
                    NAME@45..46
                      IDENT@45..46 "y"
                    EQ@46..47 "="
                    LITERAL@47..48
                      INT_NUMBER@47..48 "0"
                  COMMA@48..49 ","
                  VAR@49..50
                    NAME@49..50
                      IDENT@49..50 "z"
                  SEMICOLON@50..51 ";"
                WHITESPACE@51..56 "\n    "
                VAR_DECL@56..63
                  TYPE@56..60
                    REAL_KW@56..60 "real"
                  WHITESPACE@60..61 " "
                  VAR@61..62
                    NAME@61..62
                      IDENT@61..62 "t"
                  SEMICOLON@62..63 ";"
                WHITESPACE@63..68 "\n    "
                NET_DECL@68..75
                  NAME_REF@68..71
                    IDENT@68..71 "int"
                  WHITESPACE@71..72 " "
                  NAME@72..74
                    IDENT@72..74 "rt"
                  SEMICOLON@74..75 ";"
                WHITESPACE@75..80 "\n    "
                ANALOG_BEHAVIOUR@80..96
                  ANALOG_KW@80..86 "analog"
                  WHITESPACE@86..87 " "
                  ASSIGN_STMT@87..96
                    ASSIGN@87..95
                      PATH_EXPR@87..88
                        PATH@87..88
                          IDENT@87..88 "x"
                      WHITESPACE@88..89 " "
                      EQ@89..90 "="
                      WHITESPACE@90..91 " "
                      BIN_EXPR@91..95
                        LITERAL@91..92
                          INT_NUMBER@91..92 "2"
                        EQ2@92..94 "=="
                        PATH_EXPR@94..95
                          PATH@94..95
                            IDENT@94..95 "y"
                    SEMICOLON@95..96 ";"
                WHITESPACE@96..97 "\n"
                ENDMODULE_KW@97..106 "endmodule"
              WHITESPACE@106..107 "\n"
        "#]],
    )
}

#[test]
fn nets() {
    check(
        r#"
module test();
    electrical foo1;
    electrical foo2,bar2;
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..73
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..72
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                MODULE_PORTS@12..14
                  L_PAREN@12..13 "("
                  R_PAREN@13..14 ")"
                SEMICOLON@14..15 ";"
                WHITESPACE@15..20 "\n    "
                NET_DECL@20..36
                  NAME_REF@20..30
                    IDENT@20..30 "electrical"
                  WHITESPACE@30..31 " "
                  NAME@31..35
                    IDENT@31..35 "foo1"
                  SEMICOLON@35..36 ";"
                WHITESPACE@36..41 "\n    "
                NET_DECL@41..62
                  NAME_REF@41..51
                    IDENT@41..51 "electrical"
                  WHITESPACE@51..52 " "
                  NAME@52..56
                    IDENT@52..56 "foo2"
                  COMMA@56..57 ","
                  NAME@57..61
                    IDENT@57..61 "bar2"
                  SEMICOLON@61..62 ";"
                WHITESPACE@62..63 "\n"
                ENDMODULE_KW@63..72 "endmodule"
              WHITESPACE@72..73 "\n"
        "#]],
    )
}

#[test]
fn functions() {
    check(
        r#"
module test();
        analog function real hypsmooth;
            input x , c;
            output y , z;
            inout m , l;
            real x , c;
            integer m , z;
            string l , y;
            begin
                hypsmooth = 0.5 * (x + sqrt(x*x + 4.0*c*c));
            end
        endfunction
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..334
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..333
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                MODULE_PORTS@12..14
                  L_PAREN@12..13 "("
                  R_PAREN@13..14 ")"
                SEMICOLON@14..15 ";"
                WHITESPACE@15..24 "\n        "
                FUNCTION@24..323
                  ANALOG_KW@24..30 "analog"
                  WHITESPACE@30..31 " "
                  FUNCTION_KW@31..39 "function"
                  WHITESPACE@39..40 " "
                  TYPE@40..44
                    REAL_KW@40..44 "real"
                  WHITESPACE@44..45 " "
                  NAME@45..54
                    IDENT@45..54 "hypsmooth"
                  SEMICOLON@54..55 ";"
                  WHITESPACE@55..68 "\n            "
                  FUNCTION_ARG@68..80
                    DIRECTION@68..73
                      INPUT_KW@68..73 "input"
                    WHITESPACE@73..74 " "
                    NAME@74..75
                      IDENT@74..75 "x"
                    WHITESPACE@75..76 " "
                    COMMA@76..77 ","
                    WHITESPACE@77..78 " "
                    NAME@78..79
                      IDENT@78..79 "c"
                    SEMICOLON@79..80 ";"
                  WHITESPACE@80..93 "\n            "
                  FUNCTION_ARG@93..106
                    DIRECTION@93..99
                      OUTPUT_KW@93..99 "output"
                    WHITESPACE@99..100 " "
                    NAME@100..101
                      IDENT@100..101 "y"
                    WHITESPACE@101..102 " "
                    COMMA@102..103 ","
                    WHITESPACE@103..104 " "
                    NAME@104..105
                      IDENT@104..105 "z"
                    SEMICOLON@105..106 ";"
                  WHITESPACE@106..119 "\n            "
                  FUNCTION_ARG@119..131
                    DIRECTION@119..124
                      INOUT_KW@119..124 "inout"
                    WHITESPACE@124..125 " "
                    NAME@125..126
                      IDENT@125..126 "m"
                    WHITESPACE@126..127 " "
                    COMMA@127..128 ","
                    WHITESPACE@128..129 " "
                    NAME@129..130
                      IDENT@129..130 "l"
                    SEMICOLON@130..131 ";"
                  WHITESPACE@131..144 "\n            "
                  VAR_DECL@144..155
                    TYPE@144..148
                      REAL_KW@144..148 "real"
                    WHITESPACE@148..149 " "
                    VAR@149..150
                      NAME@149..150
                        IDENT@149..150 "x"
                    WHITESPACE@150..151 " "
                    COMMA@151..152 ","
                    WHITESPACE@152..153 " "
                    VAR@153..154
                      NAME@153..154
                        IDENT@153..154 "c"
                    SEMICOLON@154..155 ";"
                  WHITESPACE@155..168 "\n            "
                  VAR_DECL@168..182
                    TYPE@168..175
                      INTEGER_KW@168..175 "integer"
                    WHITESPACE@175..176 " "
                    VAR@176..177
                      NAME@176..177
                        IDENT@176..177 "m"
                    WHITESPACE@177..178 " "
                    COMMA@178..179 ","
                    WHITESPACE@179..180 " "
                    VAR@180..181
                      NAME@180..181
                        IDENT@180..181 "z"
                    SEMICOLON@181..182 ";"
                  WHITESPACE@182..195 "\n            "
                  VAR_DECL@195..208
                    TYPE@195..201
                      STRING_KW@195..201 "string"
                    WHITESPACE@201..202 " "
                    VAR@202..203
                      NAME@202..203
                        IDENT@202..203 "l"
                    WHITESPACE@203..204 " "
                    COMMA@204..205 ","
                    WHITESPACE@205..206 " "
                    VAR@206..207
                      NAME@206..207
                        IDENT@206..207 "y"
                    SEMICOLON@207..208 ";"
                  WHITESPACE@208..221 "\n            "
                  BLOCK_STMT@221..303
                    BEGIN_KW@221..226 "begin"
                    WHITESPACE@226..243 "\n                "
                    ASSIGN_STMT@243..287
                      ASSIGN@243..286
                        PATH_EXPR@243..252
                          PATH@243..252
                            IDENT@243..252 "hypsmooth"
                        WHITESPACE@252..253 " "
                        EQ@253..254 "="
                        WHITESPACE@254..255 " "
                        BIN_EXPR@255..286
                          LITERAL@255..258
                            STD_REAL_NUMBER@255..258 "0.5"
                          WHITESPACE@258..259 " "
                          STAR@259..260 "*"
                          WHITESPACE@260..261 " "
                          PAREN_EXPR@261..286
                            L_PAREN@261..262 "("
                            BIN_EXPR@262..285
                              PATH_EXPR@262..263
                                PATH@262..263
                                  IDENT@262..263 "x"
                              WHITESPACE@263..264 " "
                              PLUS@264..265 "+"
                              WHITESPACE@265..266 " "
                              CALL@266..285
                                PATH@266..270
                                  IDENT@266..270 "sqrt"
                                ARG_LIST@270..285
                                  L_PAREN@270..271 "("
                                  BIN_EXPR@271..284
                                    BIN_EXPR@271..274
                                      PATH_EXPR@271..272
                                        PATH@271..272
                                          IDENT@271..272 "x"
                                      STAR@272..273 "*"
                                      PATH_EXPR@273..274
                                        PATH@273..274
                                          IDENT@273..274 "x"
                                    WHITESPACE@274..275 " "
                                    PLUS@275..276 "+"
                                    WHITESPACE@276..277 " "
                                    BIN_EXPR@277..284
                                      BIN_EXPR@277..282
                                        LITERAL@277..280
                                          STD_REAL_NUMBER@277..280 "4.0"
                                        STAR@280..281 "*"
                                        PATH_EXPR@281..282
                                          PATH@281..282
                                            IDENT@281..282 "c"
                                      STAR@282..283 "*"
                                      PATH_EXPR@283..284
                                        PATH@283..284
                                          IDENT@283..284 "c"
                                  R_PAREN@284..285 ")"
                            R_PAREN@285..286 ")"
                      SEMICOLON@286..287 ";"
                    WHITESPACE@287..300 "\n            "
                    END_KW@300..303 "end"
                  WHITESPACE@303..312 "\n        "
                  ENDFUNCTION_KW@312..323 "endfunction"
                WHITESPACE@323..324 "\n"
                ENDMODULE_KW@324..333 "endmodule"
              WHITESPACE@333..334 "\n"
        "#]],
    )
}

// #[test]
// fn statements() {
//     check(
//         r#"
// `define branches\
//     branch (A,x) ax;\
//     branch (A,y) ay;\
//     branch (x,B) xb;\
//     branch (y,B) yb;\
//     branch (x,y) xy;

// `define calculate(a,b)\
//     b**a/2;\
//     C = C**2;

// module schaltung (A,B);
//     inout electrical A,B;
//     electrical x,y;
//     `branches
//     real C = 30;

//     analog begin
//         if (V(ax) > 100) begin
//             if (V(ax) > 100) begin
//                 C=42*31+1;
//             end
//                         if (V(ax) > 100) begin
//                             C=42*31+1;
//                         end else             if (V(ax) > 100) begin
//                                                  C=42*31+1;
//                                              end else begin
//                                              end
//         end
//         I(xy) <+ (C*V(ax)+I(xy))/30;
//         // I(ax) <+ `calculate(31,V(ax))
//         I(ay) <+ V(ay)/20;
//         if (V(ax) > 100) begin
//             C=42*31+1;
//         end
//         I(ax) <+ C == 0 ? V(ax)/40 : 42;
//         $stprobe("foo",c,"bar");
//         ; // Empty statements are allowed by the standard
//         ;
//         I(ay) <+ C*V(ay)/50;
//     end
// endmodule
// "#,
//         expect![[r#"

// "#]],
//     )
// }
