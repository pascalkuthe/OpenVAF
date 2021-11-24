use expect_test::{expect, Expect};
use syntax::AstNode;

use crate::tests::TestDataBase;

fn check(src: &str, expected: Expect) {
    let parse = TestDataBase::new("/root.va", src).parse_and_check();
    let actual = format!("{:#?}", parse.tree().syntax());
    expected.assert_eq(&actual)
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
                          FUNCTION_REF@39..40
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
                                  FUNCTION_REF@46..47
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
    inout wire c;
    inout electrical d;
    output electrical e,f;
    inout electrical wire g;
endmodule

module test3 (output a,b, input electrical c,d,e, (*foo*) inout electrical wire f);
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..286
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
              MODULE_DECL@26..190
                MODULE_KW@26..32 "module"
                WHITESPACE@32..33 " "
                NAME@33..38
                  IDENT@33..38 "test2"
                WHITESPACE@38..39 " "
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
                BODY_PORT_DECL@87..100
                  PORT_DECL@87..99
                    DIRECTION@87..92
                      INOUT_KW@87..92 "inout"
                    WHITESPACE@92..93 " "
                    NET_TYPE@93..97 "wire"
                    WHITESPACE@97..98 " "
                    NAME@98..99
                      IDENT@98..99 "c"
                  SEMICOLON@99..100 ";"
                WHITESPACE@100..105 "\n    "
                BODY_PORT_DECL@105..124
                  PORT_DECL@105..123
                    DIRECTION@105..110
                      INOUT_KW@105..110 "inout"
                    WHITESPACE@110..111 " "
                    NAME_REF@111..121
                      IDENT@111..121 "electrical"
                    WHITESPACE@121..122 " "
                    NAME@122..123
                      IDENT@122..123 "d"
                  SEMICOLON@123..124 ";"
                WHITESPACE@124..129 "\n    "
                BODY_PORT_DECL@129..151
                  PORT_DECL@129..150
                    DIRECTION@129..135
                      OUTPUT_KW@129..135 "output"
                    WHITESPACE@135..136 " "
                    NAME_REF@136..146
                      IDENT@136..146 "electrical"
                    WHITESPACE@146..147 " "
                    NAME@147..148
                      IDENT@147..148 "e"
                    COMMA@148..149 ","
                    NAME@149..150
                      IDENT@149..150 "f"
                  SEMICOLON@150..151 ";"
                WHITESPACE@151..156 "\n    "
                BODY_PORT_DECL@156..180
                  PORT_DECL@156..179
                    DIRECTION@156..161
                      INOUT_KW@156..161 "inout"
                    WHITESPACE@161..162 " "
                    NAME_REF@162..172
                      IDENT@162..172 "electrical"
                    WHITESPACE@172..173 " "
                    NET_TYPE@173..177 "wire"
                    WHITESPACE@177..178 " "
                    NAME@178..179
                      IDENT@178..179 "g"
                  SEMICOLON@179..180 ";"
                WHITESPACE@180..181 "\n"
                ENDMODULE_KW@181..190 "endmodule"
              WHITESPACE@190..192 "\n\n"
              MODULE_DECL@192..285
                MODULE_KW@192..198 "module"
                WHITESPACE@198..199 " "
                NAME@199..204
                  IDENT@199..204 "test3"
                WHITESPACE@204..205 " "
                L_PAREN@205..206 "("
                MODULE_PORT@206..216
                  PORT_DECL@206..216
                    DIRECTION@206..212
                      OUTPUT_KW@206..212 "output"
                    WHITESPACE@212..213 " "
                    NAME@213..214
                      IDENT@213..214 "a"
                    COMMA@214..215 ","
                    NAME@215..216
                      IDENT@215..216 "b"
                COMMA@216..217 ","
                WHITESPACE@217..218 " "
                MODULE_PORT@218..240
                  PORT_DECL@218..240
                    DIRECTION@218..223
                      INPUT_KW@218..223 "input"
                    WHITESPACE@223..224 " "
                    NAME_REF@224..234
                      IDENT@224..234 "electrical"
                    WHITESPACE@234..235 " "
                    NAME@235..236
                      IDENT@235..236 "c"
                    COMMA@236..237 ","
                    NAME@237..238
                      IDENT@237..238 "d"
                    COMMA@238..239 ","
                    NAME@239..240
                      IDENT@239..240 "e"
                COMMA@240..241 ","
                WHITESPACE@241..242 " "
                MODULE_PORT@242..273
                  PORT_DECL@242..273
                    ATTR_LIST@242..249
                      L_ATTR_PAREN@242..244 "(*"
                      ATTR@244..247
                        NAME@244..247
                          IDENT@244..247 "foo"
                      R_ATTR_PAREN@247..249 "*)"
                    WHITESPACE@249..250 " "
                    DIRECTION@250..255
                      INOUT_KW@250..255 "inout"
                    WHITESPACE@255..256 " "
                    NAME_REF@256..266
                      IDENT@256..266 "electrical"
                    WHITESPACE@266..267 " "
                    NET_TYPE@267..271 "wire"
                    WHITESPACE@271..272 " "
                    NAME@272..273
                      IDENT@272..273 "f"
                R_PAREN@273..274 ")"
                SEMICOLON@274..275 ";"
                WHITESPACE@275..276 "\n"
                ENDMODULE_KW@276..285 "endmodule"
              WHITESPACE@285..286 "\n"
        "#]],
    )
}

#[test]
fn net_decl() {
    check(
        r#"
module test ();
    wire x,y;
    electrical z;
    wire electrical l;

endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..83
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..82
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                WHITESPACE@12..13 " "
                L_PAREN@13..14 "("
                R_PAREN@14..15 ")"
                SEMICOLON@15..16 ";"
                WHITESPACE@16..21 "\n    "
                NET_DECL@21..30
                  NET_TYPE@21..25 "wire"
                  WHITESPACE@25..26 " "
                  NAME@26..27
                    IDENT@26..27 "x"
                  COMMA@27..28 ","
                  NAME@28..29
                    IDENT@28..29 "y"
                  SEMICOLON@29..30 ";"
                WHITESPACE@30..35 "\n    "
                NET_DECL@35..48
                  NAME@35..45
                    IDENT@35..45 "electrical"
                  WHITESPACE@45..46 " "
                  NAME@46..47
                    IDENT@46..47 "z"
                  SEMICOLON@47..48 ";"
                WHITESPACE@48..53 "\n    "
                NET_DECL@53..71
                  NET_TYPE@53..57 "wire"
                  WHITESPACE@57..58 " "
                  NAME_REF@58..68
                    IDENT@58..68 "electrical"
                  WHITESPACE@68..69 " "
                  NAME@69..70
                    IDENT@69..70 "l"
                  SEMICOLON@70..71 ";"
                WHITESPACE@71..73 "\n\n"
                ENDMODULE_KW@73..82 "endmodule"
              WHITESPACE@82..83 "\n"
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
    time t;
    realtime rt;
    analog x = 2==y;
endmodule
"#,
        expect![[r#"
            SOURCE_FILE@0..112
              WHITESPACE@0..1 "\n"
              MODULE_DECL@1..111
                MODULE_KW@1..7 "module"
                WHITESPACE@7..8 " "
                NAME@8..12
                  IDENT@8..12 "test"
                WHITESPACE@12..13 " "
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
                NET_DECL@56..63
                  NAME@56..60
                    IDENT@56..60 "time"
                  WHITESPACE@60..61 " "
                  NAME@61..62
                    IDENT@61..62 "t"
                  SEMICOLON@62..63 ";"
                WHITESPACE@63..68 "\n    "
                NET_DECL@68..80
                  NAME@68..76
                    IDENT@68..76 "realtime"
                  WHITESPACE@76..77 " "
                  NAME@77..79
                    IDENT@77..79 "rt"
                  SEMICOLON@79..80 ";"
                WHITESPACE@80..85 "\n    "
                ANALOG_BEHAVIOUR@85..101
                  ANALOG_KW@85..91 "analog"
                  WHITESPACE@91..92 " "
                  ASSIGN_STMT@92..101
                    ASSIGN@92..100
                      PATH_EXPR@92..93
                        PATH@92..93
                          IDENT@92..93 "x"
                      WHITESPACE@93..94 " "
                      EQ@94..95 "="
                      WHITESPACE@95..96 " "
                      BIN_EXPR@96..100
                        LITERAL@96..97
                          INT_NUMBER@96..97 "2"
                        EQ2@97..99 "=="
                        PATH_EXPR@99..100
                          PATH@99..100
                            IDENT@99..100 "y"
                    SEMICOLON@100..101 ";"
                WHITESPACE@101..102 "\n"
                ENDMODULE_KW@102..111 "endmodule"
              WHITESPACE@111..112 "\n"
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
                                FUNCTION_REF@266..270
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
