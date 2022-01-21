use expect_test::expect;

use crate::tests::TestDataBase;

#[test]
pub fn unkown_lint() {
    let src = r#"

module test();
        (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar=0.0;
endmodule



"#;

    let db = TestDataBase::new("/root.va", src.to_owned().into());
    let (_, actual) = db.parse_and_check();

    let expected = expect![[r#"
        error[L008]: unknown lint 'bar'
          ┌─ /root.va:4:44
          │
        4 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar=0.0;
          │                                            ^^^^^ unknown lint
          │
          = help: this attribute has no effect
          = lint_not_found is set to deny by default

        error[L008]: unknown lint 'foo'
          ┌─ /root.va:4:25
          │
        4 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar=0.0;
          │                         ^^^^^ unknown lint
          │
          = help: this attribute has no effect
          = lint_not_found is set to deny by default

    "#]];
    expected.assert_eq(&actual);
}

#[test]
pub fn module_overwrite_unkown_lint() {
    let src = r#"

(*openvaf_allow="lint_not_found"*) module test();
        (*openvaf_allow="foo"*) parameter real bar=0.0;
endmodule

(*openvaf_warn="lint_not_found"*) module test();
        (*openvaf_allow="foo"*) parameter real bar=0.0;
endmodule

module test();
    (*openvaf_allow="foo",openvaf_allow="lint_not_found"*) parameter real bar=0.0;
endmodule
"#;

    let db = TestDataBase::new("/root.va", src.to_owned().into());
    let (_, actual) = db.parse_and_check();

    let expected = expect![[r#"
        warning[L008]: unknown lint 'foo'
          ┌─ /root.va:8:25
          │
        8 │         (*openvaf_allow="foo"*) parameter real bar=0.0;
          │                         ^^^^^ unknown lint
          │
          = help: this attribute has no effect

    "#]];
    expected.assert_eq(&actual);
}

#[test]
pub fn overwrite_reserved_ident() {
    let src = r#"

module cmos();
endmodule


(*openvaf_deny="vams_keyword_compat"*)module cmos();
endmodule
(*openvaf_allow="vams_keyword_compat"*)module cmos();
endmodule

"#;

    let db = TestDataBase::new("/root.va", src.to_owned().into());
    let (_, actual) = db.parse_and_check();

    let expected = expect![[r#"
        warning[L012]: reserved keyword 'cmos' was used as an identifier
          ┌─ /root.va:3:8
          │
        3 │ module cmos();
          │        ^^^^ 'cmos' is a keyword
          │
          = 'cmos' will likely never be used in the implemented language subset so this use is allowed
          = to maintain compatibility with the VAMS standard this should be renamed
          = vams_keyword_compat is set to warn by default

        error[L012]: reserved keyword 'cmos' was used as an identifier
          ┌─ /root.va:7:46
          │
        7 │ (*openvaf_deny="vams_keyword_compat"*)module cmos();
          │                                              ^^^^ 'cmos' is a keyword
          │
          = 'cmos' will likely never be used in the implemented language subset so this use is allowed
          = to maintain compatibility with the VAMS standard this should be renamed

    "#]];
    expected.assert_eq(&actual);
}

#[test]
pub fn reserved_ident_nested() {
    let src = r#"
 module foo();
        (*openvaf_allow="vams_keyword_compat"*) analog begin: cmos
            parameter real nmos = 0.0;
        end
endmodule

 (*openvaf_allow="vams_keyword_compat"*) module foo();
        parameter real nmos = 0.0;
        (*openvaf_deny="vams_keyword_compat"*) analog begin: bar
             parameter real nmos = 0.0;

            (*openvaf_allow="vams_keyword_compat"*) begin: bar
                parameter real nmos = 0.0;
            end
        end
endmodule



"#;

    let db = TestDataBase::new("/root.va", src.to_owned().into());
    let (_, actual) = db.parse_and_check();

    let expected = expect![[r#"
        error[L012]: reserved keyword 'nmos' was used as an identifier
           ┌─ /root.va:11:29
           │
        11 │              parameter real nmos = 0.0;
           │                             ^^^^ 'nmos' is a keyword
           │
           = 'nmos' will likely never be used in the implemented language subset so this use is allowed
           = to maintain compatibility with the VAMS standard this should be renamed

    "#]];
    expected.assert_eq(&actual);
}
