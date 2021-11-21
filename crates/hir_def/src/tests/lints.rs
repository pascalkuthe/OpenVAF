use expect_test::expect;

use super::TestDataBase;

#[test]
pub fn item_tree_flat() {
    let src = r#"
module test();
        (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
endmodule

(*openvaf_warn="lint_not_found"*) module warn();
        
        (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
endmodule

(*openvaf_allow="lint_not_found"*) module bar();
        (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
endmodule
"#;

    let db = TestDataBase::new("/root.va", src);
    let actual = db.lower_and_check();

    let expected = expect![[r#"
        error[L008]: unknown lint 'foo'
          ┌─ /root.va:3:25
          │
        3 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                         ^^^^^ unknown lint
          │
          = help: this attribute has no effect
          = lint_not_found is set to deny by default

        error[L008]: unknown lint 'bar'
          ┌─ /root.va:3:44
          │
        3 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                                            ^^^^^ unknown lint
          │
          = help: this attribute has no effect
          = lint_not_found is set to deny by default

        warning[L008]: unknown lint 'foo'
          ┌─ /root.va:8:25
          │
        8 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                         ^^^^^ unknown lint
          │
          = help: this attribute has no effect

        warning[L008]: unknown lint 'bar'
          ┌─ /root.va:8:44
          │
        8 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                                            ^^^^^ unknown lint
          │
          = help: this attribute has no effect

    "#]];
    expected.assert_eq(&actual);
}

#[test]
pub fn item_tree_nested() {
    let src = r#"
(*openvaf_warn="lint_not_found"*) module test();
        (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
        analog begin: foo
            (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real other;
        end
endmodule
module foo();
        (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
       
        analog 
            (*openvaf_warn="lint_not_found"*) begin
                (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real other;
            end

        (*openvaf_allow="lint_not_found"*) analog begin
                (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real other;
        end
endmodule
"#;

    let db = TestDataBase::new("/root.va", src);
    let actual = db.lower_and_check();

    let expected = expect![[r#"
        warning[L008]: unknown lint 'foo'
          ┌─ /root.va:3:25
          │
        3 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                         ^^^^^ unknown lint
          │
          = help: this attribute has no effect

        warning[L008]: unknown lint 'bar'
          ┌─ /root.va:3:44
          │
        3 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                                            ^^^^^ unknown lint
          │
          = help: this attribute has no effect

        warning[L008]: unknown lint 'foo'
          ┌─ /root.va:5:29
          │
        5 │             (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real other;
          │                             ^^^^^ unknown lint
          │
          = help: this attribute has no effect

        warning[L008]: unknown lint 'bar'
          ┌─ /root.va:5:48
          │
        5 │             (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real other;
          │                                                ^^^^^ unknown lint
          │
          = help: this attribute has no effect

        error[L008]: unknown lint 'foo'
          ┌─ /root.va:9:25
          │
        9 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                         ^^^^^ unknown lint
          │
          = help: this attribute has no effect
          = lint_not_found is set to deny by default

        error[L008]: unknown lint 'bar'
          ┌─ /root.va:9:44
          │
        9 │         (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real bar;
          │                                            ^^^^^ unknown lint
          │
          = help: this attribute has no effect
          = lint_not_found is set to deny by default

        warning[L008]: unknown lint 'foo'
           ┌─ /root.va:13:33
           │
        13 │                 (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real other;
           │                                 ^^^^^ unknown lint
           │
           = help: this attribute has no effect

        warning[L008]: unknown lint 'bar'
           ┌─ /root.va:13:52
           │
        13 │                 (*openvaf_allow="foo",openvaf_warn="bar"*) parameter real other;
           │                                                    ^^^^^ unknown lint
           │
           = help: this attribute has no effect

    "#]];
    expected.assert_eq(&actual);
}


#[test]
pub fn body() {
    let src = r#"
module foo();
        var int x;
        analog 
            (*openvaf_warn="lint_not_found"*) begin: foo
                (*openvaf_allow="foo"*) x = 0;
                (*openvaf_allow="lint_not_found"*) begin: bar 
                    (*openvaf_allow="foo"*) x = 0;
                end
            end
endmodule
"#;

    let db = TestDataBase::new("/root.va", src);
    let actual = db.lower_and_check();

    let expected = expect![[r#"
        warning[L008]: unknown lint 'foo'
          ┌─ /root.va:6:33
          │
        6 │                 (*openvaf_allow="foo"*) x = 0;
          │                                 ^^^^^ unknown lint
          │
          = help: this attribute has no effect

    "#]];
    expected.assert_eq(&actual);
}


// TODO document that lint attributes only apply recrusively for items
