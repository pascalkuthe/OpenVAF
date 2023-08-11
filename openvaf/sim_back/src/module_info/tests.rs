use hir::diagnostics::sink::Buffer;
use hir::diagnostics::ConsoleSink;
use hir::CompilationDB;
use indoc::indoc;

#[test]
fn invalid_attr() {
    let src = indoc! {r#"
        module test;
            (* units=1, desc=xx, group=foo*bar, type=2  *) parameter real foo=2.0, bar=3.0;
            (* type="foo"  *) parameter real test=1.0;
            (* units=1, desc=xx *) real init;
            aliasparam alias=foo;
        endmodule
    "#};
    let db = CompilationDB::new_virtual(src).unwrap();
    let mut buf = Buffer::no_color();
    {
        let mut sink = ConsoleSink::buffer(&db, &mut buf);
        sink.annonymize_paths();
        super::collect_modules(&db, false, &mut sink);
    }
    expect_test::expect![[r#"
        error: illegal expression supplied to 'units' attribute; expected a string literal
          --> /root.va:2:8
          |
        2 |     (* units=1, desc=xx, group=foo*bar, type=2  *) parameter real foo=2.0, bar=3.0;
          |        ^^^^^^^ expected a string literal

        error: illegal expression supplied to 'desc' attribute; expected a string literal
          --> /root.va:2:17
          |
        2 |     (* units=1, desc=xx, group=foo*bar, type=2  *) parameter real foo=2.0, bar=3.0;
          |                 ^^^^^^^ expected a string literal

        error: illegal expression supplied to 'group' attribute; expected a string literal
          --> /root.va:2:26
          |
        2 |     (* units=1, desc=xx, group=foo*bar, type=2  *) parameter real foo=2.0, bar=3.0;
          |                          ^^^^^^^^^^^^^ expected a string literal

        error: illegal expression supplied to 'type' attribute; expected a string literal
          --> /root.va:2:41
          |
        2 |     (* units=1, desc=xx, group=foo*bar, type=2  *) parameter real foo=2.0, bar=3.0;
          |                                         ^^^^^^ expected a string literal

        warning: unknown type "foo" expected "model" or "instance"
          --> /root.va:3:8
          |
        3 |     (* type="foo"  *) parameter real test=1.0;
          |        ^^^^^^^^^^ unknown type

        error: illegal expression supplied to 'units' attribute; expected a string literal
          --> /root.va:4:8
          |
        4 |     (* units=1, desc=xx *) real init;
          |        ^^^^^^^ expected a string literal

        error: illegal expression supplied to 'desc' attribute; expected a string literal
          --> /root.va:4:17
          |
        4 |     (* units=1, desc=xx *) real init;
          |                 ^^^^^^^ expected a string literal

        error: could not compile `root.va` due to 6 previous errors; 1 warning emitted

    "#]]
    .assert_eq(&String::from_utf8(buf.into_inner()).unwrap());
}

#[test]
fn parameters() {
    let src = indoc! {r#"
        module test;
            (* units="m", desc="hmm", group="foo", type="instance" *) parameter real foo=2.0, bar=3.0;
            aliasparam alias=foo;
            (* type="model" *) parameter real module_param=3.0;
        endmodule
    "#};
    let db = CompilationDB::new_virtual(src).unwrap();
    let modules = super::collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap();
    assert_eq!(modules.len(), 1);
    let params: Vec<_> = modules[0].params.iter().map(|(k, v)| (k.name(&db), v)).collect();
    expect_test::expect![[r#"
        [
            (
                "foo",
                ParamInfo {
                    name: "foo",
                    alias: [
                        "alias",
                    ],
                    unit: "m",
                    description: "hmm",
                    group: "foo",
                    is_instance: true,
                },
            ),
            (
                "bar",
                ParamInfo {
                    name: "bar",
                    alias: [],
                    unit: "m",
                    description: "hmm",
                    group: "foo",
                    is_instance: true,
                },
            ),
            (
                "module_param",
                ParamInfo {
                    name: "module_param",
                    alias: [],
                    unit: "",
                    description: "",
                    group: "",
                    is_instance: false,
                },
            ),
        ]
    "#]]
    .assert_debug_eq(&params);
}

#[test]
fn opvars() {
    let src = indoc! {r#"
        module test;
            (* units="m", desc="hmm" *) real both1, both2=3.0;
            (* units="m" *) real units_;
            (* desc="hmm" *) real desc_;
        endmodule
    "#};
    let db = CompilationDB::new_virtual(src).unwrap();
    let modules = super::collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap();
    assert_eq!(modules.len(), 1);
    let params: Vec<_> = modules[0].op_vars.iter().map(|(k, v)| (k.name(&db), v)).collect();
    expect_test::expect![[r#"
        [
            (
                "both1",
                OpVar {
                    unit: "m",
                    description: "hmm",
                },
            ),
            (
                "both2",
                OpVar {
                    unit: "m",
                    description: "hmm",
                },
            ),
            (
                "units_",
                OpVar {
                    unit: "m",
                    description: "",
                },
            ),
            (
                "desc_",
                OpVar {
                    unit: "",
                    description: "hmm",
                },
            ),
        ]
    "#]]
    .assert_debug_eq(&params);
}
