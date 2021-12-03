use basedb::BaseDB;
use expect_test::{expect, Expect};

use crate::{db::HirDefDB, tests::TestDataBase, FunctionId};

fn check_db(db: TestDataBase, expect: Expect) {
    assert_eq!(db.parse(db.root_file()).errors(), &[]);
    let actual = db.lower_and_check();
    assert_eq!(actual, "");
    let actual = db.def_map(db.root_file()).dump(&db);
    expect.assert_eq(&actual);
}

fn check(src: &str, expect: Expect) {
    let db = TestDataBase::new("/root.va", src);
    check_db(db, expect)
}

#[test]
pub fn function() {
    let src = r#"
module test();

        analog function real hypsmooth;
            input x , c;
            real x , c;
            begin
                hypsmooth = 0.5 * (x + sqrt(x*x + 4.0*c*c));
            end
        endfunction

        analog function real test;
            input x;
            input y;
            real x,y;
            test = x*y;
        endfunction
endmodule
"#;

    let expect = expect![[r#"
        test = module;

            hypsmooth = function;

                c = function argument;
                hypsmooth = variable;
                x = function argument;
            test = function;

                test = variable;
                x = function argument;
                y = function argument;
    "#]];
    check(src, expect)
}

fn check_db_fun(db: TestDataBase, expect: Expect) {
    assert_eq!(db.parse(db.root_file()).errors(), &[]);
    let actual = db.lower_and_check();
    assert_eq!(actual, "");
    let actual = db.function_def_map(FunctionId(0u32.into())).dump(&db);
    expect.assert_eq(&actual);
}

fn check_fun(src: &str, expect: Expect) {
    let db = TestDataBase::new("/root.va", src);
    check_db_fun(db, expect)
}

#[test]
pub fn function_source_map() {
    let src = r#"
module bar;
endmodule
module test;
        parameter real foo=0.0;
        real bar;
        analog function real test;
            input x;
            input y;
            real x,y;
            test = x*y;
        endfunction

        analog function real hypsmooth;
            input x , c;
            real x , c;
            begin
                hypsmooth = 0.5 * (x + sqrt(x*x + 4.0*c*c));
            end
        endfunction
endmodule
"#;

    let expect = expect![[r#"
        bar = module;
        test = module;

            foo = parameter;
            hypsmooth = function;

                c = function argument;
                hypsmooth = variable;
                x = function argument;
            test = function;

                test = variable;
                x = function argument;
                y = function argument;
    "#]];
    check_fun(src, expect)
}

#[test]
pub fn blocks() {
    let src = r#"
module test();
        parameter real foo = 0.0;
        parameter real bar = foo;
        analog begin: test
            parameter real foo = 1.0;
            begin
                begin: test_nested
                    parameter real bar = 0.0;
                end
            end
        end

        analog begin: test2
            real bar;
        end
endmodule
"#;

    let expect = expect![[r#"
        test = module;

            bar = parameter;
            foo = parameter;
            test = block scope;

                foo = parameter;
                test_nested = block scope;

                    bar = parameter;
            test2 = block scope;

                bar = variable;
    "#]];
    check(src, expect)
}

#[test]
pub fn branches() {
    let src = r#"
module test(a);
    electrical a;
    inout a;
    electrical c;
    branch (a,c) br_ac;
    branch (<a>) br_a_port;

endmodule
"#;

    let expect = expect![[r#"
        test = module;

            a = node;
            br_a_port = branch;
            br_ac = branch;
            c = node;
    "#]];
    check(src, expect)
}

#[test]
pub fn std() {
    let expect = expect![[r#"
        Acc = nature access function;
        Acceleration = nature;
        Alpha = nature access function;
        Angle = nature;
        Angular_Acceleration = nature;
        Angular_Force = nature;
        Angular_Velocity = nature;
        Charge = nature;
        Current = nature;
        F = nature access function;
        Flux = nature;
        Force = nature;
        I = nature access function;
        Imp = nature access function;
        Impulse = nature;
        MMF = nature access function;
        Magneto_Motive_Force = nature;
        Omega = nature access function;
        Phi = nature access function;
        Pos = nature access function;
        Position = nature;
        Power = nature;
        Pwr = nature access function;
        Q = nature access function;
        Tau = nature access function;
        Temp = nature access function;
        Temperature = nature;
        Theta = nature access function;
        V = nature access function;
        Vel = nature access function;
        Velocity = nature;
        Voltage = nature;
        current = discipline;
        ddiscrete = discipline;
        electrical = discipline;
        kinematic = discipline;
        kinematic_v = discipline;
        logi = discipline;
        magnetic = discipline;
        rotational = discipline;
        rotational_omega = discipline;
        thermal = discipline;
        voltage = discipline;

    "#]];
    check("`include \"disciplines.vams\"", expect)
}
