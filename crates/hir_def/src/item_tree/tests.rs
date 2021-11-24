use crate::{db::HirDefDB, tests::TestDataBase};
use basedb::BaseDB;
use expect_test::{expect, Expect};

fn check_db(db: TestDataBase, expect: Expect) {
    assert_eq!(db.parse(db.root_file()).errors(), &[]);
    let actual = db.lower_and_check();
    assert_eq!(actual, "");
    let actual = db.item_tree(db.root_file()).dump();
    expect.assert_eq(&actual);
}

fn check(src: &str, expect: Expect) {
    let db = TestDataBase::new("/root.va", src);
    check_db(db, expect)
}

#[test]
pub fn std() {
    let expect = expect![[r#"
        nature Current
            parent = None
            units = Some("A")
            ddt_nature = None
            idt_nature = Some(Name("Charge"))
            access = Some(Name("I"))
            units
            abstol
        nature Charge
            parent = None
            units = Some("coul")
            ddt_nature = Some(Name("Current"))
            idt_nature = None
            access = Some(Name("Q"))
            units
            abstol
        nature Voltage
            parent = None
            units = Some("V")
            ddt_nature = None
            idt_nature = Some(Name("Flux"))
            access = Some(Name("V"))
            units
            abstol
        nature Flux
            parent = None
            units = Some("Wb")
            ddt_nature = Some(Name("Voltage"))
            idt_nature = None
            access = Some(Name("Phi"))
            units
            abstol
        nature Magneto_Motive_Force
            parent = None
            units = Some("A*turn")
            ddt_nature = None
            idt_nature = None
            access = Some(Name("MMF"))
            units
            abstol
        nature Temperature
            parent = None
            units = Some("K")
            ddt_nature = None
            idt_nature = None
            access = Some(Name("Temp"))
            units
            abstol
        nature Power
            parent = None
            units = Some("W")
            ddt_nature = None
            idt_nature = None
            access = Some(Name("Pwr"))
            units
            abstol
        nature Position
            parent = None
            units = Some("m")
            ddt_nature = Some(Name("Velocity"))
            idt_nature = None
            access = Some(Name("Pos"))
            units
            abstol
        nature Velocity
            parent = None
            units = Some("m/s")
            ddt_nature = Some(Name("Acceleration"))
            idt_nature = Some(Name("Position"))
            access = Some(Name("Vel"))
            units
            abstol
        nature Acceleration
            parent = None
            units = Some("m/s^2")
            ddt_nature = Some(Name("Impulse"))
            idt_nature = Some(Name("Velocity"))
            access = Some(Name("Acc"))
            units
            abstol
        nature Impulse
            parent = None
            units = Some("m/s^3")
            ddt_nature = None
            idt_nature = Some(Name("Acceleration"))
            access = Some(Name("Imp"))
            units
            abstol
        nature Force
            parent = None
            units = Some("N")
            ddt_nature = None
            idt_nature = None
            access = Some(Name("F"))
            units
            abstol
        nature Angle
            parent = None
            units = Some("rads")
            ddt_nature = Some(Name("Angular_Velocity"))
            idt_nature = None
            access = Some(Name("Theta"))
            units
            abstol
        nature Angular_Velocity
            parent = None
            units = Some("rads/s")
            ddt_nature = Some(Name("Angular_Acceleration"))
            idt_nature = Some(Name("Angle"))
            access = Some(Name("Omega"))
            units
            abstol
        nature Angular_Acceleration
            parent = None
            units = Some("rads/s^2")
            ddt_nature = None
            idt_nature = Some(Name("Angular_Velocity"))
            access = Some(Name("Alpha"))
            units
            abstol
        nature Angular_Force
            parent = None
            units = Some("N*m")
            ddt_nature = None
            idt_nature = None
            access = Some(Name("Tau"))
            units
            abstol
        discipline logi

            potential = None
            flow = None
            domain = Some(Discrete)
        discipline ddiscrete

            potential = None
            flow = None
            domain = Some(Discrete)
        discipline electrical

            potential = Some(Name("Voltage"))
            flow = Some(Name("Current"))
            domain = None
        discipline voltage

            potential = Some(Name("Voltage"))
            flow = None
            domain = None
        discipline current

            potential = None
            flow = Some(Name("Current"))
            domain = None
        discipline magnetic

            potential = Some(Name("Magneto_Motive_Force"))
            flow = Some(Name("Flux"))
            domain = None
        discipline thermal

            potential = Some(Name("Temperature"))
            flow = Some(Name("Power"))
            domain = None
        discipline kinematic

            potential = Some(Name("Position"))
            flow = Some(Name("Force"))
            domain = None
        discipline kinematic_v

            potential = Some(Name("Velocity"))
            flow = Some(Name("Force"))
            domain = None
        discipline rotational

            potential = Some(Name("Angle"))
            flow = Some(Name("Angular_Force"))
            domain = None
        discipline rotational_omega

            potential = Some(Name("Angular_Velocity"))
            flow = Some(Name("Angular_Force"))
            domain = None"#]];
    check("`include \"disciplines.vams\"", expect)
}

#[test]
pub fn function() {
    let src = r#"
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
"#;

    let expect = expect![[r#"
        module test

            expected_ports = []
            function hypsmooth

                arg x = { is_input = true, is_output = false}
                arg c = { is_input = true, is_output = false}
                arg y = { is_input = false, is_output = true}
                arg z = { is_input = false, is_output = true}
                arg m = { is_input = true, is_output = true}
                arg l = { is_input = true, is_output = true}
                var real x
                var real c
                var integer m
                var integer z
                var string l
                var string y"#]];
    check(src, expect)
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
        module test

            expected_ports = []
            param real foo
            param real bar
            block Some(Name("test"))

                param real foo
                block Some(Name("test_nested"))

                    param real bar
            block Some(Name("test2"))

                var real bar"#]];
    check(src, expect)
}


