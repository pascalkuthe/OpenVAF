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
            units = Some(("A", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = Some((NatureRef { name: Name("Charge"), kind: Nature }, Idx::<NatureAttrData>(2)))
            access = Some((Name("I"), Idx::<NatureAttrData>(1)))
            attr0: units
            attr1: access
            attr2: idt_nature
            attr3: abstol
        nature Charge
            parent = None
            units = Some(("coul", Idx::<NatureAttrData>(0)))
            ddt_nature = Some((NatureRef { name: Name("Current"), kind: Nature }, Idx::<NatureAttrData>(2)))
            idt_nature = None
            access = Some((Name("Q"), Idx::<NatureAttrData>(1)))
            attr4: units
            attr5: access
            attr6: ddt_nature
            attr7: abstol
        nature Voltage
            parent = None
            units = Some(("V", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = Some((NatureRef { name: Name("Flux"), kind: Nature }, Idx::<NatureAttrData>(2)))
            access = Some((Name("V"), Idx::<NatureAttrData>(1)))
            attr8: units
            attr9: access
            attr10: idt_nature
            attr11: abstol
        nature Flux
            parent = None
            units = Some(("Wb", Idx::<NatureAttrData>(0)))
            ddt_nature = Some((NatureRef { name: Name("Voltage"), kind: Nature }, Idx::<NatureAttrData>(2)))
            idt_nature = None
            access = Some((Name("Phi"), Idx::<NatureAttrData>(1)))
            attr12: units
            attr13: access
            attr14: ddt_nature
            attr15: abstol
        nature Magneto_Motive_Force
            parent = None
            units = Some(("A*turn", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = None
            access = Some((Name("MMF"), Idx::<NatureAttrData>(1)))
            attr16: units
            attr17: access
            attr18: abstol
        nature Temperature
            parent = None
            units = Some(("K", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = None
            access = Some((Name("Temp"), Idx::<NatureAttrData>(1)))
            attr19: units
            attr20: access
            attr21: abstol
        nature Power
            parent = None
            units = Some(("W", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = None
            access = Some((Name("Pwr"), Idx::<NatureAttrData>(1)))
            attr22: units
            attr23: access
            attr24: abstol
        nature Position
            parent = None
            units = Some(("m", Idx::<NatureAttrData>(0)))
            ddt_nature = Some((NatureRef { name: Name("Velocity"), kind: Nature }, Idx::<NatureAttrData>(2)))
            idt_nature = None
            access = Some((Name("Pos"), Idx::<NatureAttrData>(1)))
            attr25: units
            attr26: access
            attr27: ddt_nature
            attr28: abstol
        nature Velocity
            parent = None
            units = Some(("m/s", Idx::<NatureAttrData>(0)))
            ddt_nature = Some((NatureRef { name: Name("Acceleration"), kind: Nature }, Idx::<NatureAttrData>(2)))
            idt_nature = Some((NatureRef { name: Name("Position"), kind: Nature }, Idx::<NatureAttrData>(3)))
            access = Some((Name("Vel"), Idx::<NatureAttrData>(1)))
            attr29: units
            attr30: access
            attr31: ddt_nature
            attr32: idt_nature
            attr33: abstol
        nature Acceleration
            parent = None
            units = Some(("m/s^2", Idx::<NatureAttrData>(0)))
            ddt_nature = Some((NatureRef { name: Name("Impulse"), kind: Nature }, Idx::<NatureAttrData>(2)))
            idt_nature = Some((NatureRef { name: Name("Velocity"), kind: Nature }, Idx::<NatureAttrData>(3)))
            access = Some((Name("Acc"), Idx::<NatureAttrData>(1)))
            attr34: units
            attr35: access
            attr36: ddt_nature
            attr37: idt_nature
            attr38: abstol
        nature Impulse
            parent = None
            units = Some(("m/s^3", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = Some((NatureRef { name: Name("Acceleration"), kind: Nature }, Idx::<NatureAttrData>(2)))
            access = Some((Name("Imp"), Idx::<NatureAttrData>(1)))
            attr39: units
            attr40: access
            attr41: idt_nature
            attr42: abstol
        nature Force
            parent = None
            units = Some(("N", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = None
            access = Some((Name("F"), Idx::<NatureAttrData>(1)))
            attr43: units
            attr44: access
            attr45: abstol
        nature Angle
            parent = None
            units = Some(("rads", Idx::<NatureAttrData>(0)))
            ddt_nature = Some((NatureRef { name: Name("Angular_Velocity"), kind: Nature }, Idx::<NatureAttrData>(2)))
            idt_nature = None
            access = Some((Name("Theta"), Idx::<NatureAttrData>(1)))
            attr46: units
            attr47: access
            attr48: ddt_nature
            attr49: abstol
        nature Angular_Velocity
            parent = None
            units = Some(("rads/s", Idx::<NatureAttrData>(0)))
            ddt_nature = Some((NatureRef { name: Name("Angular_Acceleration"), kind: Nature }, Idx::<NatureAttrData>(2)))
            idt_nature = Some((NatureRef { name: Name("Angle"), kind: Nature }, Idx::<NatureAttrData>(3)))
            access = Some((Name("Omega"), Idx::<NatureAttrData>(1)))
            attr50: units
            attr51: access
            attr52: ddt_nature
            attr53: idt_nature
            attr54: abstol
        nature Angular_Acceleration
            parent = None
            units = Some(("rads/s^2", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = Some((NatureRef { name: Name("Angular_Velocity"), kind: Nature }, Idx::<NatureAttrData>(2)))
            access = Some((Name("Alpha"), Idx::<NatureAttrData>(1)))
            attr55: units
            attr56: access
            attr57: idt_nature
            attr58: abstol
        nature Angular_Force
            parent = None
            units = Some(("N*m", Idx::<NatureAttrData>(0)))
            ddt_nature = None
            idt_nature = None
            access = Some((Name("Tau"), Idx::<NatureAttrData>(1)))
            attr59: units
            attr60: access
            attr61: abstol
        discipline logi

            potential = None
            flow = None
            domain = Some((Discrete, Idx::<DisciplineAttrData>(0)))
            attr0: domain (UserDefined)
        discipline ddiscrete

            potential = None
            flow = None
            domain = Some((Discrete, Idx::<DisciplineAttrData>(0)))
            attr1: domain (UserDefined)
        discipline electrical

            potential = Some((NatureRef { name: Name("Voltage"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = Some((NatureRef { name: Name("Current"), kind: Nature }, Idx::<DisciplineAttrData>(1)))
            domain = None
            attr2: potential (UserDefined)
            attr3: flow (UserDefined)
        discipline voltage

            potential = Some((NatureRef { name: Name("Voltage"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = None
            domain = None
            attr4: potential (UserDefined)
        discipline current

            potential = None
            flow = Some((NatureRef { name: Name("Current"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            domain = None
            attr5: flow (UserDefined)
        discipline magnetic

            potential = Some((NatureRef { name: Name("Magneto_Motive_Force"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = Some((NatureRef { name: Name("Flux"), kind: Nature }, Idx::<DisciplineAttrData>(1)))
            domain = None
            attr6: potential (UserDefined)
            attr7: flow (UserDefined)
        discipline thermal

            potential = Some((NatureRef { name: Name("Temperature"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = Some((NatureRef { name: Name("Power"), kind: Nature }, Idx::<DisciplineAttrData>(1)))
            domain = None
            attr8: potential (UserDefined)
            attr9: flow (UserDefined)
        discipline kinematic

            potential = Some((NatureRef { name: Name("Position"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = Some((NatureRef { name: Name("Force"), kind: Nature }, Idx::<DisciplineAttrData>(1)))
            domain = None
            attr10: potential (UserDefined)
            attr11: flow (UserDefined)
        discipline kinematic_v

            potential = Some((NatureRef { name: Name("Velocity"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = Some((NatureRef { name: Name("Force"), kind: Nature }, Idx::<DisciplineAttrData>(1)))
            domain = None
            attr12: potential (UserDefined)
            attr13: flow (UserDefined)
        discipline rotational

            potential = Some((NatureRef { name: Name("Angle"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = Some((NatureRef { name: Name("Angular_Force"), kind: Nature }, Idx::<DisciplineAttrData>(1)))
            domain = None
            attr14: potential (UserDefined)
            attr15: flow (UserDefined)
        discipline rotational_omega

            potential = Some((NatureRef { name: Name("Angular_Velocity"), kind: Nature }, Idx::<DisciplineAttrData>(0)))
            flow = Some((NatureRef { name: Name("Angular_Force"), kind: Nature }, Idx::<DisciplineAttrData>(1)))
            domain = None
            attr16: potential (UserDefined)
            attr17: flow (UserDefined)"#]];
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

            function hypsmooth

                arg Real x = { is_input = true, is_output = false}
                arg Real c = { is_input = true, is_output = false}
                arg String y = { is_input = false, is_output = true}
                arg Integer z = { is_input = false, is_output = true}
                arg Integer m = { is_input = true, is_output = true}
                arg String l = { is_input = true, is_output = true}"#]];
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

#[test]
pub fn nodes() {
    let src = r#"
module test(foo,test);
    electrical foo;
    inout foo;
    electrical bar;
    gnd bar;
    inout electrical test;
endmodule
"#;

    let expect = expect![[r#"
        module test

            node foo = {is_input: true, is_output:true, gnd: false , discipline Some(Name("electrical"))}
            node test = {is_input: true, is_output:true, gnd: false , discipline Some(Name("electrical"))}
            node bar = {is_input: false, is_output:false, gnd: false , discipline Some(Name("electrical"))}"#]];
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
        module test

            node a = {is_input: true, is_output:true, gnd: false , discipline Some(Name("electrical"))}
            node c = {is_input: false, is_output:false, gnd: false , discipline Some(Name("electrical"))}
            branch br_ac = Nodes(a, c)
            branch br_a_port = PortFlow(a)"#]];
    check(src, expect)
}
