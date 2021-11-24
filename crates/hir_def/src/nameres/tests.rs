use basedb::BaseDB;
use expect_test::{expect, Expect};

use crate::{db::HirDefDB, tests::TestDataBase};

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
endmodule
"#;

    let expect = expect![[r#"
        test = ModuleId(ModuleId(0));

            hypsmooth = FunctionId(FunctionId(0));

                c = FunctionArgId(FunctionArgId(1));
                hypsmooth = FunctionReturn(FunctionId(0));
                x = FunctionArgId(FunctionArgId(0));
    "#]];
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
        test = ModuleId(ModuleId(0));

            bar = ParamId(ParamId(1));
            foo = ParamId(ParamId(0));
            test = BlockId(BlockId(0));

                foo = ParamId(ParamId(2));
                test_nested = BlockId(BlockId(2));

                    bar = ParamId(ParamId(3));
            test2 = BlockId(BlockId(1));

                bar = VarId(VarId(0));
    "#]];
    check(src, expect)
}

#[test]
pub fn std() {

    let expect = expect![[r#"
        Acc = NatureAccess(NatureAccess(NatureId(9)));
        Acceleration = NatureId(NatureId(9));

            abstol = NatureAttrId(NatureAttrId(19));
            ddt_nature = NatureId(NatureId(10));
            idt_nature = NatureId(NatureId(8));
            units = NatureAttrId(NatureAttrId(18));
        Alpha = NatureAccess(NatureAccess(NatureId(14)));
        Angle = NatureId(NatureId(12));

            abstol = NatureAttrId(NatureAttrId(25));
            ddt_nature = NatureId(NatureId(13));
            units = NatureAttrId(NatureAttrId(24));
        Angular_Acceleration = NatureId(NatureId(14));

            abstol = NatureAttrId(NatureAttrId(29));
            idt_nature = NatureId(NatureId(13));
            units = NatureAttrId(NatureAttrId(28));
        Angular_Force = NatureId(NatureId(15));

            abstol = NatureAttrId(NatureAttrId(31));
            units = NatureAttrId(NatureAttrId(30));
        Angular_Velocity = NatureId(NatureId(13));

            abstol = NatureAttrId(NatureAttrId(27));
            ddt_nature = NatureId(NatureId(14));
            idt_nature = NatureId(NatureId(12));
            units = NatureAttrId(NatureAttrId(26));
        Charge = NatureId(NatureId(1));

            abstol = NatureAttrId(NatureAttrId(3));
            ddt_nature = NatureId(NatureId(0));
            units = NatureAttrId(NatureAttrId(2));
        Current = NatureId(NatureId(0));

            abstol = NatureAttrId(NatureAttrId(1));
            idt_nature = NatureId(NatureId(1));
            units = NatureAttrId(NatureAttrId(0));
        F = NatureAccess(NatureAccess(NatureId(11)));
        Flux = NatureId(NatureId(3));

            abstol = NatureAttrId(NatureAttrId(7));
            ddt_nature = NatureId(NatureId(2));
            units = NatureAttrId(NatureAttrId(6));
        Force = NatureId(NatureId(11));

            abstol = NatureAttrId(NatureAttrId(23));
            units = NatureAttrId(NatureAttrId(22));
        I = NatureAccess(NatureAccess(NatureId(0)));
        Imp = NatureAccess(NatureAccess(NatureId(10)));
        Impulse = NatureId(NatureId(10));

            abstol = NatureAttrId(NatureAttrId(21));
            idt_nature = NatureId(NatureId(9));
            units = NatureAttrId(NatureAttrId(20));
        MMF = NatureAccess(NatureAccess(NatureId(4)));
        Magneto_Motive_Force = NatureId(NatureId(4));

            abstol = NatureAttrId(NatureAttrId(9));
            units = NatureAttrId(NatureAttrId(8));
        Omega = NatureAccess(NatureAccess(NatureId(13)));
        Phi = NatureAccess(NatureAccess(NatureId(3)));
        Pos = NatureAccess(NatureAccess(NatureId(7)));
        Position = NatureId(NatureId(7));

            abstol = NatureAttrId(NatureAttrId(15));
            ddt_nature = NatureId(NatureId(8));
            units = NatureAttrId(NatureAttrId(14));
        Power = NatureId(NatureId(6));

            abstol = NatureAttrId(NatureAttrId(13));
            units = NatureAttrId(NatureAttrId(12));
        Pwr = NatureAccess(NatureAccess(NatureId(6)));
        Q = NatureAccess(NatureAccess(NatureId(1)));
        Tau = NatureAccess(NatureAccess(NatureId(15)));
        Temp = NatureAccess(NatureAccess(NatureId(5)));
        Temperature = NatureId(NatureId(5));

            abstol = NatureAttrId(NatureAttrId(11));
            units = NatureAttrId(NatureAttrId(10));
        Theta = NatureAccess(NatureAccess(NatureId(12)));
        V = NatureAccess(NatureAccess(NatureId(2)));
        Vel = NatureAccess(NatureAccess(NatureId(8)));
        Velocity = NatureId(NatureId(8));

            abstol = NatureAttrId(NatureAttrId(17));
            ddt_nature = NatureId(NatureId(9));
            idt_nature = NatureId(NatureId(7));
            units = NatureAttrId(NatureAttrId(16));
        Voltage = NatureId(NatureId(2));

            abstol = NatureAttrId(NatureAttrId(5));
            idt_nature = NatureId(NatureId(3));
            units = NatureAttrId(NatureAttrId(4));
        current = DisciplineId(DisciplineId(4));
        ddiscrete = DisciplineId(DisciplineId(1));
        electrical = DisciplineId(DisciplineId(2));
        kinematic = DisciplineId(DisciplineId(7));
        kinematic_v = DisciplineId(DisciplineId(8));
        logi = DisciplineId(DisciplineId(0));
        magnetic = DisciplineId(DisciplineId(5));
        rotational = DisciplineId(DisciplineId(9));
        rotational_omega = DisciplineId(DisciplineId(10));
        thermal = DisciplineId(DisciplineId(6));
        voltage = DisciplineId(DisciplineId(3));
    "#]];
    check("`include \"disciplines.vams\"", expect)
}
