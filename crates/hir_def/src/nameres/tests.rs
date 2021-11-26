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
        Acc = NatureAccess(NatureAccess(NatureAttrId(9)));
        Acceleration = NatureId(NatureId(9));
        Alpha = NatureAccess(NatureAccess(NatureAttrId(14)));
        Angle = NatureId(NatureId(12));
        Angular_Acceleration = NatureId(NatureId(14));
        Angular_Force = NatureId(NatureId(15));
        Angular_Velocity = NatureId(NatureId(13));
        Charge = NatureId(NatureId(1));
        Current = NatureId(NatureId(0));
        F = NatureAccess(NatureAccess(NatureAttrId(11)));
        Flux = NatureId(NatureId(3));
        Force = NatureId(NatureId(11));
        I = NatureAccess(NatureAccess(NatureAttrId(0)));
        Imp = NatureAccess(NatureAccess(NatureAttrId(10)));
        Impulse = NatureId(NatureId(10));
        MMF = NatureAccess(NatureAccess(NatureAttrId(4)));
        Magneto_Motive_Force = NatureId(NatureId(4));
        Omega = NatureAccess(NatureAccess(NatureAttrId(13)));
        Phi = NatureAccess(NatureAccess(NatureAttrId(3)));
        Pos = NatureAccess(NatureAccess(NatureAttrId(7)));
        Position = NatureId(NatureId(7));
        Power = NatureId(NatureId(6));
        Pwr = NatureAccess(NatureAccess(NatureAttrId(6)));
        Q = NatureAccess(NatureAccess(NatureAttrId(1)));
        Tau = NatureAccess(NatureAccess(NatureAttrId(15)));
        Temp = NatureAccess(NatureAccess(NatureAttrId(5)));
        Temperature = NatureId(NatureId(5));
        Theta = NatureAccess(NatureAccess(NatureAttrId(12)));
        V = NatureAccess(NatureAccess(NatureAttrId(2)));
        Vel = NatureAccess(NatureAccess(NatureAttrId(8)));
        Velocity = NatureId(NatureId(8));
        Voltage = NatureId(NatureId(2));
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
