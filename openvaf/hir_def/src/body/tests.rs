use std::fs::read_to_string;

use basedb::BaseDB;
use expect_test::{expect, Expect};
use sourcegen::project_root;

use crate::db::HirDefDB;
use crate::tests::TestDataBase;
use crate::{FunctionId, ModuleId};

fn check_db(db: TestDataBase, expect: Expect) {
    assert_eq!(db.parse(db.root_file()).errors(), &[]);
    let actual = db.lower_and_check();
    assert_eq!(actual, "");
    let actual = db.body(ModuleId(0u32.into()).into()).dump(&db);
    expect.assert_eq(&actual);
}

fn check(src: &str, expect: Expect) {
    let db = TestDataBase::new("/root.va", src);
    check_db(db, expect)
}

fn check_db_fun(db: TestDataBase, expect: Expect) {
    assert_eq!(db.parse(db.root_file()).errors(), &[]);
    let actual = db.lower_and_check();
    assert_eq!(actual, "");
    let actual = db.body(FunctionId(0u32.into()).into()).dump(&db);
    expect.assert_eq(&actual);
}

fn check_fun(src: &str, expect: Expect) {
    let db = TestDataBase::new("/root.va", src);
    check_db_fun(db, expect)
}

#[test]
fn nested_blocks() {
    check(
        r#"
        module test;
            analog begin
                begin : Model_initialization
                   parameter real foo = 0.0;
                   x = 0.0;
                   y = exp(x+1);

                end
            end
        endmodule
        "#,
        expect![[r#"
            analog begin: (Root)

                begin: BlockId(0) (ScopeId { root_file: FileId(0), local_scope: Idx::<Scope>(1), src: Root })

                    x=0;
                    y=exp(x + 1, );
                end
            end
        "#]],
    )
}

#[test]
fn branch_access() {
    check(
        r#"
        module test;
            analog begin
                x = V(a,b);
                y = V(x);
                z = I(<x>);
                z = I(br_ab);
            end
        endmodule
        "#,
        expect![[r#"
            analog begin: (Root)

                x=V(a, b, );
                y=V(x, );
                z=I(<x>, );
                z=I(br_ab, );
            end
        "#]],
    )
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
endmodule
"#;

    let expect = expect![[r#"
        analog test=x * y;
    "#]];
    check_fun(src, expect)
}

#[test]
pub fn diode() {
    let path = project_root().join("integration_tests").join("DIODE").join("diode.va");
    let src = read_to_string(path).unwrap();

    let expect = expect![[r#"
        analog begin: (Root)

            if Rth > minr
            begin: (Root)

                Tdev=$temperature() + V(br_sht, );
            end
            else
            begin: (Root)

                Tdev=$temperature();
            end
            VT=0.000000000000000000000013806503 * Tdev / 0.0000000000000000001602176462;
            Is_t=Is * exp(ln(Tdev / Tnom, ) * zetais / N + Tdev / Tnom - 1 * ea / VT * N, );
            rs_t=Rs * pow(Tdev / Tnom, zetars, );
            rth_t=Rth * pow(Tdev / Tnom, zetarth, );
            Vd=V(br_a_ci, );
            Vr=V(br_ci_c, );
            Id=Is_t * limexp(Vd / N * VT, ) - 1;
            vf=Vj * 1 - pow(3, -1 / M, );
            x=vf - Vd / VT;
            y=sqrt(x * x + 1.92, );
            Vd_smooth=vf - VT * x + y / 2;
            Qd=Cj0 * Vj * 1 - pow(1 - Vd_smooth / Vj, 1 - M, ) / 1 - M;
            I(br_a_ci, )<+Id + ddt(Qd, ) + $simparam("<literal>", 0.000000000001, ) * Vd;
            if Rs > minr
            begin: (Root)

                I(br_ci_c, )<+Vr / rs_t;
            end
            else
            begin: (Root)

                V(br_ci_c, )<+0;
            end
            if Rth > minr
            begin: (Root)

                pterm=Id * Vd;
                if Rs > minr
                begin: (Root)

                    pterm=pterm + pow(Vr, 2, ) / rs_t;
                end
                else
                <missing>;
                I(br_sht, )<+pterm - V(br_sht, ) / rth_t;
            end
            else
            begin: (Root)

                V(br_sht, )<+0;
            end
            cd=ddx(Qd, V(A, ), );
            gd=ddx(Id, V(A, ), );
        end
    "#]];
    check(&src, expect)
}
