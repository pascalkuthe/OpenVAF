use basedb::{diagnostics, BaseDB};
use expect_test::{expect, Expect};
use hir_def::db::HirDefDB;
use hir_def::nameres::ScopeDefItem;
use hir_ty::db::HirTyDB;
use hir_ty::validation::{BodyValidationDiagnostic, TypeValidationDiagnostic};
use stdx::format_to;

use crate::tests::TestDataBase;
use crate::HirInterner;

fn check_with_diagnostics(src: &str, diagnostics: Expect, cfg: Expect) {
    let db = TestDataBase::new("/root.va", src);
    let def_map = db.def_map(db.root_file());

    let def = *def_map[def_map.root()]
        .declarations
        .values()
        .find_map(|def| if let ScopeDefItem::ModuleId(id) = def { Some(id) } else { None })
        .unwrap();
    let def = def.into();
    let mut actual = String::new();

    let res = &db.parse(db.root_file());
    eprintln!("{:?}", res);
    if !res.errors().is_empty() {
        diagnostics::assert_empty_diagnostics(&db, db.root_file(), res.errors())
    }
    let res = &db.inference_result(def);
    if !res.diagnostics.is_empty() {
        format_to!(actual, "{:#?}", res.diagnostics);
    }

    let validation_res = TypeValidationDiagnostic::collect(&db, db.root_file());
    if !validation_res.is_empty() {
        format_to!(actual, "{:#?}", validation_res);
    }

    let validation_res = BodyValidationDiagnostic::collect(&db, def);
    if !validation_res.is_empty() {
        format_to!(actual, "{:#?}", validation_res);
    }

    diagnostics.assert_eq(&actual);

    let res = HirInterner::lower_body(&db, def);
    cfg.assert_eq(&res.0.dump(Some(&res.2)));
}

fn check(src: &str, cfg: Expect) {
    check_with_diagnostics(src, expect![[r#""#]], cfg)
}

#[test]
fn case() {
    let src = r#"
module test;
    parameter integer foo = 0;
    parameter integer bar = 0;
    real test;
    real test2;
    analog case(abs(foo)+bar)
        0: test = 3.141;
        1,2,3: begin
            test = foo / 3.141;
            test2 = sin(test);
        end
        default: test = 0;
    endcase

endmodule
    "#;
    let cfg = expect![[r##"
        {
        next_local _13;
        next_place p2;
        bb0:
            let _0 := i32.< [#0, i32 0];
            if _0 { bb1 } else { bb2 } 
        bb1:
            let _1 := i32- [#0];
            goto bb3;
        bb2:
            let _2 := copy [#0];
            goto bb3;
        bb3:
            phi _3 := [(bb1, _1), (bb2, _2)];
            let _4 := i32.+ [_3, #1];
            let _5 := i32.== [i32 0, _4];
            if _5 { bb5 } else { bb6 } 
        bb4:
            end
        bb5:
            let p0 := copy [f64 3.141];
            goto bb4;
        bb6:
            let _9 := i32.== [i32 1, _4];
            if _9 { bb7 } else { bb8 } 
        bb7:
            let _6 := cast_i32_f64 [#0];
            let _7 := f64./ [_6, f64 3.141];
            let p0 := copy [_7];
            let _8 := sin [p0];
            let p1 := copy [_8];
            goto bb4;
        bb8:
            let _10 := i32.== [i32 2, _4];
            if _10 { bb7 } else { bb9 } 
        bb9:
            let _11 := i32.== [i32 3, _4];
            if _11 { bb7 } else { bb10 } 
        bb10:
            let _12 := cast_i32_f64 [i32 0];
            let p0 := copy [_12];
            goto bb4;
        }"##]];
    check(src, cfg)
}
