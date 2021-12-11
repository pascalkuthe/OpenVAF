use basedb::{diagnostics, BaseDB};
use expect_test::{expect, Expect};
use hir_def::db::HirDefDB;
use hir_def::nameres::ScopeDefItem;
use hir_ty::db::HirTyDB;
use hir_ty::validation::{BodyValidationDiagnostic, TypeValidationDiagnostic};
use stdx::format_to;

use crate::tests::TestDataBase;
use crate::LoweringResult;

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

    let res = LoweringResult::lower_body(&db, def);
    let lits = &db.body_source_map(def).str_lit_interner;
    cfg.assert_eq(&res.cfg.dump(lits));
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
        next_local _10;
        next_place p2;
        bb0:
            let _0 := i32.abs [#0];
            let _1 := i32.+ [_0, #1];
            let _2 := i32.== [i32 0, _1];
            if _2 { bb2 } else { bb3 } 
        bb1:
            goto bb8;
        bb2:
            let p0 := copy [f64 3.141];
            goto bb1;
        bb3:
            let _6 := i32.== [i32 1, _1];
            if _6 { bb4 } else { bb5 } 
        bb4:
            let _3 := cast_i32_f64 [#0];
            let _4 := f64./ [_3, f64 3.141];
            let p0 := copy [_4];
            let _5 := sin [p0];
            let p1 := copy [_5];
            goto bb1;
        bb5:
            let _7 := i32.== [i32 2, _1];
            if _7 { bb4 } else { bb6 } 
        bb6:
            let _8 := i32.== [i32 3, _1];
            if _8 { bb4 } else { bb7 } 
        bb7:
            let _9 := cast_i32_f64 [i32 0];
            let p0 := copy [_9];
            goto bb1;
        bb8:
            end
        }"##]];
    check(src, cfg)
}
