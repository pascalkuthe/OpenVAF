use cfg::ControlFlowGraph;
use expect_test::{expect, Expect};
use program_dependence::{use_def, AssigmentInterner, ProgramDependenGraph};

use crate::auto_diff;

// TODO backend integration test that derivatives const eval correctly

fn check(src: &str, data_flow_result: Expect) {
    let (mut cfg, literals) = ControlFlowGraph::parse(src).unwrap();

    let assignments = AssigmentInterner::new(&cfg);
    let pdg = ProgramDependenGraph::build(&assignments, &cfg);
    let mut dfs = use_def::DepthFirstSearch::new(&pdg);
    dfs.walk_place::<true>(0u32.into(), &pdg, &cfg);

    auto_diff(
        &mut cfg,
        &pdg,
        &dfs,
        4,
        [
            (0u32.into(), vec![(0u32.into(), 1f64.to_bits())].into_boxed_slice()),
            (1u32.into(), vec![(1u32.into(), 1f64.to_bits())].into_boxed_slice()),
        ],
        [],
    );
    let actual = cfg.dump(Some(&literals));
    data_flow_result.assert_eq(&actual);
}

#[test]
fn phi() {
    let src = r##"
        {
        next_local _3;
        next_place p1;
        bb0:
            if #0 { bb1 } else { bb2 }
        bb1:
            let _0 := exp [#1];
            goto bb3;
        bb2:
            let _1 := exp [#2];
            goto bb3;
        bb3:
            phi _2 := [(bb1,_0),(bb2,_1)];  
            let p0 := cb1 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _6;
        next_place p1;
        bb0:
            let _3 := copy [f64 0.0];
            if #0 { bb1 } else { bb2 } 
        bb1:
            let _0 := exp [#1];
            let _4 := f64.* [_0, f64 1.0];
            goto bb3;
        bb2:
            let _1 := exp [#2];
            goto bb3;
        bb3:
            phi _2 := [(bb1, _0), (bb2, _1)];
            phi _5 := [(bb1, _4), (bb2, _3)];
            let p0 := copy [_5];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn place() {
    let src = r##"
        {
        next_local _0;
        next_place p2;
        bb0:
            if #0 { bb1 } else { bb2 }
        bb1:
            let p1 := exp [#1];
            goto bb3;
        bb2:
            let p1 := exp [#2];
            goto bb3;
        bb3:
            let p0 := cb1 [p1];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _1;
        next_place p3;
        bb0:
            let _0 := copy [f64 0.0];
            if #0 { bb1 } else { bb2 } 
        bb1:
            let p1 := exp [#1];
            let p2 := f64.* [p1, f64 1.0];
            goto bb3;
        bb2:
            let p1 := exp [#2];
            goto bb3;
        bb3:
            let p0 := copy [p2];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn self_assign() {
    let src = r##"
        {
        next_local _0;
        next_place p1;
        bb0:
            let p0 := copy [#1];
            let p0 := f64.* [p0,#1];
            let p0 := cb1 [p0];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _4;
        next_place p2;
        bb0:
            let p0 := copy [#1];
            let p1 := copy [f64 1.0];
            let _1 := f64.* [p0, #1];
            let _2 := f64.* [p0, f64 1.0];
            let _3 := f64.* [p1, #1];
            let p1 := f64.+ [_2, _3];
            let p0 := copy [_1];
            let p0 := copy [p1];
            let _0 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn self_assign_second_order() {
    let src = r##"
        {
        next_local _0;
        next_place p1;
        bb0:
            let p0 := copy [#1];
            let p0 := f64.* [p0,#1];
            let p0 := cb1 [p0];
            let p0 := cb1 [p0];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _11;
        next_place p3;
        bb0:
            let p0 := copy [#1];
            let p1 := copy [f64 1.0];
            let p2 := copy [f64 0.0];
            let _1 := f64.* [p0, #1];
            let _2 := f64.* [p0, f64 1.0];
            let _3 := f64.* [p1, #1];
            let _4 := f64.+ [_2, _3];
            let _6 := f64.* [p0, f64 0.0];
            let _7 := f64.* [p1, f64 1.0];
            let _5 := f64.+ [_6, _7];
            let _9 := f64.* [p1, f64 1.0];
            let _10 := f64.* [p2, #1];
            let _8 := f64.+ [_9, _10];
            let p2 := f64.+ [_5, _8];
            let p0 := copy [_1];
            let p1 := copy [_4];
            let p0 := copy [p1];
            let p1 := copy [p2];
            let p0 := copy [p1];
            let _0 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn exp() {
    let src = r##"
        {
        next_local _3;
        next_place p1;
        bb0:
            let _0 := exp [#1];
            let _1 := cb0 [_0];
            let _2 := cb1 [_0];
            let p0 := f64.+ [_1, _2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _6;
        next_place p1;
        bb0:
            let _0 := exp [#1];
            let _4 := f64.* [_0, f64 0.0];
            let _5 := f64.* [_0, f64 1.0];
            let _1 := copy [_4];
            let _2 := copy [_5];
            let p0 := f64.+ [_1, _2];
            let _3 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn mul() {
    let src = r##"
        {
        next_local _4;
        next_place p1;
        bb0:
            let _0 := exp [#1];
            let _1 := exp [#2];
            let _2 := f64.* [_1,_0];
            let p0 := cb0 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _9;
        next_place p1;
        bb0:
            let _0 := exp [#1];
            let _5 := f64.* [_0, f64 0.0];
            let _1 := exp [#2];
            let _2 := f64.* [_1, _0];
            let _7 := f64.* [_1, _5];
            let _8 := f64.* [f64 0.0, _0];
            let _6 := f64.+ [_7, _8];
            let p0 := copy [_6];
            let _4 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn int_mul() {
    let src = r##"
        {
        next_local _4;
        next_place p1;
        bb0:
            let _0 := i32- [#0];
            let _1 := i32- [#1];
            let _2 := i32.* [_1,_0];
            let p0 := cb0 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _12;
        next_place p1;
        bb0:
            let _0 := i32- [#0];
            let _5 := f64- [f64 1.0];
            let _1 := i32- [#1];
            let _6 := f64- [f64 0.0];
            let _2 := i32.* [_1, _0];
            let _8 := cast_i32_f64 [_1];
            let _9 := cast_i32_f64 [_0];
            let _10 := f64.* [_8, _5];
            let _11 := f64.* [_6, _9];
            let _7 := f64.+ [_10, _11];
            let p0 := copy [_7];
            let _4 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn int_div() {
    let src = r##"
        {
        next_local _4;
        next_place p1;
        bb0:
            let _0 := i32- [#0];
            let _1 := i32- [#1];
            let _2 := i32./ [_1,_0];
            let p0 := cb0 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _15;
        next_place p1;
        bb0:
            let _0 := i32- [#0];
            let _5 := f64- [f64 1.0];
            let _1 := i32- [#1];
            let _6 := f64- [f64 0.0];
            let _2 := i32./ [_1, _0];
            let _7 := i32.* [_0, _0];
            let _8 := cast_i32_f64 [_7];
            let _10 := cast_i32_f64 [_1];
            let _11 := cast_i32_f64 [_0];
            let _12 := f64./ [_6, _11];
            let _13 := f64.* [_10, _5];
            let _14 := f64./ [_13, _8];
            let _9 := f64.- [_12, _14];
            let p0 := copy [_9];
            let _4 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn exp_second_order() {
    let src = r##"
        {
        next_local _3;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _1 := cb0 [_0];
            let p0 := cb0 [_1];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _9;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _4 := f64.* [_0, f64 1.0];
            let _6 := f64.* [_0, f64 0.0];
            let _7 := f64.* [_4, f64 1.0];
            let _5 := f64.+ [_6, _7];
            let _1 := copy [_4];
            let _8 := copy [_5];
            let p0 := copy [_8];
            let _3 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn mul_second_order() {
    let src = r##"
        {
        next_local _6;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _1 := exp [#1];
            let _3 := f64.* [_0,_1];
            let _4 := cb0 [_3];
            let p0 := cb1 [_4];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _31;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _7 := f64.* [_0, f64 1.0];
            let _8 := f64.* [_0, f64 0.0];
            let _10 := f64.* [_0, f64 0.0];
            let _11 := f64.* [_8, f64 1.0];
            let _9 := f64.+ [_10, _11];
            let _1 := exp [#1];
            let _12 := f64.* [_1, f64 0.0];
            let _13 := f64.* [_1, f64 1.0];
            let _15 := f64.* [_1, f64 0.0];
            let _16 := f64.* [_13, f64 0.0];
            let _14 := f64.+ [_15, _16];
            let _3 := f64.* [_0, _1];
            let _18 := f64.* [_0, _12];
            let _19 := f64.* [_7, _1];
            let _17 := f64.+ [_18, _19];
            let _21 := f64.* [_0, _13];
            let _22 := f64.* [_8, _1];
            let _20 := f64.+ [_21, _22];
            let _24 := f64.* [_0, _14];
            let _25 := f64.* [_8, _12];
            let _23 := f64.+ [_24, _25];
            let _27 := f64.* [_7, _13];
            let _28 := f64.* [_9, _1];
            let _26 := f64.+ [_27, _28];
            let _29 := f64.+ [_23, _26];
            let _4 := copy [_17];
            let _30 := copy [_29];
            let p0 := copy [_30];
            let _6 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn sin_second_order() {
    let src = r##"
        {
        next_local _3;
        next_place p1;
        bb0:
            let _0 := sin [#0];
            let _1 := cb0 [_0];
            let p0 := cb0 [_1];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _13;
        next_place p1;
        bb0:
            let _0 := sin [#0];
            let _4 := cos [#0];
            let _5 := f64.* [_4, f64 1.0];
            let _6 := sin [#0];
            let _7 := f64- [_6];
            let _8 := f64.* [_7, f64 1.0];
            let _10 := f64.* [_4, f64 0.0];
            let _11 := f64.* [_8, f64 1.0];
            let _9 := f64.+ [_10, _11];
            let _1 := copy [_5];
            let _12 := copy [_9];
            let p0 := copy [_12];
            let _3 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn sin_exp_second_order() {
    let src = r##"
        {
        next_local _5;
        next_place p1;
        bb0:
            let _0 := sin [#0];
            let _1 := exp [#0];
            let _2 := f64.* [_0, _1];
            let _3 := cb0 [_2];
            let p0 := cb0 [_3];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _29;
        next_place p1;
        bb0:
            let _0 := sin [#0];
            let _6 := cos [#0];
            let _7 := f64.* [_6, f64 1.0];
            let _8 := sin [#0];
            let _9 := f64- [_8];
            let _10 := f64.* [_9, f64 1.0];
            let _12 := f64.* [_6, f64 0.0];
            let _13 := f64.* [_10, f64 1.0];
            let _11 := f64.+ [_12, _13];
            let _1 := exp [#0];
            let _14 := f64.* [_1, f64 1.0];
            let _16 := f64.* [_1, f64 0.0];
            let _17 := f64.* [_14, f64 1.0];
            let _15 := f64.+ [_16, _17];
            let _2 := f64.* [_0, _1];
            let _19 := f64.* [_0, _14];
            let _20 := f64.* [_7, _1];
            let _18 := f64.+ [_19, _20];
            let _22 := f64.* [_0, _15];
            let _23 := f64.* [_7, _14];
            let _21 := f64.+ [_22, _23];
            let _25 := f64.* [_7, _14];
            let _26 := f64.* [_11, _1];
            let _24 := f64.+ [_25, _26];
            let _27 := f64.+ [_21, _24];
            let _3 := copy [_18];
            let _28 := copy [_27];
            let p0 := copy [_28];
            let _5 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn ln_sin_exp() {
    let src = r##"
        {
        next_local _5;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _1 := sin [#0];
            let _2 := f64.* [_0,_1];
            let _3 := ln [_2];
            let p0 := cb0 [_3];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _13;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _6 := f64.* [_0, f64 1.0];
            let _1 := sin [#0];
            let _7 := cos [#0];
            let _8 := f64.* [_7, f64 1.0];
            let _2 := f64.* [_0, _1];
            let _10 := f64.* [_0, _8];
            let _11 := f64.* [_6, _1];
            let _9 := f64.+ [_10, _11];
            let _3 := ln [_2];
            let _12 := f64./ [_9, _2];
            let p0 := copy [_12];
            let _5 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn ln_sinh_exp() {
    let src = r##"
        {
        next_local _5;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _1 := sinh [#0];
            let _2 := f64.* [_0,_1];
            let _3 := ln [_2];
            let p0 := cb0 [_3];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _13;
        next_place p1;
        bb0:
            let _0 := exp [#0];
            let _6 := f64.* [_0, f64 1.0];
            let _1 := sinh [#0];
            let _7 := cosh [#0];
            let _8 := f64.* [_7, f64 1.0];
            let _2 := f64.* [_0, _1];
            let _10 := f64.* [_0, _8];
            let _11 := f64.* [_6, _1];
            let _9 := f64.+ [_10, _11];
            let _3 := ln [_2];
            let _12 := f64./ [_9, _2];
            let p0 := copy [_12];
            let _5 := copy [f64 0.0];
            end
        }"##]];
    check(src, expect);
}
