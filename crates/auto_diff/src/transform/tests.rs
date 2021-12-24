use cfg::ControlFlowGraph;
use expect_test::{expect, Expect};

use crate::auto_diff;

// TODO backend integration test that derivatives const eval correctly

fn check(src: &str, data_flow_result: Expect) {
    let (mut cfg, literals) = ControlFlowGraph::parse(src).unwrap();

    auto_diff(
        &mut cfg,
        [
            (0u32.into(), vec![(0u32.into(), 1f64.to_bits())].into_boxed_slice()),
            (1u32.into(), vec![(1u32.into(), 1f64.to_bits())].into_boxed_slice()),
        ],
        None,
    );

    let actual = cfg.dump(Some(&literals));
    data_flow_result.assert_eq(&actual);
}

#[test]
fn phi() {
    let src = r##"
        {
        next_local _4;
        next_place p0;
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
            let _3 := cb1 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _7;
        next_place p0;
        bb0:
            if #0 { bb1 } else { bb2 } 
        bb1:
            let _0 := exp [#1];
            let _4 := f64.* [_0, f64 1.0];
            goto bb3;
        bb2:
            let _1 := exp [#2];
            let _5 := f64.* [_1, f64 0.0];
            goto bb3;
        bb3:
            phi _6 := [(bb1, _4), (bb2, _5)];
            phi _2 := [(bb1, _0), (bb2, _1)];
            let _3 := copy [_6];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn place() {
    let src = r##"
        {
        next_local _0;
        next_place p1;
        bb0:
            if #0 { bb1 } else { bb2 }
        bb1:
            let p0 := exp [#1];
            goto bb3;
        bb2:
            let p0 := exp [#2];
            goto bb3;
        bb3:
            let _3 := cb1 [p0];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _0;
        next_place p2;
        bb0:
            if #0 { bb1 } else { bb2 } 
        bb1:
            let p0 := exp [#1];
            let p1 := f64.* [p0, f64 1.0];
            goto bb3;
        bb2:
            let p0 := exp [#2];
            let p1 := f64.* [p0, f64 0.0];
            goto bb3;
        bb3:
            let _3 := copy [p1];
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
        next_local _3;
        next_place p2;
        bb0:
            let p0 := copy [#1];
            let p1 := copy [f64 1.0];
            let _0 := f64.* [p0, #1];
            let _1 := f64.* [p0, f64 1.0];
            let _2 := f64.* [p1, #1];
            let p1 := f64.+ [_1, _2];
            let p0 := copy [_0];
            let p0 := copy [p1];
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
            let _0 := copy [p1];
            let p1 := copy [p2];
            let p0 := copy [_0];
            let p0 := copy [p1];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn exp() {
    let src = r##"
        {
        next_local _3;
        next_place p0;
        bb0:
            let _0 := exp [#1];
            let _1 := cb0 [_0];
            let _2 := cb1 [_0];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _5;
        next_place p0;
        bb0:
            let _0 := exp [#1];
            let _4 := f64.* [_0, f64 0.0];
            let _3 := f64.* [_0, f64 1.0];
            let _1 := copy [_4];
            let _2 := copy [_3];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn mul() {
    let src = r##"
        {
        next_local _4;
        next_place p0;
        bb0:
            let _0 := exp [#1];
            let _1 := exp [#2];
            let _2 := f64.* [_1,_0];
            let _3 := cb0 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _9;
        next_place p0;
        bb0:
            let _0 := exp [#1];
            let _5 := f64.* [_0, f64 0.0];
            let _1 := exp [#2];
            let _6 := f64.* [_1, f64 0.0];
            let _2 := f64.* [_1, _0];
            let _7 := f64.* [_1, _5];
            let _8 := f64.* [_6, _0];
            let _4 := f64.+ [_7, _8];
            let _3 := copy [_4];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn int_mul() {
    let src = r##"
        {
        next_local _4;
        next_place p0;
        bb0:
            let _0 := i32- [#0];
            let _1 := i32- [#1];
            let _2 := i32.* [_1,_0];
            let _3 := cb0 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _11;
        next_place p0;
        bb0:
            let _0 := i32- [#0];
            let _7 := f64- [f64 1.0];
            let _1 := i32- [#1];
            let _8 := f64- [f64 0.0];
            let _2 := i32.* [_1, _0];
            let _5 := cast_i32_f64 [_1];
            let _6 := cast_i32_f64 [_0];
            let _9 := f64.* [_5, _7];
            let _10 := f64.* [_8, _6];
            let _4 := f64.+ [_9, _10];
            let _3 := copy [_4];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn int_div() {
    let src = r##"
        {
        next_local _4;
        next_place p0;
        bb0:
            let _0 := i32- [#0];
            let _1 := i32- [#1];
            let _2 := i32./ [_1,_0];
            let _3 := cb0 [_2];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _14;
        next_place p0;
        bb0:
            let _0 := i32- [#0];
            let _11 := f64- [f64 1.0];
            let _1 := i32- [#1];
            let _9 := f64- [f64 0.0];
            let _2 := i32./ [_1, _0];
            let _5 := i32.* [_0, _0];
            let _6 := cast_i32_f64 [_5];
            let _7 := cast_i32_f64 [_1];
            let _8 := cast_i32_f64 [_0];
            let _10 := f64./ [_9, _8];
            let _12 := f64.* [_7, _11];
            let _13 := f64./ [_12, _6];
            let _4 := f64.- [_10, _13];
            let _3 := copy [_4];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn exp_second_order() {
    let src = r##"
        {
        next_local _3;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _8;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _4 := f64.* [_0, f64 1.0];
            let _6 := f64.* [_0, f64 0.0];
            let _7 := f64.* [_4, f64 1.0];
            let _5 := f64.+ [_6, _7];
            let _1 := copy [_4];
            let _3 := copy [_5];
            let _2 := copy [_3];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn mul_second_order() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _1 := exp [#1];
            let _3 := f64.* [_0,_1];
            let _4 := cb0 [_3];
            let _5 := cb1 [_4];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _30;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _10 := f64.* [_0, f64 1.0];
            let _15 := f64.* [_0, f64 0.0];
            let _28 := f64.* [_0, f64 0.0];
            let _29 := f64.* [_15, f64 1.0];
            let _23 := f64.+ [_28, _29];
            let _1 := exp [#1];
            let _9 := f64.* [_1, f64 0.0];
            let _14 := f64.* [_1, f64 1.0];
            let _26 := f64.* [_1, f64 0.0];
            let _27 := f64.* [_14, f64 0.0];
            let _19 := f64.+ [_26, _27];
            let _3 := f64.* [_0, _1];
            let _11 := f64.* [_0, _9];
            let _12 := f64.* [_10, _1];
            let _7 := f64.+ [_11, _12];
            let _16 := f64.* [_0, _14];
            let _17 := f64.* [_15, _1];
            let _13 := f64.+ [_16, _17];
            let _20 := f64.* [_0, _19];
            let _21 := f64.* [_15, _9];
            let _18 := f64.+ [_20, _21];
            let _24 := f64.* [_10, _14];
            let _25 := f64.* [_23, _1];
            let _22 := f64.+ [_24, _25];
            let _8 := f64.+ [_18, _22];
            let _4 := copy [_7];
            let _6 := copy [_8];
            let _5 := copy [_6];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn sin_second_order() {
    let src = r##"
        {
        next_local _3;
        next_place p0;
        bb0:
            let _0 := sin [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _12;
        next_place p0;
        bb0:
            let _0 := sin [#0];
            let _6 := cos [#0];
            let _4 := f64.* [_6, f64 1.0];
            let _7 := sin [#0];
            let _8 := f64- [_7];
            let _9 := f64.* [_8, f64 1.0];
            let _10 := f64.* [_6, f64 0.0];
            let _11 := f64.* [_9, f64 1.0];
            let _5 := f64.+ [_10, _11];
            let _1 := copy [_4];
            let _3 := copy [_5];
            let _2 := copy [_3];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn sin_exp_second_order() {
    let src = r##"
        {
        next_local _5;
        next_place p0;
        bb0:
            let _0 := sin [#0];
            let _1 := exp [#0];
            let _2 := f64.* [_0, _1];
            let _3 := cb0 [_2];
            let _4 := cb0 [_3];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _28;
        next_place p0;
        bb0:
            let _0 := sin [#0];
            let _22 := cos [#0];
            let _9 := f64.* [_22, f64 1.0];
            let _23 := sin [#0];
            let _24 := f64- [_23];
            let _25 := f64.* [_24, f64 1.0];
            let _26 := f64.* [_22, f64 0.0];
            let _27 := f64.* [_25, f64 1.0];
            let _17 := f64.+ [_26, _27];
            let _1 := exp [#0];
            let _8 := f64.* [_1, f64 1.0];
            let _20 := f64.* [_1, f64 0.0];
            let _21 := f64.* [_8, f64 1.0];
            let _13 := f64.+ [_20, _21];
            let _2 := f64.* [_0, _1];
            let _10 := f64.* [_0, _8];
            let _11 := f64.* [_9, _1];
            let _6 := f64.+ [_10, _11];
            let _14 := f64.* [_0, _13];
            let _15 := f64.* [_9, _8];
            let _12 := f64.+ [_14, _15];
            let _18 := f64.* [_9, _8];
            let _19 := f64.* [_17, _1];
            let _16 := f64.+ [_18, _19];
            let _7 := f64.+ [_12, _16];
            let _3 := copy [_6];
            let _5 := copy [_7];
            let _4 := copy [_5];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn ln_sin_exp() {
    let src = r##"
        {
        next_local _5;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _1 := sin [#0];
            let _2 := f64.* [_0,_1];
            let _3 := ln [_2];
            let _4 := cb0 [_3];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _12;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _8 := f64.* [_0, f64 1.0];
            let _1 := sin [#0];
            let _11 := cos [#0];
            let _7 := f64.* [_11, f64 1.0];
            let _2 := f64.* [_0, _1];
            let _9 := f64.* [_0, _7];
            let _10 := f64.* [_8, _1];
            let _6 := f64.+ [_9, _10];
            let _3 := ln [_2];
            let _5 := f64./ [_6, _2];
            let _4 := copy [_5];
            end
        }"##]];
    check(src, expect);
}

#[test]
fn ln_sinh_exp() {
    let src = r##"
        {
        next_local _5;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _1 := sinh [#0];
            let _2 := f64.* [_0,_1];
            let _3 := ln [_2];
            let _4 := cb0 [_3];
            end
        }"##;
    let expect = expect![[r##"
        {
        next_local _12;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _8 := f64.* [_0, f64 1.0];
            let _1 := sinh [#0];
            let _11 := cosh [#0];
            let _7 := f64.* [_11, f64 1.0];
            let _2 := f64.* [_0, _1];
            let _9 := f64.* [_0, _7];
            let _10 := f64.* [_8, _1];
            let _6 := f64.+ [_9, _10];
            let _3 := ln [_2];
            let _5 := f64./ [_6, _2];
            let _4 := copy [_5];
            end
        }"##]];
    check(src, expect);
}
