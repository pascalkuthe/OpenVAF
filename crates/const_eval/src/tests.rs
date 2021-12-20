use ahash::AHashMap;
use cfg::ControlFlowGraph;
use expect_test::{expect, Expect};

use crate::conditional_const_propagation;

fn check(raw: &str, propagted: Expect) {
    let (mut cfg, literals) = ControlFlowGraph::parse(raw).unwrap();
    conditional_const_propagation(&mut cfg, &AHashMap::new());
    let dump = cfg.dump(&literals);
    propagted.assert_eq(&dump);
}

#[test]
pub fn smoke_test() {
    let raw = r##"
        {
        next_local _10;
        next_place p2;
        bb0:
            let _0 := copy [#0];
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
        }"##;
    let propagted = expect![[r##"
        {
        next_local _10;
        next_place p2;
        bb0:
            let _0 := copy [#0];
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
            let _9 := copy [f64 0.0];
            let p0 := copy [f64 0.0];
            goto bb1;
        bb8:
            end
        }"##]];
    check(raw, propagted)
}

#[test]
pub fn short_circuit() {
    // only short circuit for src < 1 (auto generated code)
    let raw = r##"
        {
        next_local _6;
        next_place p1;
        bb0:
            let _0 := f64.* [f64 0.0,p0];
            let _1 := f64.* [p0,f64 0.0];
            let _2 := f64./ [f64 0.0,p0];
            let _3 := f64.* [f64 0.0,p0]; //src -1;
            let _4 := f64.* [p0,f64 0.0]; //src -1;
            let _5 := f64./ [f64 0.0,p0]; //src -1;
            end
        }"##;
    let propagted = expect![[r#"
        {
        next_local _6;
        next_place p1;
        bb0:
            let _0 := f64.* [f64 0.0, p0];
            let _1 := f64.* [p0, f64 0.0];
            let _2 := f64./ [f64 0.0, p0];
            let _3 := copy [f64 0.0];
            let _4 := copy [f64 0.0];
            let _5 := copy [f64 0.0];
            end
        }"#]];
    check(raw, propagted)
}

#[test]
pub fn const_cond() {
    let raw = r##"
        {
        next_local _6;
        next_place p1;
        bb0:
            if true { bb1 } else { bb2 }
        bb1:
            let p0 := copy [f64 3.141];
            let _1 := copy [f64 2.0];
            goto bb3;
        bb2:
            let p0 := copy [f64 1.0];
            let _2 := copy [f64 1.0];
            goto bb3;
        bb3:
            phi _3 := [(bb1,_1),(bb2,_2)];
            let _4 := copy [p0];
            let _5 := copy [_3];
            end
        }"##;
    let propagted = expect![[r#"
        {
        next_local _6;
        next_place p1;
        bb0:
            if true { bb1 } else { bb2 } 
        bb1:
            let p0 := copy [f64 3.141];
            let _1 := copy [f64 2.0];
            goto bb3;
        bb2:
            let p0 := copy [f64 1.0];
            let _2 := copy [f64 1.0];
            goto bb3;
        bb3:
            phi _3 := [(bb1, _1), (bb2, _2)];
            let _4 := copy [f64 3.141];
            let _5 := copy [f64 2.0];
            end
        }"#]];
    check(raw, propagted)
}

#[test]
pub fn cond() {
    let raw = r##"
        {
        next_local _11;
        next_place p2;
        bb0:
            if #0 { bb1 } else { bb2 }
        bb1:
            let p0 := copy [f64 3.141];
            let p1 := copy [f64 1.0];
            let _1 := copy [f64 2.0];
            let _2 := copy [f64 4.0];
            goto bb3;
        bb2:
            let p0 := copy [f64 1.0];
            let p1 := copy [f64 1.0];
            let _3 := copy [f64 1.0];
            let _4 := copy [f64 4.0];
            goto bb3;
        bb3:
            phi _5 := [(bb1,_1),(bb2,_3)];
            phi _6 := [(bb1,_2),(bb2,_4)];
            let _7 := copy [p0];
            let _8 := copy [p1];
            let _9 := copy [_5];
            let _10 := copy [_6];
            end
        }"##;
    let propagted = expect![[r##"
        {
        next_local _11;
        next_place p2;
        bb0:
            if #0 { bb1 } else { bb2 } 
        bb1:
            let p0 := copy [f64 3.141];
            let p1 := copy [f64 1.0];
            let _1 := copy [f64 2.0];
            let _2 := copy [f64 4.0];
            goto bb3;
        bb2:
            let p0 := copy [f64 1.0];
            let p1 := copy [f64 1.0];
            let _3 := copy [f64 1.0];
            let _4 := copy [f64 4.0];
            goto bb3;
        bb3:
            phi _5 := [(bb1, _1), (bb2, _3)];
            phi _6 := [(bb1, _2), (bb2, _4)];
            let _7 := copy [p0];
            let _8 := copy [f64 1.0];
            let _9 := copy [_5];
            let _10 := copy [f64 4.0];
            end
        }"##]];
    check(raw, propagted)
}

#[test]
pub fn non_const_loop() {
    let raw = r##"
        {
        next_local _2;
        next_place p1;
        bb0:
            let p0 := copy [i32 10];
            goto bb1;
        bb1:
            let p0 := i32.- [p0, i32 10];
            goto bb2;
        bb2:
            let _0 := i32.< [p0,i32 10];
            if _0 { bb3 } else{ bb4 } (loop)
        bb3: 
            let p0 := i32.+ [p0,i32 20];
            goto bb2;
        bb4:
            let _1 := copy [p0]; 
            end
        }"##;
    let propagted = expect![[r#"
        {
        next_local _2;
        next_place p1;
        bb0:
            let p0 := copy [i32 10];
            goto bb1;
        bb1:
            let p0 := copy [i32 0];
            goto bb2;
        bb2:
            let _0 := i32.< [p0, i32 10];
            if _0 { bb3 } else { bb4 } (loop)
        bb3:
            let p0 := i32.+ [p0, i32 20];
            goto bb2;
        bb4:
            let _1 := copy [p0];
            end
        }"#]];
    check(raw, propagted)
}

#[test]
pub fn const_loop() {
    let raw = r##"
        {
        next_local _2;
        next_place p1;
        bb0:
            let p0 := copy [i32 20];
            goto bb1;
        bb1:
            let p0 := i32.- [p0, i32 10];
            goto bb2;
        bb2:
            let _0 := i32.< [p0,i32 10];
            if _0 { bb3 } else{ bb4 } (loop)
        bb3: 
            let p0 := i32.+ [p0,i32 20];
            goto bb2;
        bb4:
            let _1 := copy [p0]; 
            end
        }"##;
    let propagted = expect![[r#"
        {
        next_local _2;
        next_place p1;
        bb0:
            let p0 := copy [i32 20];
            goto bb1;
        bb1:
            let p0 := copy [i32 10];
            goto bb2;
        bb2:
            let _0 := copy [false];
            if false { bb3 } else { bb4 } (loop)
        bb3:
            let p0 := i32.+ [p0, i32 20];
            goto bb2;
        bb4:
            let _1 := copy [i32 10];
            end
        }"#]];
    check(raw, propagted)
}
