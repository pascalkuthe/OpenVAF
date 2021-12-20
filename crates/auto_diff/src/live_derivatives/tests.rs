use cfg::ControlFlowGraph;
use expect_test::{expect, Expect};

use crate::live_derivatives;

fn check(src: &str, data_flow_result: Expect) {
    let (cfg, _) = ControlFlowGraph::parse(src).unwrap();

    let res = live_derivatives(
        &cfg,
        [
            (0u32.into(), vec![(0u32.into(), 1f64.to_bits())].into_boxed_slice()),
            (1u32.into(), vec![(1u32.into(), 1f64.to_bits())].into_boxed_slice()),
        ],
        None,
    );

    let actual =
        format!("{:#?}\n{:#?}", res.analysis.live_local_derivatives.borrow(), res.entry_sets);
    data_flow_result.assert_eq(&actual);
}

#[test]
fn smoke_test() {
    let src = r##"
        {
        next_local _11;
        next_place p2;
        bb0:
            let p0 := copy [f64 0.0];
            let _0 := f64.- [#0];
            let _1 := cb2 [#1, f64 3.141];
            let _2 := f64./ [#2, f64 3.141];
            let _3 := sin [#3];
            let _4 := f64.!= [#4, f64 1.0];
            if _4 { bb1 } else { bb2 }
        bb1:
            let _5 := f64.* [_1,#5];
            let p0 := copy [_2];
            goto bb3;
        bb2:
            let _6 := copy [_3];
            goto bb3;
        bb3:
            phi _7 := [(bb1,_5),(bb2,_6)];
            let _8 := cb0 [_7];
            let _9 := f64.+ [p0,_8];
            let _10 := cb1 [_9];
            end
        }"##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(11x2) {
            _1: unkown0,
            _1: unkown1,
            _1: unkown2,
            _2: unkown1,
            _3: unkown0,
            _3: unkown1,
            _3: unkown2,
            _5: unkown0,
            _5: unkown1,
            _5: unkown2,
            _6: unkown0,
            _6: unkown1,
            _6: unkown2,
            _7: unkown0,
            _7: unkown1,
            _7: unkown2,
            _8: unkown1,
            _9: unkown1,
        }
        {
            bb0: SparseBitMatrix(2x2) {
                p0: unkown1,
            },
            bb1: SparseBitMatrix(2x2) {
                p0: unkown1,
            },
            bb2: SparseBitMatrix(2x2) {
                p0: unkown1,
            },
            bb3: SparseBitMatrix(2x2) {},
        }"#]];

    check(src, data_flow_result)
}

#[test]
fn loop_with_backwards_dependency() {
    let src = r##"
        {
        next_local _4;
        next_place p1;
        bb0:
            let p0 := copy [i32 10];
            let _1 := f64.+ [f64 3141, #0];
            goto bb1;
        bb1:
            let p0 := i32.- [p0, i32 10];
            goto bb2;
        bb2:
            let _0 := i32.< [p0,i32 10];
            if _0 { bb3 } else{ bb4 } (loop)
        bb3: 
            let _2 := cast_i32_f64 [p0];
            let p0 := i32.+ [p0,i32 20];
            let p1 := f64.* [_2,_1];
            goto bb2;
        bb4:
            let _3 := cb0 [p1]; 
            end
        }"##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(4x2) {
            _1: unkown0,
            _2: unkown0,
        }
        {
            bb0: SparseBitMatrix(1x2) {
                p0: unkown0,
                p1: unkown0,
            },
            bb1: SparseBitMatrix(1x2) {
                p0: unkown0,
                p1: unkown0,
            },
            bb2: SparseBitMatrix(1x2) {
                p0: unkown0,
                p1: unkown0,
            },
            bb3: SparseBitMatrix(1x2) {
                p0: unkown0,
                p1: unkown0,
            },
            bb4: SparseBitMatrix(1x2) {},
        }"#]];

    check(src, data_flow_result)
}

#[test]
fn self_assign() {
    let src = r##"
        {
        next_local _4;
        next_place p1;
        bb0:
            let p0 := copy [#1];
            goto bb1;
        bb1:
            let p0 := exp [p0];
            let p0 := cb1 [p0];
            end
        }"##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(4x2) {}
        {
            bb0: SparseBitMatrix(1x2) {
                p0: unkown1,
            },
            bb1: SparseBitMatrix(1x2) {},
        }"#]];

    check(src, data_flow_result)
}
