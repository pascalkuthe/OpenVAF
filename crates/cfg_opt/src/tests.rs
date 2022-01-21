use cfg::ControlFlowGraph;
use expect_test::{expect, Expect};

use crate::{simplify_branches, simplify_cfg};

fn check(raw: &str, propagted: Expect) {
    let (mut cfg, literals) = ControlFlowGraph::parse(raw).unwrap();
    simplify_branches(&mut cfg);
    simplify_cfg(&mut cfg);
    cfg.assert_verified();
    // remove_dead_data(&mut cfg, param_cnt);
    let dump = cfg.dump(Some(&literals));
    propagted.assert_eq(&dump);
}

#[test]
fn dead_phis() {
    let cfg = r##"
        {
        next_local _8;
        next_place p1;
        bb0:
            goto bb2;
        bb1:
            let _0 := copy [#1];
            goto bb3;
        bb2:
            let _1 := copy [#2];
            goto bb3;

        bb3:
            let _2 := copy [#0];
            goto bb3;
        bb4:
            phi _2 := [(bb1,_0),(bb2,_1),(bb3,_2)];
            end
        }"##;

    let simplified_cfg = expect![[r##"
        {
        next_local _8;
        next_place p1;
        bb0:
            let _1 := copy [#2];
            goto bb1;
        bb1:
            let _2 := copy [#0];
            goto bb1;
        }"##]];
    check(cfg, simplified_cfg)
}

#[test]
fn non_local_phis() {
    let cfg = r##"
        {
        next_local _2;
        next_place p1;
        bb0:
            let _0 := copy [#1];
            if #2 { bb1 } else { bb2 }
        bb1:
            goto bb3;
        bb2:
            let _1 := copy [#3];
            goto bb3;

        bb3:
            phi _2 := [(bb1,_0),(bb2,_1)];
            end
        }"##;

    let simplified_cfg = expect![[r##"
        {
        next_local _2;
        next_place p1;
        bb0:
            let _0 := copy [#1];
            if #2 { bb1 } else { bb2 } 
        bb1:
            goto bb3;
        bb2:
            let _1 := copy [#3];
            goto bb3;
        bb3:
            phi _2 := [(bb1, _0), (bb2, _1)];
            end
        }"##]];
    check(cfg, simplified_cfg)
}

#[test]
fn const_phis_chained() {
    let cfg = r##"
        {
        next_local _8;
        next_place p1;
        bb0:
            if true { bb1 } else { bb5 }
        bb1:
            let p0 := copy [f64 3.141];
            if true { bb2 } else { bb3 }
        bb2:
            let p0 := f64.* [p0,f64 3.141];
            let _7 := copy [#1];
            goto bb4;
        bb3:
            let p0 := f64./ [p0,f64 3.141];
            let _8 := copy [#2];
            goto bb4;
        bb4:
            phi _1 := [(bb2,_7),(bb3,_8)];
            let p0 := f64.+ [p0,#3];
            goto bb6;
        bb5:
            let p0 := copy [f64 1.0];
            let _2 := copy [f64 1.0];
            goto bb6;
        bb6:
            phi _3 := [(bb4,_1),(bb5,_2)];
            let _4 := copy [p0];
            let _5 := copy [_3];
            end
        }"##;

    let simplified_cfg = expect![[r##"
        {
        next_local _8;
        next_place p1;
        bb0:
            let p0 := copy [f64 3.141];
            let p0 := f64.* [p0, f64 3.141];
            let _7 := copy [#1];
            let _1 := copy [_7];
            let p0 := f64.+ [p0, #3];
            let _3 := copy [_1];
            let _4 := copy [p0];
            let _5 := copy [_3];
            end
        }"##]];
    check(cfg, simplified_cfg)
}

#[test]
fn const_phis() {
    let cfg = r##"
        {
        next_local _8;
        next_place p1;
        bb0:
            if #4 { bb1 } else { bb5 }
        bb1:
            let p0 := copy [f64 3.141];
            if true { bb2 } else { bb3 }
        bb2:
            let p0 := f64.* [p0,f64 3.141];
            let _7 := copy [#1];
            goto bb6;
        bb3:
            let p0 := f64./ [p0,f64 3.141];
            let _8 := copy [#2];
            goto bb4;
        bb4:
            phi _1 := [(bb6,_7),(bb3,_8)];
            let p0 := f64.+ [p0,#3];
            goto bb7;
        bb5:
            let p0 := copy [f64 1.0];
            let _2 := copy [f64 1.0];
            goto bb7;
        bb6:
            goto bb4;
        bb7:
            phi _3 := [(bb4,_1),(bb5,_2)];
            let _4 := copy [p0];
            let _5 := copy [_3];
            end
        }"##;

    let simplified_cfg = expect![[r##"
        {
        next_local _8;
        next_place p1;
        bb0:
            if #4 { bb1 } else { bb2 } 
        bb1:
            let p0 := copy [f64 3.141];
            let p0 := f64.* [p0, f64 3.141];
            let _7 := copy [#1];
            let _1 := copy [_7];
            let p0 := f64.+ [p0, #3];
            goto bb3;
        bb2:
            let p0 := copy [f64 1.0];
            let _2 := copy [f64 1.0];
            goto bb3;
        bb3:
            phi _3 := [(bb1, _1), (bb2, _2)];
            let _4 := copy [p0];
            let _5 := copy [_3];
            end
        }"##]];
    check(cfg, simplified_cfg)
}

#[test]
fn goto_chain() {
    let cfg = r##"
        {
        next_local _1;
        next_place p1;
        bb0:
            let p0 := copy [f64 2.0];
            if #4 { bb1 } else { bb2 }
        bb1:
            let p0 := copy [f64 3.141];
            goto bb3;
        bb2:
            goto bb3;
        bb3:
            goto bb4;
        bb4:
            let _0 := copy [p0];
            end
        }"##;

    let simplified_cfg = expect![[r##"
        {
        next_local _1;
        next_place p1;
        bb0:
            let p0 := copy [f64 2.0];
            if #4 { bb1 } else { bb2 } 
        bb1:
            let p0 := copy [f64 3.141];
            goto bb2;
        bb2:
            let _0 := copy [p0];
            end
        }"##]];
    check(cfg, simplified_cfg)
}
