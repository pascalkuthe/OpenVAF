use bitset::BitSet;
use cfg::ControlFlowGraph;
use expect_test::{expect, Expect};

use crate::use_def::DepthFirstSearch;
use crate::{AssigmentInterner, ProgramDependenGraph};

fn verify(cfg: &str, expect: Expect) {
    let (mut cfg, literals) = ControlFlowGraph::parse(cfg).unwrap();
    let assignments = AssigmentInterner::new(&cfg);
    let mut pdg = ProgramDependenGraph::build(&assignments, &cfg);
    let mut work_queue = DepthFirstSearch::new(&pdg);
    let mut places = BitSet::new_empty(cfg.next_place.into());
    places.insert(0u32.into());
    work_queue.walk_places::<_, true>(places.iter(), &pdg, &cfg);
    work_queue.remove_unvisited_from_cfg(&mut cfg, &assignments, Some(&mut pdg));
    let cfg = cfg.dump(Some(&literals));
    expect.assert_eq(&cfg)
}

#[test]
fn smoke_test() {
    let cfg = r##"
    {
    next_local _8;
    next_place p2;
    bb0:
        let p1 := copy [f64 0.0];
        let p1 := copy [f64 1.0];
        let _0 := f64.!= [#4, f64 1.0];
        let _2 := f64.+ [f64 2.0, #0];
        if _0 { bb1 } else { bb2 }
    bb1:
        let _1 := f64.* [_2,#2];
        let p0 := f64.+ [_1, p1];
        let _3 := f64./ [p0, #1];
        let p1 := f64.+ [_3, _1];
        goto bb6;
    bb2:
        let _4 := f64.!= [#2, f64 1.0];
        if _4 { bb3 } else { bb4 }
    bb3:
        let _5 := copy [f64 5.0];
        goto bb5;
    bb4:
        let _6 := copy [f64 2.0];
        goto bb5;
    bb5:
        phi _7 := [(bb3,_5),(bb4,_6)];
        let p0 := copy [_7];
        goto bb6;
    bb6:
        end
    }"##;

    let expect = expect![[r##"
        {
        next_local _8;
        next_place p2;
        bb0:
            noop;
            let p1 := copy [f64 1.0];
            let _0 := f64.!= [#4, f64 1.0];
            let _2 := f64.+ [f64 2.0, #0];
            if _0 { bb1 } else { bb2 } 
        bb1:
            let _1 := f64.* [_2, #2];
            let p0 := f64.+ [_1, p1];
            noop;
            noop;
            goto bb6;
        bb2:
            let _4 := f64.!= [#2, f64 1.0];
            if _4 { bb3 } else { bb4 } 
        bb3:
            let _5 := copy [f64 5.0];
            goto bb5;
        bb4:
            let _6 := copy [f64 2.0];
            goto bb5;
        bb5:
            phi _7 := [(bb3, _5), (bb4, _6)];
            let p0 := copy [_7];
            goto bb6;
        bb6:
            end
        }"##]];

    verify(cfg, expect);
}

#[test]
fn loop_tail_dependence() {
    let cfg = r##"
    {
    next_local _2;
    next_place p2;
    bb0:
        let p0 := copy [f64 3.141];
        let p1 := copy [i32 1];
        let p2 := copy [f64 2.0];
        goto bb1;
    bb1:
        let _0 := i32.< [p1, i32 100];
        if _0 { bb2 } else { bb3 }
    bb2:
        let p0 := f64.+ [p0, p2];
        let p2 := f64.* [p2, f64 100.0];
        goto bb1;
    bb3:
        let p2 := copy [f64 1.0];
        end
    }"##;

    let expect = expect![[r#"
        {
        next_local _2;
        next_place p2;
        bb0:
            let p0 := copy [f64 3.141];
            let p1 := copy [i32 1];
            let p2 := copy [f64 2.0];
            goto bb1;
        bb1:
            let _0 := i32.< [p1, i32 100];
            if _0 { bb2 } else { bb3 } 
        bb2:
            let p0 := f64.+ [p0, p2];
            let p2 := f64.* [p2, f64 100.0];
            goto bb1;
        bb3:
            noop;
            end
        }"#]];

    verify(cfg, expect);
}
