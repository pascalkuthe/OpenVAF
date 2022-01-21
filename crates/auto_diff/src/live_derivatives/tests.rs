use cfg::ControlFlowGraph;
use expect_test::{expect, Expect};
use program_dependence::{use_def, AssigmentInterner, ProgramDependenGraph};
use stdx::format_to;
use stdx::iter::zip;

use crate::{LiveDerivatives, Unkowns};

fn check(src: &str, data_flow_result: Expect) {
    let (cfg, _) = ControlFlowGraph::parse(src).unwrap();

    let mut unkowns = Unkowns::new([
        (0u32.into(), vec![(0u32.into(), 1f64.to_bits())].into_boxed_slice()),
        (1u32.into(), vec![(1u32.into(), 1f64.to_bits())].into_boxed_slice()),
    ]);

    let assignments = AssigmentInterner::new(&cfg);
    let pdg = ProgramDependenGraph::build(&assignments, &cfg);
    let mut dfs = use_def::DepthFirstSearch::new(&pdg);
    dfs.walk_place::<true>(0u32.into(), &pdg, &cfg);
    let res = LiveDerivatives::build(&dfs, &cfg, &pdg, 4, &mut unkowns, []);

    let mut actual = format!("{:#?}\n{{\n", res.local_derivatives);
    for (loc, unkowns) in
        zip(assignments.assigment_locations.raw.iter(), res.assign_derivatives.row_data())
    {
        for unkown in unkowns.iter() {
            format_to!(actual, "\t{:?}: {:?},\n", loc, unkown);
        }
    }
    actual.push('}');
    data_flow_result.assert_eq(&actual);
}

#[test]
fn smoke_test() {
    let src = r##"
        {
        next_local _11;
        next_place p3;
        bb0:
            let p2 := copy [f64 0.0];
            let _0 := f64.- [#0];
            let _1 := cb2 [#1, f64 3.141];
            let _2 := f64./ [#0, f64 3.141];
            let _3 := sin [#3];
            let _4 := f64.!= [#2, f64 1.0];
            if _4 { bb1 } else { bb2 }
        bb1:
            let _5 := f64.* [_1,#5];
            let p2 := copy [_2];
            goto bb3;
        bb2:
            let _6 := copy [_3];
            goto bb3;
        bb3:
            phi _7 := [(bb1,_5),(bb2,_6)];
            let _8 := cb0 [_7];
            let _9 := f64.+ [p2,_8];
            let p0 := cb1 [_9];
            end
        }"##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(11x2) {
            _1: unkown0,
            _1: unkown1,
            _1: unkown2,
            _2: unkown1,
            _5: unkown0,
            _5: unkown1,
            _5: unkown2,
            _7: unkown0,
            _7: unkown1,
            _7: unkown2,
            _8: unkown0,
            _8: unkown1,
            _8: unkown2,
            _9: unkown1,
        }
        {
        	bb0 -> Idx::<Instruction>(0): unkown1,
        	bb1 -> Idx::<Instruction>(1): unkown1,
        	bb3 -> Idx::<Instruction>(2): unkown1,
        }"#]];

    check(src, data_flow_result)
}

#[test]
fn loop_with_backwards_dependency() {
    let src = r##"
        {
        next_local _4;
        next_place p3;
        bb0:
            let p1 := copy [#1];
            let _1 := f64.+ [f64 3141, #0];
            goto bb1;
        bb1:
            let p1 := i32.- [p1, i32 10];
            goto bb2;
        bb2:
            let _0 := i32.< [p1,i32 10];
            if _0 { bb3 } else{ bb4 } (loop)
        bb3: 
            let _2 := cast_i32_f64 [p1];
            let p1 := i32.+ [p1,i32 20];
            let p2 := f64.* [_2,_1];
            goto bb2;
        bb4:
            let p0 := cb0 [p2]; 
            end
        }"##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(4x2) {
            _1: unkown0,
            _2: unkown0,
        }
        {
        	bb0 -> Idx::<Instruction>(0): unkown0,
        	bb1 -> Idx::<Instruction>(0): unkown0,
        	bb3 -> Idx::<Instruction>(1): unkown0,
        	bb3 -> Idx::<Instruction>(2): unkown0,
        	bb4 -> Idx::<Instruction>(0): unkown0,
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
        	bb0 -> Idx::<Instruction>(0): unkown1,
        	bb1 -> Idx::<Instruction>(0): unkown1,
        	bb1 -> Idx::<Instruction>(1): unkown1,
        }"#]];

    check(src, data_flow_result)
}
