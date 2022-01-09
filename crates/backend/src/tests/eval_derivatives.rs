//! The tests here are pretty excessive
//! However in the derivatives and const eval typos can happen easily and lurk (a lot of the trig
//! functions are rarely used) but when they are used the errors can be quite bad (silently wrong
//! derivatives). Therefore significant testing is important here.
//!
//! Note: These tests do not include any control flow. Derivatives are not control flow
//! transformations while const eval only transforms control flow for const branches (not affected
//! by derivatives). Therefore testing control flow is better left to the approriate unit tests.
//! These tests aim to prevent typos in derivatives (sign mixup etc.) and const eval (like sin used
//! to eval cos)

use auto_diff::auto_diff;
use cfg::{smallvec, ControlFlowGraph, InstrDst, Local, Op};
use float_cmp::{assert_approx_eq, F64Margin};
use program_dependence::{use_def, AssigmentInterner, ProgramDependenGraph};

fn check(cfg: &str, params: &[f64], expected: &[(Local, f64)]) {
    let (mut cfg, _literals) = ControlFlowGraph::parse(cfg).unwrap();
    cfg.blocks.last_mut().unwrap().instructions.push(cfg::Instruction {
        dst: InstrDst::Place(0u32.into()),
        op: Op::Copy,
        args: smallvec![expected.last().unwrap().0.into()],
        src: 0,
    });
    cfg.next_place += 1u32;

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

    let params = params.iter().enumerate().map(|(i, val)| (i.into(), (*val).into())).collect();
    let res = const_eval::conditional_const_propagation(&mut cfg, &params);
    for (local, expected) in expected {
        let val = res.analysis.ssa_consts.get_option(*local);
        eprintln!("eval {:?}={:?} (expected {})", local, val, expected);
        let val = val.expect("failed to const eval").unwrap_real();
        // FIXME custom margin for every test?
        let margin = F64Margin::default().ulps(9);
        assert_approx_eq!(f64, val, *expected, margin)
    }
}

#[test]
fn second_order_exp() {
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
    let param0 = 3f64;
    let param1 = 2f64;
    let res = param0.exp() * param1.exp();
    check(src, &[param0, param1], &[(3u32.into(), res), (5u32.into(), res)])
}

#[test]
fn third_order_ln_sin_exp() {
    let src = r##"
        {
        next_local _7;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _1 := sin [#0];
            let _2 := f64.* [_0,_1];
            let _3 := ln [_2];
            let _4 := cb0 [_3];
            let _5 := cb0 [_4];
            let _6 := cb0 [_5];
            end
        }"##;
    let param0 = 8f64;
    let res = (param0.exp() * param0.sin()).ln();
    let derivative_1 = 1.0 + 1.0 / param0.tan();
    let derivative_2 = -1.0 / (param0.sin() * param0.sin());
    let derivative_3 = 2.0 / (param0.tan() * param0.sin() * param0.sin());
    check(
        src,
        &[param0],
        &[
            (3u32.into(), res),
            (4u32.into(), derivative_1),
            (5u32.into(), derivative_2),
            (6u32.into(), derivative_3),
        ],
    )
}

#[test]
fn third_order_ln_sinh_exp() {
    let src = r##"
        {
        next_local _7;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _1 := sinh [#0];
            let _2 := f64.* [_0,_1];
            let _3 := ln [_2];
            let _4 := cb0 [_3];
            let _5 := cb0 [_4];
            let _6 := cb0 [_5];
            end
        }"##;
    let param0 = 0.8f64;
    let res = (param0.exp() * param0.sinh()).ln();
    let derivative_1 = 1.0 + 1.0 / param0.tanh();
    let derivative_2 = -1.0 / (param0.sinh() * param0.sinh());
    let derivative_3 = 2.0 / (param0.tanh() * param0.sinh() * param0.sinh());
    check(
        src,
        &[param0],
        &[
            (3u32.into(), res),
            (4u32.into(), derivative_1),
            (5u32.into(), derivative_2),
            (6u32.into(), derivative_3),
        ],
    )
}

#[test]
fn third_order_exp_x() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := exp [#0];
            let _1 := copy [#0];
            let _2 := f64./ [_0,_1];
            let _3 := cb0 [_2];
            let _4 := cb0 [_3];
            let _5 := cb0 [_4];
            end
        }"##;
    let x = 0.8f64;
    let res = x.exp() / x;
    let derivative_1 = res - res / x;
    let derivative_2 = (x * x - 2.0 * x + 2.0) * res / x / x;
    let derivative_3 = (x * x * x - 3.0 * x * x + 6.0 * x - 6.0) * res / x / x / x;
    check(
        src,
        &[x],
        &[
            (2u32.into(), res),
            (3u32.into(), derivative_1),
            (4u32.into(), derivative_2),
            (5u32.into(), derivative_3),
        ],
    )
}

#[test]
fn third_order_asin() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := asin [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 0.8f64;
    let res = x.asin();
    let derivative_3 = (2.0 * x * x + 1.0) / (1.0 - x * x).powf(5.0 / 2.0);
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}

#[test]
fn third_order_acos() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := acos [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 0.8f64;
    let res = x.acos();
    let derivative_3 = -(2.0 * x * x + 1.0) / (1.0 - x * x).powf(5.0 / 2.0);
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}

#[test]
fn third_order_acosh() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := acosh [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 2f64;
    let res = x.acosh();
    let derivative_3 = (2.0 * x * x + 1.0) / (x * x - 1.0).powf(5.0 / 2.0);
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}

#[test]
fn third_order_asinh() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := asinh [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 2f64;
    let res = x.asinh();
    let derivative_3 = (2.0 * x * x - 1.0) / (1.0 + x * x).powf(5.0 / 2.0);
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}

#[test]
fn third_order_tan() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := tan [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 2f64;
    let res = x.tan();
    let derivative_1 = 1.0 / x.cos().powi(2);
    let derivative_3 = (4.0 * x.sin().powi(2) + 2.0) / x.cos().powi(4);
    check(
        src,
        &[x],
        &[(0u32.into(), res), (1u32.into(), derivative_1), (3u32.into(), derivative_3)],
    )
}

#[test]
fn third_order_tanh() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := tanh [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 2f64;
    let res = x.tanh();
    let derivative_3 = (4.0 * x.sinh().powi(2) - 2.0) / x.cosh().powi(4);
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}

#[test]
fn third_order_atan() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := atan [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 2f64;
    let res = x.atan();
    let derivative_3 = (6.0 * x.powi(2) - 2.0) / (x * x + 1.0).powi(3);
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}

#[test]
fn third_order_atanh() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := atanh [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 2f64;
    let res = x.atanh();
    let derivative_3 = -(6.0 * x.powi(2) + 2.0) / (x * x - 1.0).powi(3);
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}

#[test]
fn third_order_log10() {
    let src = r##"
        {
        next_local _6;
        next_place p0;
        bb0:
            let _0 := log [#0];
            let _1 := cb0 [_0];
            let _2 := cb0 [_1];
            let _3 := cb0 [_2];
            end
        }"##;
    let x = 2f64;
    let res = x.log10();
    let derivative_3 = 2.0 / 10f64.ln() / x / x / x;
    check(src, &[x], &[(0u32.into(), res), (3u32.into(), derivative_3)])
}
