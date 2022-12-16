use bitset::HybridBitSet;
use expect_test::{expect, Expect};
use float_cmp::{ApproxEq, F64Margin};
use mir::{ControlFlowGraph, DominatorTree, KnownDerivatives};
use mir_interpret::{Data, Interpreter};
use mir_reader::parse_function;
use typed_index_collections::TiSlice;

use crate::auto_diff;

fn check_simple(src: &str, data_flow_result: Expect) {
    let (mut func, _) = parse_function(src).unwrap();
    let mut cfg = ControlFlowGraph::new();
    cfg.compute(&func);
    let mut dom_tree = DominatorTree::default();
    dom_tree.compute(&func, &cfg, true, false, true);

    let unknowns = [10u32.into(), 11u32.into(), 12u32.into()].into_iter().collect();

    let mut call1 = HybridBitSet::new_empty();
    call1.insert(0u32.into(), 3);
    let mut call2 = HybridBitSet::new_empty();
    call2.insert(1u32.into(), 3);
    let mut call3 = HybridBitSet::new_empty();
    call3.insert(2u32.into(), 3);
    let ddx_calls = [
        (0u32.into(), (call1, HybridBitSet::new_empty())),
        (1u32.into(), (call2, HybridBitSet::new_empty())),
        (2u32.into(), (call3, HybridBitSet::new_empty())),
    ]
    .into_iter()
    .collect();

    let unknowns = KnownDerivatives { unknowns, ddx_calls };

    auto_diff(&mut func, &dom_tree, &unknowns, &[]);
    data_flow_result.assert_eq(&func.to_debug_string());
}

fn check_num(src: &str, data_flow_result: Expect, args: &[f64], num: f64) {
    let (mut func, _) = parse_function(src).unwrap();
    let mut cfg = ControlFlowGraph::new();
    cfg.compute(&func);

    let unkowns = [10u32.into(), 11u32.into()].into_iter().collect();

    let mut call1 = HybridBitSet::new_empty();
    call1.insert(0u32.into(), 2);
    let mut call2 = HybridBitSet::new_empty();
    call2.insert(1u32.into(), 2);
    let ddx_calls = [
        (0u32.into(), (call1, HybridBitSet::new_empty())),
        (1u32.into(), (call2, HybridBitSet::new_empty())),
    ]
    .into_iter()
    .collect();

    let unkowns = KnownDerivatives { unknowns: unkowns, ddx_calls };

    let mut dom_tree = DominatorTree::default();
    dom_tree.compute(&func, &cfg, true, false, true);
    auto_diff(&mut func, &dom_tree, &unkowns, &[]);
    let mut interpret = Interpreter::new(
        &func,
        TiSlice::from_ref(&[]),
        TiSlice::from_ref(Data::from_f64_slice(args)),
    );
    interpret.run();
    let val: f64 = interpret.state.read(100u32.into());
    // we use mathmatically simplified formulations which can round quite differently so use a more
    // generate epsilon (relative error of 1.6e-15 is fine)
    let margin = F64Margin::default().epsilon(10f64 * f64::EPSILON);
    if !val.approx_eq(num, margin) {
        eprintln!(
            "\x1b[31;1merror\x1b: auto_diff result {} does not match expected value {}",
            val, num
        );
        data_flow_result.assert_eq(&func.to_debug_string());
        unreachable!("invalid value produced by autodiff")
    } else {
        data_flow_result.assert_eq(&func.to_debug_string());
    }
}

#[test]
fn phi() {
    let src = r##"
        function %bar(v10, v11, v12) {
            fn0 = const fn %ddx_v10(1) -> 1
            fn1 = const fn %ddx_v11(1) -> 1

        block0:
            br v12, block1, block2

        block1:
            v13 = exp v10
            jmp block3

        block2:
            v14 = exp v11
            jmp block3

        block3:
            v15 = phi [v13, block1], [v14, block2]
            v16 = call fn1 (v15)
            v17 = call fn0 (v16)
            v100 = optbarrier v17
        }"##;
    let expect = expect![[r#"
        function %bar(v10, v11, v12) {
            inst0 = const fn %ddx_v10(1) -> 1
            inst1 = const fn %ddx_v11(1) -> 1
            v3 = fconst 0.0

        block0:
            br v12, block1, block2

        block1:
            v13 = exp v10
            jmp block3

        block2:
            v14 = exp v11
            jmp block3

        block3:
            v15 = phi [v13, block1], [v14, block2]
            v101 = phi [v13, block1], [v3, block2]
            v102 = phi [v3, block1], [v14, block2]
            v100 = optbarrier v3
        }
    "#]];
    check_simple(src, expect);
}

#[test]
fn exp_second_order() {
    let src = r##"
        function %bar(v10, v11, v12) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v13 = exp v10
            v16 = fmul v10, v11
            v17 = exp v16
            v18 = fadd v13, v17
            v14 = call fn0 (v18)
            v15 = call fn0 (v14)
            v100 = optbarrier v15
        }"##;
    let expect = expect![[r#"
        function %bar(v10, v11, v12) {
            inst0 = const fn %ddx_v10(1) -> 1

        block0:
            v13 = exp v10
            v16 = fmul v10, v11
            v17 = exp v16
            v101 = fmul v11, v17
            v102 = fmul v101, v11
            v18 = fadd v13, v17
            v103 = fadd v13, v101
            v104 = fadd v13, v102
            v100 = optbarrier v104
        }
    "#]];
    check_simple(src, expect);
}

#[test]
fn sin_second_order() {
    let src = r##"
        function %bar(v10, v11) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = fmul v10, v11
            v13 = sin v12
            v14 = call fn0 (v13)
            v15 = call fn0 (v14)
            v100 = optbarrier v15
        }"##;
    let expect = expect![[r#"
        function %bar(v10, v11) {
            inst0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = fmul v10, v11
            v13 = sin v12
            v101 = cos v12
            v102 = fmul v11, v101
            v103 = sin v12
            v104 = fneg v103
            v105 = fmul v11, v104
            v106 = fmul v105, v11
            v100 = optbarrier v106
        }
    "#]];
    check_simple(src, expect);
}

#[test]
fn sin_exp_second_order() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = sin v10
            v13 = exp v10
            v16= fmul v12, v13
            v14 = call fn0 (v16)
            v15 = call fn0 (v14)
            v100 = optbarrier v15
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = sin v10
            v101 = cos v10
            v102 = sin v10
            v103 = fneg v102
            v13 = exp v10
            v16 = fmul v12, v13
            v104 = fmul v101, v13
            v105 = fadd v104, v16
            v106 = fmul v103, v13
            v107 = fadd v106, v104
            v108 = fadd v107, v105
            v100 = optbarrier v108
        }
    "#]];
    check_simple(src, expect);
}

#[test]
fn third_order_ln_sin_exp() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = sin v10
            v13 = exp v10
            v16 = fmul v12, v13
            v17= ln v16
            v14 = call fn0 (v17)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = sin v10
            v101 = cos v10
            v102 = sin v10
            v103 = fneg v102
            v104 = cos v10
            v105 = fneg v104
            v13 = exp v10
            v16 = fmul v12, v13
            v106 = fmul v101, v13
            v107 = fadd v106, v16
            v108 = fmul v103, v13
            v109 = fadd v108, v106
            v110 = fadd v109, v107
            v111 = fmul v105, v13
            v112 = fadd v111, v108
            v113 = fadd v112, v109
            v114 = fadd v113, v110
            v17 = ln v16
            v115 = fdiv v107, v16
            v116 = fmul v16, v16
            v117 = fdiv v110, v16
            v118 = fmul v107, v107
            v119 = fdiv v118, v116
            v120 = fsub v117, v119
            v121 = fmul v16, v16
            v122 = fmul v116, v116
            v123 = fmul v107, v16
            v124 = fmul v107, v16
            v125 = fadd v123, v124
            v126 = fdiv v114, v16
            v127 = fmul v107, v110
            v128 = fdiv v127, v121
            v129 = fsub v126, v128
            v130 = fmul v110, v107
            v131 = fmul v110, v107
            v132 = fadd v130, v131
            v133 = fdiv v132, v116
            v134 = fmul v125, v118
            v135 = fdiv v134, v122
            v136 = fsub v133, v135
            v137 = fsub v129, v136
            v100 = optbarrier v137
        }
    "#]];

    let v10 = 1f64;
    let res = 2.0 / (v10.tan() * v10.sin() * v10.sin());

    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_ln_sinh_exp() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = sinh v10
            v13 = exp v10
            v16 = fmul v12, v13
            v17= ln v16
            v14 = call fn0 (v17)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = sinh v10
            v101 = cosh v10
            v102 = sinh v10
            v103 = cosh v10
            v13 = exp v10
            v16 = fmul v12, v13
            v104 = fmul v101, v13
            v105 = fadd v104, v16
            v106 = fmul v102, v13
            v107 = fadd v106, v104
            v108 = fadd v107, v105
            v109 = fmul v103, v13
            v110 = fadd v109, v106
            v111 = fadd v110, v107
            v112 = fadd v111, v108
            v17 = ln v16
            v113 = fdiv v105, v16
            v114 = fmul v16, v16
            v115 = fdiv v108, v16
            v116 = fmul v105, v105
            v117 = fdiv v116, v114
            v118 = fsub v115, v117
            v119 = fmul v16, v16
            v120 = fmul v114, v114
            v121 = fmul v105, v16
            v122 = fmul v105, v16
            v123 = fadd v121, v122
            v124 = fdiv v112, v16
            v125 = fmul v105, v108
            v126 = fdiv v125, v119
            v127 = fsub v124, v126
            v128 = fmul v108, v105
            v129 = fmul v108, v105
            v130 = fadd v128, v129
            v131 = fdiv v130, v114
            v132 = fmul v123, v116
            v133 = fdiv v132, v120
            v134 = fsub v131, v133
            v135 = fsub v127, v134
            v100 = optbarrier v135
        }
    "#]];

    let v10 = 80f64;
    let res = 2.0 / (v10.tanh() * v10.sinh() * v10.sinh());
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_asin() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = asin v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0
            v11 = fconst 0x1.0000000000000p1

        block0:
            v12 = asin v10
            v101 = fmul v10, v10
            v102 = fsub v6, v101
            v103 = sqrt v102
            v104 = fdiv v6, v103
            v105 = fmul v11, v103
            v106 = fmul v103, v103
            v107 = fadd v10, v10
            v108 = fsub v3, v107
            v109 = fdiv v108, v105
            v110 = fmul v109, v6
            v111 = fdiv v110, v106
            v112 = fsub v3, v111
            v113 = fmul v105, v105
            v114 = fmul v106, v106
            v115 = fmul v109, v11
            v116 = fmul v109, v103
            v117 = fmul v109, v103
            v118 = fadd v116, v117
            v119 = fadd v6, v6
            v120 = fsub v3, v119
            v121 = fdiv v120, v105
            v122 = fmul v115, v108
            v123 = fdiv v122, v113
            v124 = fsub v121, v123
            v125 = fmul v124, v6
            v126 = fdiv v125, v106
            v127 = fmul v118, v110
            v128 = fdiv v127, v114
            v129 = fsub v126, v128
            v130 = fsub v3, v129
            v100 = optbarrier v130
        }
    "#]];

    let v10 = 0.5f64;
    let res = (2.0 * v10 * v10 + 1.0) / (1.0 - v10 * v10).powf(5.0 / 2.0);
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_acos() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = acos v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0
            v11 = fconst 0x1.0000000000000p1

        block0:
            v12 = acos v10
            v101 = fmul v10, v10
            v102 = fsub v6, v101
            v103 = sqrt v102
            v104 = fneg v103
            v105 = fdiv v6, v104
            v106 = fmul v11, v103
            v107 = fmul v104, v104
            v108 = fadd v10, v10
            v109 = fsub v3, v108
            v110 = fdiv v109, v106
            v111 = fneg v110
            v112 = fmul v111, v6
            v113 = fdiv v112, v107
            v114 = fsub v3, v113
            v115 = fmul v106, v106
            v116 = fmul v107, v107
            v117 = fmul v110, v11
            v118 = fmul v111, v104
            v119 = fmul v111, v104
            v120 = fadd v118, v119
            v121 = fadd v6, v6
            v122 = fsub v3, v121
            v123 = fdiv v122, v106
            v124 = fmul v117, v109
            v125 = fdiv v124, v115
            v126 = fsub v123, v125
            v127 = fneg v126
            v128 = fmul v127, v6
            v129 = fdiv v128, v107
            v130 = fmul v120, v112
            v131 = fdiv v130, v116
            v132 = fsub v129, v131
            v133 = fsub v3, v132
            v100 = optbarrier v133
        }
    "#]];

    let v10 = 0.5f64;
    let res = -(2.0 * v10 * v10 + 1.0) / (1.0 - v10 * v10).powf(5.0 / 2.0);
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_acosh() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = acosh v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0
            v11 = fconst 0x1.0000000000000p1

        block0:
            v12 = acosh v10
            v101 = fmul v10, v10
            v102 = fsub v101, v6
            v103 = sqrt v102
            v104 = fdiv v6, v103
            v105 = fmul v11, v103
            v106 = fmul v103, v103
            v107 = fadd v10, v10
            v108 = fsub v107, v3
            v109 = fdiv v108, v105
            v110 = fmul v109, v6
            v111 = fdiv v110, v106
            v112 = fsub v3, v111
            v113 = fmul v105, v105
            v114 = fmul v106, v106
            v115 = fmul v109, v11
            v116 = fmul v109, v103
            v117 = fmul v109, v103
            v118 = fadd v116, v117
            v119 = fadd v6, v6
            v120 = fsub v119, v3
            v121 = fdiv v120, v105
            v122 = fmul v115, v108
            v123 = fdiv v122, v113
            v124 = fsub v121, v123
            v125 = fmul v124, v6
            v126 = fdiv v125, v106
            v127 = fmul v118, v110
            v128 = fdiv v127, v114
            v129 = fsub v126, v128
            v130 = fsub v3, v129
            v100 = optbarrier v130
        }
    "#]];

    let v10 = 0.5f64;
    let res = (2.0 * v10 * v10 + 1.0) / (v10 * v10 - 1.0).powf(5.0 / 2.0);
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_tan() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = tan v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0

        block0:
            v12 = tan v10
            v101 = fmul v12, v12
            v102 = fadd v6, v101
            v103 = fmul v102, v12
            v104 = fmul v102, v12
            v105 = fadd v103, v104
            v106 = fadd v3, v105
            v107 = fmul v106, v12
            v108 = fmul v102, v102
            v109 = fadd v107, v108
            v110 = fmul v106, v12
            v111 = fmul v102, v102
            v112 = fadd v110, v111
            v113 = fadd v109, v112
            v114 = fadd v3, v113
            v100 = optbarrier v114
        }
    "#]];

    let v10 = 2f64;
    let res = (4.0 * v10.sin().powi(2) + 2.0) / v10.cos().powi(4);
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_tanh() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = tanh v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0

        block0:
            v12 = tanh v10
            v101 = fmul v12, v12
            v102 = fsub v6, v101
            v103 = fmul v102, v12
            v104 = fmul v102, v12
            v105 = fadd v103, v104
            v106 = fsub v3, v105
            v107 = fmul v106, v12
            v108 = fmul v102, v102
            v109 = fadd v107, v108
            v110 = fmul v106, v12
            v111 = fmul v102, v102
            v112 = fadd v110, v111
            v113 = fadd v109, v112
            v114 = fsub v3, v113
            v100 = optbarrier v114
        }
    "#]];

    let v10 = 2f64;
    let res = (4.0 * v10.sinh().powi(2) - 2.0) / v10.cosh().powi(4);
    check_num(src, expect, &[v10], res);
}

#[test]
fn second_order_pow() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = pow v10, v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v100 = optbarrier v15
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0

        block0:
            v12 = pow v10, v10
            v101 = feq v10, v3
            br v101, block1, block2

        block1:
            v110 = phi [v3, block0], [v104, block2]
            v111 = phi [v3, block0], [v109, block2]
            v100 = optbarrier v111

        block2:
            v102 = ln v10
            v103 = fadd v6, v102
            v104 = fmul v103, v12
            v105 = fdiv v6, v10
            v106 = fadd v3, v105
            v107 = fmul v106, v12
            v108 = fmul v104, v103
            v109 = fadd v107, v108
            jmp block1
        }
    "#]];

    let v10 = 2f64;
    let res = v10.powf(v10) * (v10.ln() + 1.0).powi(2) + v10.powf(v10 - 1f64);
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_atan() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = atan v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0

        block0:
            v12 = atan v10
            v101 = fmul v10, v10
            v102 = fadd v6, v101
            v103 = fdiv v6, v102
            v104 = fmul v102, v102
            v105 = fadd v10, v10
            v106 = fadd v3, v105
            v107 = fmul v106, v6
            v108 = fdiv v107, v104
            v109 = fsub v3, v108
            v110 = fmul v104, v104
            v111 = fmul v106, v102
            v112 = fmul v106, v102
            v113 = fadd v111, v112
            v114 = fadd v6, v6
            v115 = fadd v3, v114
            v116 = fmul v115, v6
            v117 = fdiv v116, v104
            v118 = fmul v113, v107
            v119 = fdiv v118, v110
            v120 = fsub v117, v119
            v121 = fsub v3, v120
            v100 = optbarrier v121
        }
    "#]];

    let v10 = 0.7f64;
    let res = (6.0 * v10.powi(2) - 2.0) / (v10 * v10 + 1.0).powi(3);
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_atanh() {
    let src = r##"
        function %bar(v10) {
            fn0 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = atanh v10
            v14 = call fn0 (v12)
            v15 = call fn0 (v14)
            v18 = call fn0 (v15)
            v100 = optbarrier v18
        }"##;
    let expect = expect![[r#"
        function %bar(v10) {
            inst0 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0

        block0:
            v12 = atanh v10
            v101 = fmul v10, v10
            v102 = fsub v6, v101
            v103 = fdiv v6, v102
            v104 = fmul v102, v102
            v105 = fadd v10, v10
            v106 = fsub v3, v105
            v107 = fmul v106, v6
            v108 = fdiv v107, v104
            v109 = fsub v3, v108
            v110 = fmul v104, v104
            v111 = fmul v106, v102
            v112 = fmul v106, v102
            v113 = fadd v111, v112
            v114 = fadd v6, v6
            v115 = fsub v3, v114
            v116 = fmul v115, v6
            v117 = fdiv v116, v104
            v118 = fmul v113, v107
            v119 = fdiv v118, v110
            v120 = fsub v117, v119
            v121 = fsub v3, v120
            v100 = optbarrier v121
        }
    "#]];

    let v10 = 80f64;
    let res = -(6.0 * v10.powi(2) + 2.0) / (v10 * v10 - 1.0).powi(3);
    check_num(src, expect, &[v10], res);
}

#[test]
fn third_order_log10() {
    let src = r##"
        function %bar(v11) {
            fn1 = const fn %ddx_v10(1) -> 1

        block0:
            v12 = log v11
            v13 = call fn1 (v12)
            v14 = call fn1 (v13)
            v15 = call fn1 (v14)
            v100 = optbarrier v15
        }"##;
    let expect = expect![[r#"
        function %bar(v11) {
            inst0 = const fn %(0) -> 0
            inst1 = const fn %ddx_v10(1) -> 1
            v3 = fconst 0.0
            v10 = fconst 0x1.bcb7b1526e50ep-2

        block0:
            v12 = log v11
            v101 = fdiv v10, v11
            v102 = fmul v11, v11
            v103 = fdiv v10, v102
            v104 = fsub v3, v103
            v105 = fmul v102, v102
            v106 = fadd v11, v11
            v107 = fmul v106, v10
            v108 = fdiv v107, v105
            v109 = fsub v3, v108
            v110 = fsub v3, v109
            v100 = optbarrier v110
        }
    "#]];

    let v11 = 2f64;
    let res = 2.0 / 10f64.ln() / v11 / v11 / v11;
    check_num(src, expect, &[v11], res);
}

#[test]
fn subgraph() {
    let src = r##"
        function %bar(v10, v11, v12) {
            fn0 = const fn %ddx_v10(1) -> 1
            fn1 = const fn %ddx_v11(1) -> 1
            fn2 = const fn %ddx_v12(1) -> 1
            v20 = fconst 0x1.bcb7b1526e50ep-2

        block0:
            v22 = fadd v10, v11
            v14 = fadd v22, v12
            v15 = fmul v14, v20
            v16 = fmul v15, v20
            v17 = fmul v16, v20
            v23 = fmul v22, v20
            v24 = fmul v23, v20
            v25 = fmul v24, v20
            v26 = fmul v25, v20
            v27 = fmul v26, v20
            v18 = fmul v17, v20
            v19 = fmul v18, v20
            v40 = fmul v19, v20
            v41 = fmul v40, v20
            v28 = call fn0 (v41)
            v39 = call fn1 (v41)
            v30 = call fn2 (v41)
            v31 = call fn0 (v27)
            v32 = call fn1 (v27)
            v101 = optbarrier v28
            v102 = optbarrier v29
            v103 = optbarrier v30
            v104 = optbarrier v31
            v105 = optbarrier v32
        }"##;
    let expect = expect![[r#"
        function %bar(v10, v11, v12) {
            inst0 = const fn %ddx_v10(1) -> 1
            inst1 = const fn %ddx_v11(1) -> 1
            inst2 = const fn %ddx_v12(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0
            v20 = fconst 0x1.bcb7b1526e50ep-2

        block0:
            v22 = fadd v10, v11
            v106 = fadd v6, v3
            v107 = fadd v3, v6
            v14 = fadd v22, v12
            v108 = fadd v3, v6
            v109 = fadd v6, v3
            v15 = fmul v14, v20
            v16 = fmul v15, v20
            v110 = fmul v20, v20
            v17 = fmul v16, v20
            v111 = fmul v110, v20
            v23 = fmul v22, v20
            v24 = fmul v23, v20
            v112 = fmul v20, v20
            v25 = fmul v24, v20
            v113 = fmul v112, v20
            v26 = fmul v25, v20
            v114 = fmul v113, v20
            v27 = fmul v26, v20
            v115 = fmul v114, v20
            v116 = fmul v106, v115
            v117 = fmul v107, v115
            v18 = fmul v17, v20
            v118 = fmul v111, v20
            v19 = fmul v18, v20
            v119 = fmul v118, v20
            v40 = fmul v19, v20
            v120 = fmul v119, v20
            v41 = fmul v40, v20
            v121 = fmul v120, v20
            v122 = fmul v108, v121
            v123 = fmul v109, v121
            v124 = fmul v106, v123
            v125 = fmul v107, v123
            v101 = optbarrier v124
            v102 = optbarrier v29
            v103 = optbarrier v122
            v104 = optbarrier v116
            v105 = optbarrier v117
        }
    "#]];

    check_simple(src, expect);
}
