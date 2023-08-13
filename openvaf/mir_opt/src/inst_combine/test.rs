use expect_test::{expect, Expect};
use mir::ControlFlowGraph;
use mir_reader::parse_function;

use crate::inst_combine;

fn check(src: &str, data_flow_result: Expect) {
    let (mut func, _) = parse_function(src).unwrap();
    let mut cfg = ControlFlowGraph::new();
    cfg.compute(&func);
    inst_combine(&mut func);
    data_flow_result.assert_eq(&func.to_debug_string());
}

#[test]
fn diode() {
    let raw = r#"
        function %bar(v10, v11, v12) {
            fn0 = const fn %ddx_v10(1) -> 1
            fn1 = const fn %ddx_v11(1) -> 1
            v3 = fconst 0.0
            v6 = fconst 0x1.0000000000000p0

        block0:
            v13 = fadd v11, v11
            v102 = fadd v6, v6
            v14 = fadd v11, v11
            v103 = fadd v6, v6
            v20 = fmul v12, v10
            v21 = fdiv v20, v14
            v104 = fmul v14, v14
            v105 = fmul v103, v20
            v106 = fdiv v105, v104
            v107 = fsub v3, v106
            v22 = exp v21
            v108 = fmul v107, v22
            v23 = fmul v13, v22
            v109 = fmul v102, v22
            v110 = fmul v108, v13
            v111 = fadd v109, v110
            v101 = optbarrier v111
        }
    "#;
    let expect = expect![[r#"
        function %bar(v10, v11, v12) {
            inst0 = const fn %ddx_v10(1) -> 1
            inst1 = const fn %ddx_v11(1) -> 1
            v3 = fconst 0.0

        block0:
            v13 = fadd v11, v11
            v14 = fadd v11, v11
            v20 = fmul v12, v10
            v21 = fdiv v20, v14
            v104 = fmul v14, v14
            v105 = fmul v11, v20
            v106 = fdiv v105, v104
            v107 = fsub v3, v106
            v22 = exp v21
            v108 = fmul v107, v22
            v23 = fmul v13, v22
            v109 = fmul v11, v22
            v110 = fmul v108, v13
            v111 = fadd v109, v110
            v101 = optbarrier v111
        }
    "#]];
    check(raw, expect)
}
