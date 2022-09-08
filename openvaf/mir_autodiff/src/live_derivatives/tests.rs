use std::fmt::{self, Debug};

use bitset::{HybridBitSet, SparseBitMatrix};
use expect_test::{expect, Expect};
use mir::{ControlFlowGraph, DominatorTree, Function, Inst, KnownDerivatives, Value};
use mir_reader::parse_function;

use crate::intern::Derivative;
use crate::{DerivativeIntern, LiveDerivatives};

struct DerivativeFmt<'a> {
    func: &'a Function,
    derivatives: &'a SparseBitMatrix<Inst, Derivative>,
}

impl Debug for DerivativeFmt<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        /// Forces its contents to print in regular mode instead of alternate mode.
        struct OneLinePrinter(Value, Derivative);
        impl fmt::Debug for OneLinePrinter {
            fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(fmt, "{}: {:?}", self.0, self.1)
            }
        }

        write!(
            fmt,
            "SparseBitMatrix({}x{}) ",
            self.derivatives.num_rows(),
            self.derivatives.num_columns()
        )
        .unwrap();
        let items = self.derivatives.rows().flat_map(|inst| {
            self.derivatives.iter(inst).map(move |unkown| {
                let val = self.func.dfg.first_result(inst);
                OneLinePrinter(val, unkown)
            })
        });
        fmt.debug_set().entries(items).finish()
    }
}

fn check(src: &str, data_flow_result: Expect) {
    let (func, _) = parse_function(src).unwrap();

    let unkowns = [10u32.into(), 11u32.into()].into_iter().collect();

    let mut call1 = HybridBitSet::new_empty();
    call1.insert(0u32.into(), 2);
    let mut call2 = HybridBitSet::new_empty();
    call2.insert(1u32.into(), 2);
    let mut call3_pos = HybridBitSet::new_empty();
    call3_pos.insert(0u32.into(), 2);
    let mut call3_neg = HybridBitSet::new_empty();
    call3_neg.insert(1u32.into(), 2);
    let ddx_calls = [
        (0u32.into(), (call1, HybridBitSet::new_empty())),
        (1u32.into(), (call2, HybridBitSet::new_empty())),
        (2u32.into(), (call3_pos, call3_neg)),
    ]
    .into_iter()
    .collect();

    let derivative_info = KnownDerivatives { unknowns: unkowns, ddx_calls };
    let mut unkowns = DerivativeIntern::new(&derivative_info);

    let mut cfg = ControlFlowGraph::new();
    cfg.compute(&func);
    let mut dom_tree = DominatorTree::default();
    dom_tree.compute(&func, &cfg, true, false, true);

    let res = LiveDerivatives::build(&func, &mut unkowns, &[], &dom_tree);
    let printer = DerivativeFmt { func: &func, derivatives: &res.mat };

    let actual = format!("{:#?}", printer);
    data_flow_result.assert_eq(&actual);
}

#[test]
fn smoke_test() {
    let src = r##"
        function %bar(v10, v11, v12) {
            fn0 = const fn %ddx_v10(1) -> 1
            fn1 = const fn %ddx_v11(1) -> 1

        block0:
            v20 = fdiv v10, v4
            v40 = fdiv v12, v12
            v13 = fdiv v40, v20
            v21 = fadd v5, v12 
            v16 = fmul v4, v11
            jmp block1

        block1:
            v14 = flt v7, v10
            v22 = fadd v10, v11
            br v14, block2, block4

        block2:
            v15 = fmul v10, v16
            v23 = fadd v5, v12 
            jmp block3

        block3:
            v24 = fadd v5, v12 
            v25 = fadd v5, v19 
            jmp block4

        block4:
            v30 = phi [v22, block1], [v15, block3]
            v31 = call fn1 (v30)
            v32 = fadd v31, v13
            v33 = call fn0 (v32)

        }
    "##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(19x3) {
            v20: derivative0,
            v13: derivative0,
            v16: derivative1,
            v22: derivative0,
            v22: derivative1,
            v15: derivative0,
            v15: derivative1,
            v15: derivative2,
            v30: derivative0,
            v30: derivative1,
            v30: derivative2,
            v31: derivative0,
            v31: derivative1,
            v31: derivative2,
            v32: derivative0,
            v33: derivative0,
        }"#]];

    check(src, data_flow_result)
}

#[test]
fn multi_derivative() {
    let src = r##"
        function %bar(v10, v11, v12) {
            fn0 = const fn %ddx_v10(1) -> 1
            fn1 = const fn %ddx_v11(1) -> 1
            fn2 = const fn %ddx_node1(1) -> 1

        block0:
            v20 = fdiv v10, v4
            v27 = fdiv v12, v12
            v26 = fmul v20, v11
            v21 = fadd v27, v26
            v22 = fadd v40, v21
            v23 = call fn2 (v22)

        }
    "##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(6x2) {
            v20: derivative0,
            v26: derivative0,
            v26: derivative1,
            v21: derivative0,
            v21: derivative1,
            v22: derivative0,
            v22: derivative1,
            v23: derivative0,
            v23: derivative1,
        }"#]];

    check(src, data_flow_result)
}

/// back edges always work because live derivatives just takes a union of the arguments
/// Because loops must not contain ddx functions all phis with backward edges would always end up
/// as unions of identical sets
///
/// but its nice to test anyway
///
/// even this simple testcase can't be handeled without SSA/DFA while its trivial with this
/// implementation
#[test]
fn back_edge() {
    let src = r##"
        function %bar(v10, v11, v12) {
            fn0 = const fn %ddx_v10(1) -> 1
            fn1 = const fn %ddx_v11(1) -> 1

        block0:
            v20 = fdiv v10, v4
            jmp block1

        block1:
            v21 = phi [v20, block1], [v22, block2]
            br v12, block2, block3

        block2:
            v23 = fadd v20, v20
            v22 = fadd v21, v23

        block3:
            v31 = call fn0 (v21)
        }
    "##;

    let data_flow_result = expect![[r#"
        SparseBitMatrix(7x2) {
            v20: derivative0,
            v21: derivative0,
            v23: derivative0,
            v22: derivative0,
            v31: derivative0,
        }"#]];

    check(src, data_flow_result)
}
