use expect_test::{expect, Expect};
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::ControlFlowGraph;
use mir_reader::parse_function;

use crate::simplify_cfg;

fn expect_test(raw: &str, expect: Expect) {
    let (mut func, _) = parse_function(raw).unwrap();
    let mut cfg = ControlFlowGraph::new();
    cfg.compute(&func);
    simplify_cfg(&mut func, &mut cfg);

    expect.assert_eq(&func.to_debug_string())
}

#[test]
pub fn goto_chain() {
    let raw = r##"
        function %bar(v4, v8, v9, v10) {
        v5 = iconst 42
        v6 = iconst 23
        block0:
            v7 = iadd v5, v8
            jmp block1
        block1:
            v12 = ilt v7, v10
            br v12, block2, block6
        block2:
            v13 = isub v7, v10
            jmp block3
        block3:
            jmp block4
        block4:
            jmp block5
        block5:
            jmp block6
        block6:
            v14 = phi [v12, block1], [v13, block5]
            v15 = phi [v12, block1], [v12, block5]
            v16 = phi [v12, block1], [v13, block5]
            v17 = imul v16, v15
        }
    "##;

    let expect = expect![[r#"
        function %bar(v4, v8, v9, v10) {
            v5 = iconst 42
        block0:
            v7 = iadd v5, v8
            v12 = ilt v7, v10
            br v12, block2, block6

        block2:
            v13 = isub v7, v10
            jmp block6

        block6:
            v14 = phi [v12, block0], [v13, block2]
            v17 = imul v14, v12
        }
    "#]];

    expect_test(raw, expect)
}

#[test]
pub fn chained_trivial_phi() {
    let raw = r##"
        function %bar(v4) {
        v5 = iconst 42
        v6 = iconst 23
        block0:
            br v4, block1, block5

        block1:
            br v4, block2, block3

        block2:
            jmp block4

        block3:
            jmp block4

        block4:
            v7 = phi [v5, block2], [v6, block3]
            jmp block6


        block5:
            jmp block6

        block6:
            v8 = phi [v5, block5], [v7, block4]

        }
    "##;

    let expect = expect![[r#"
        function %bar(v4) {
            v5 = iconst 42
            v6 = iconst 23
        block0:
            br v4, block1, block6

        block1:
            br v4, block6, block3

        block3:
            jmp block6

        block6:
            v8 = phi [v5, block0], [v5, block1], [v6, block3]
        }
    "#]];

    expect_test(raw, expect)
}

#[test]
pub fn const_terminator() {
    let raw = r##"
        function %bar(v4) {
        v5 = iconst 42
        v6 = iconst 23
        block0:
            br v4, block1, block5

        block1:
            br v1, block2, block3

        block2:
            jmp block4

        block3:
            jmp block4

        block4:
            v7 = phi [v5, block2], [v6, block3]
            jmp block6


        block5:
            jmp block6

        block6:
            v8 = phi [v5, block5], [v7, block4]
            v9 = imul v8, v8

        }
    "##;

    let expect = expect![[r#"
        function %bar(v4) {
            v5 = iconst 42
            v6 = iconst 23
        block0:
            br v4, block6, block5

        block5:
            jmp block6

        block6:
            v8 = phi [v6, block0], [v5, block5]
            v9 = imul v8, v8
        }
    "#]];

    expect_test(raw, expect)
}

#[test]
pub fn duplicate_phis_set() {
    let raw = r##"
        function %bar(v4) {
        v5 = iconst 42
        v6 = iconst 23
        block0:
            br v4, block1, block2
        block1:
            jmp block3
        block2:
            jmp block3
        block3:
            v7 = phi [v5, block1], [v6, block2]
            v8 = phi [v6, block1], [v5, block2]
        block4:
            v9 = imul v7, v8
        }
    "##;
    let (mut func, _) = parse_function(raw).unwrap();
    let mut cursor = FuncCursor::new(&mut func).at_first_insertion_point(3u32.into());

    // equivalent to v7
    for _ in 0..32 {
        cursor.ins().phi(&[(1u32.into(), 5u32.into()), (2u32.into(), 6u32.into())]);
    }

    // different insert direction but logically equivalent
    for _ in 0..32 {
        cursor.ins().phi(&[(2u32.into(), 6u32.into()), (1u32.into(), 5u32.into())]);
    }

    // equivalent to v8
    for _ in 0..32 {
        cursor.ins().phi(&[(2u32.into(), 5u32.into()), (1u32.into(), 6u32.into())]);
    }

    // different insert direction but logically equivalent
    for _ in 0..32 {
        cursor.ins().phi(&[(1u32.into(), 6u32.into()), (2u32.into(), 5u32.into())]);
    }

    cursor.next_inst();
    cursor.next_inst();
    cursor.ins().jump(4u32.into());

    let mut cfg = ControlFlowGraph::new();
    cfg.compute(&func);
    simplify_cfg(&mut func, &mut cfg);

    let expect = expect![[r#"
        function %bar(v4) {
            v5 = iconst 42
            v6 = iconst 23
        block0:
            br v4, block3, block2

        block2:
            jmp block3

        block3:
            v16 = phi [v5, block0], [v6, block2]
            v80 = phi [v6, block0], [v5, block2]
            v9 = imul v16, v80
        }
    "#]];
    expect.assert_eq(&func.to_debug_string())
}
