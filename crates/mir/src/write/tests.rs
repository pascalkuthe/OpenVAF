use expect_test::expect;

use crate::builder::InstBuilder;
use crate::cursor::{Cursor, CursorPosition, FuncCursor};
use crate::Function;

#[test]
fn basic() {
    let mut func = Function::with_name("foo".to_owned());

    let block = func.dfg.make_block();
    func.layout.append_block(block);

    let v0 = func.dfg.append_block_param(block);

    let v1 = func.dfg.append_block_param(block);
    let mut cursor = FuncCursor::new(&mut func);
    cursor.set_position(CursorPosition::After(block));
    cursor.ins().iadd(v0, v1);

    let expected = expect![[r#"
        function %foo {
        block0(v0, v1):
            v2 = iadd v0, v1
        }
    "#]];
    expected.assert_eq(&func.to_debug_string());
}

#[test]
fn aliases() {
    let mut func = Function::with_name("bar".to_owned());
    {
        let block0 = func.dfg.make_block();
        let mut pos = FuncCursor::new(&mut func);
        pos.insert_block(block0);

        // make some detached values for change_to_alias
        let v0 = pos.func.dfg.append_block_param(block0);
        let v1 = pos.func.dfg.append_block_param(block0);
        let v2 = pos.func.dfg.append_block_param(block0);
        pos.func.dfg.detach_block_params(block0);

        // alias to a param--will be printed at beginning of block defining param
        let v3 = pos.func.dfg.append_block_param(block0);
        pos.func.dfg.change_to_alias(v0, v3);

        // alias to an alias--should print attached to alias, not ultimate target
        pos.func.dfg.make_value_alias_for_serialization(v0, v2); // v0 <- v2

        // alias to a result--will be printed after instruction producing result
        let _dummy0 = pos.ins().iconst(42);
        let v4 = pos.ins().iadd(v0, v0);
        pos.func.dfg.change_to_alias(v1, v4);
        let _dummy1 = pos.ins().iconst(23);
        let _v7 = pos.ins().iadd(v1, v1);
    }

    let expected = expect![[r#"
        function %bar {
        block0(v3):
            v0 -> v3
            v2 -> v0
            v4 = iconst 42
            v5 = iadd v0, v0
            v1 -> v5
            v6 = iconst 23
            v7 = iadd v1, v1
        }
    "#]];
    expected.assert_eq(&func.to_debug_string());
}
