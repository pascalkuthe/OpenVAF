use expect_test::expect;

use crate::builder::InstBuilder;
use crate::cursor::{Cursor, CursorPosition, FuncCursor};
use crate::Function;

#[test]
fn basic() {
    let mut func = Function::with_name("foo".to_owned());

    let block = func.layout.append_new_block();
    let v3 = func.dfg.make_param(0u32.into());
    let v4 = func.dfg.make_param(1u32.into());
    let v5 = func.dfg.iconst(3);

    let mut cursor = FuncCursor::new(&mut func);
    cursor.set_position(CursorPosition::After(block));
    let v6 = cursor.ins().iadd(v3, v4);
    cursor.ins().isub(v6, v5);

    let expected = expect![[r#"
        function %foo(v14, v15) {
            v16 = iconst 3
        block0:
            v17 = iadd v14, v15
            v18 = isub v17, v16
        }
    "#]];
    expected.assert_eq(&func.to_debug_string());
}
