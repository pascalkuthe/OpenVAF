use ahash::AHashSet;
use expect_test::{expect, Expect};
use hir::diagnostics::ConsoleSink;
use hir::Node;
use indoc::indoc;
use lasso::Rodeo;

use crate::{context, CompilationDB};

fn compile(src: &str) -> (AHashSet<Node>, CompilationDB) {
    let db = CompilationDB::new_virtual(src).unwrap();
    let module = crate::collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap().remove(0);
    let mut literals = Rodeo::new();
    let mut context = context::Context::new(&db, &mut literals, &module);
    context.compute_outputs(true);
    context.compute_cfg();
    context.optimize();
    let pruned = context.prune_nodes();
    (pruned, db)
}

fn assert_pruned(src: &str, expect: Expect) {
    let (pruned, db) = compile(src);
    let mut nodes: Vec<_> = pruned.into_iter().map(|node| node.name(&db)).collect();
    nodes.sort_unstable();
    expect.assert_debug_eq(&nodes);
}

#[test]
fn smoke_test() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module test(inout a, inout c);
            electrical i1, i2, a, c;
            parameter real foo=1.0, bar=2.0;
            analog begin
                I(a) <+ V(a);
                I(c) <+ V(i1);
                I(i1) <+ foo*V(i1);
                I(i1) <+ foo*V(i1);
                I(i2) <+ V(i1)/bar + V(i2)*foo;
            end
        endmodule
    "#};

    cov_mark::check!(prune_ignore_ports);
    assert_pruned(
        src,
        expect![[r#"
        [
            "i1",
            "i2",
        ]
    "#]],
    );
}

#[test]
fn switch_branch() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module test();
            electrical i1, i2;
            parameter real foo=1.0, bar=2.0;
            analog begin
                if foo == 1.0
                    I(i1) <+ V(a);
                else
                    V(i1) <+ 1;
                V(i2) <+ 1;
                I(i2) <+ V(a);
            end
        endmodule
    "#};

    assert_pruned(
        src,
        expect![[r#"
        [
            "i2",
        ]
    "#]],
    );
}
