use std::fs;

use expect_test::expect_file;
use hir::diagnostics::ConsoleSink;
use hir::CompilationDB;
use indoc::indoc;
use lasso::Rodeo;
use stdx::{integration_test_dir, openvaf_test_data};

use crate::context::{Context, OptimiziationStage};
use crate::dae::DaeSystem;
use crate::init::Initialization;
use crate::topology::Topology;

fn run_test(src: &str) {
    let db = CompilationDB::new_virtual(src).unwrap();
    let module = crate::collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap().remove(0);
    let mut literals = Rodeo::new();
    let mut cx = Context::new(&db, &mut literals, &module);
    cx.compute_outputs(true);
    cx.compute_cfg();
    cx.optimize(OptimiziationStage::Initial);

    let topology = Topology::new(&mut cx);
    let mut dae_system = DaeSystem::new(&mut cx, topology);

    cx.compute_cfg();
    let gvn = cx.optimize(OptimiziationStage::PostDerivative);
    dae_system.sparsify(&mut cx);

    cx.refresh_op_dependent_insts();
    let init = Initialization::new(&mut cx, gvn);
    let name = module.module.name(&db);
    let test_dir = openvaf_test_data("init");
    let topology = format!("{:#?}\n{:#?}", init.cached_vals, init.cache_slots);
    assert!(cx.func.validate());
    assert!(init.func.validate());
    expect_file![test_dir.join(format!("{name}_system.snap"))].assert_eq(&topology);
    let func = format!("{:#?}", init.func);
    expect_file![test_dir.join(format!("{name}_init_mir.snap"))].assert_eq(&func);
    let func = format!("{:#?}", &cx.func);
    expect_file![test_dir.join(format!("{name}_eval_mir.snap"))].assert_eq(&func);
}

#[test]
fn diode() {
    let src = fs::read_to_string(integration_test_dir("DIODE").join("diode.va")).unwrap();
    run_test(&src);
}

#[test]
fn resistor() {
    cov_mark::check!(cache_output);
    cov_mark::check!(op_independent_output);
    let src = fs::read_to_string(integration_test_dir("RESISTOR").join("resistor.va")).unwrap();
    run_test(&src);
}

#[test]
fn op_dependent_collapse_hint() {
    cov_mark::check!(ignore_if_op_dependent);
    let src = indoc! {r#"
        `include "disciplines.vams"
        module op_dependent_collapse_hint(inout a, inout c);
            electrical a, c, d;
            parameter real foo=1.0;
            analog begin
                if (foo == 0) 
                    V(d) <+ 0.0;
                if (V(a, c) < 0) 
                    V(d) <+ 0.0;
                else
                    V(d) <+ foo;
                I(a, c) <+ V(d);
            end
        endmodule
    "#};
    run_test(src);
}

#[test]
fn analysis() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module analysis_dependent(inout a, inout c);
            electrical a, c;
            parameter real R=1.0;
            analog begin
                if (analysis("tran"))
                    I(a, c) <+ R * V(a,c);
            end
        endmodule
    "#};
    run_test(src);
}
