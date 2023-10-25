use std::fs;

use expect_test::expect_file;
use hir::diagnostics::ConsoleSink;
use hir::CompilationDB;
use indoc::indoc;
use lasso::Rodeo;
use stdx::{integration_test_dir, openvaf_test_data};

use crate::context::{Context, OptimiziationStage};
use crate::dae::DaeSystem;
use crate::topology;

fn run_test(src: &str) {
    let db = CompilationDB::new_virtual(src).unwrap();
    let module = crate::collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap().remove(0);
    let mut literals = Rodeo::new();
    let mut context = Context::new(&db, &mut literals, &module);
    context.compute_outputs(true);
    context.compute_cfg();
    context.optimize(OptimiziationStage::Inital);
    let topology = topology::Topology::new(&mut context);
    let mut dae_system = DaeSystem::new(&mut context, topology);
    context.compute_cfg();
    context.optimize(OptimiziationStage::Final);
    dae_system.sparsify(&mut context);
    let name = module.module.name(&db);
    let test_dir = openvaf_test_data("dae");
    let topology = format!("{dae_system:#?}");
    assert!(context.func.validate());
    expect_file![test_dir.join(format!("{name}_system.snap"))].assert_eq(&topology);
    let func = format!("{:#?}", context.func);
    expect_file![test_dir.join(format!("{name}_mir.snap"))].assert_eq(&func)
}

#[test]
fn diode() {
    let src = fs::read_to_string(integration_test_dir("DIODE").join("diode.va")).unwrap();
    run_test(&src);
}

#[test]
fn resistor() {
    let src = fs::read_to_string(integration_test_dir("RESISTOR").join("resistor.va")).unwrap();
    run_test(&src);
}

#[test]
fn lim_rhs() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module lim_rhs(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0, bar=2.0;
            analog begin
                I(a, c) <+ foo*exp($limit(V(a,c), "testlim"));
            end
        endmodule
    "#};
    run_test(src);
}

#[test]
fn lim_rhs_sign() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module lim_rhs_sign(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0, bar=2.0;
            real Vac;
            analog begin
                if (foo < 0) 
                    Vac = $limit(V(c, a), "testlim");
                else
                    Vac = $limit(V(a, c), "testlim");

                I(a, c) <+ foo*exp(Vac);
            end
        endmodule
    "#};
    run_test(src);
}

#[test]
fn voltage_src() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module voltage_src(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0;
            analog begin
                V(a, c) <+ foo;
            end
        endmodule
    "#};
    run_test(src);
}

#[test]
fn const_switch_branch() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module const_switch_branch(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0;
            analog begin
                if (foo < 0 )
                    V(a, c) <+ foo;
                else
                    I(a, c) <+ foo;
            end
        endmodule
    "#};
    run_test(src);
}

#[test]
fn dyn_switch_branch() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module dyn_switch_branch(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0;
            analog begin
                if (V(a, c) < 0) 
                    V(a, c) <+ foo * V(a, c);
                else
                    I(a, c) <+ foo * V(a, c);
            end
        endmodule
    "#};
    run_test(src);
}
