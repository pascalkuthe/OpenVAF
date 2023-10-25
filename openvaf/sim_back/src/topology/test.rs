use expect_test::expect_file;
use hir::diagnostics::ConsoleSink;
use hir::CompilationDB;
use indoc::indoc;
use lasso::Rodeo;
use mir::Function;
use stdx::openvaf_test_data;

use crate::context::{Context, OptimiziationStage};
use crate::topology::Topology;

fn compile(src: &str) -> (Function, Topology, String) {
    let db = CompilationDB::new_virtual(src).unwrap();
    let module = crate::collect_modules(&db, false, &mut ConsoleSink::new(&db)).unwrap().remove(0);
    let mut literals = Rodeo::new();
    let mut context = Context::new(&db, &mut literals, &module);
    context.compute_outputs(true);
    context.compute_cfg();
    context.optimize(OptimiziationStage::Initial);
    let topology = Topology::new(&mut context);
    assert!(context.func.validate());
    (context.func, topology, module.module.name(&db))
}

fn assert(src: &str) {
    let (func, topology, name) = compile(src);
    println!("{func:?}");
    let test_dir = openvaf_test_data("contributions");
    let topology = format!("{topology:#?}");
    expect_file![test_dir.join(format!("{name}_topology.snap"))].assert_eq(&topology);
    let func = format!("{func:#?}");
    expect_file![test_dir.join(format!("{name}_mir.snap"))].assert_eq(&func)
}

#[test]
fn linear_analog_operators() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module linear_analog_operators(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0, bar=2.0;
            analog begin
                I(a, c) <+ V(a) + foo*ddt(V(a));
                I(a, c) <+ bar + foo*white_noise(2*bar, "bar");
            end
        endmodule
    "#};

    assert(src);
}

/// This testcase ensures that conditional time derivates are evaluated properly.
/// In general conditions are a bit tricky to handle. We need to create
/// a dedicated internal node for the ddt(x) if it's used in an operating
/// point dependent condition to ensure the internal state gets updated
/// correctly (otherwise there would be discontinuities). This is fairly niche
/// tough and its important to ensure that the more common case (of operting
/// point independent conditions) also work.
#[test]
fn conditional_ddt() {
    cov_mark::check!(conditional_phi);
    let src = indoc! {r#"
        `include "disciplines.vams"
        module conditional_ddt(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0, bar=2.0;
            real tmp;
            analog begin
                I(a, c) <+ V(a);
                tmp = ddt(V(a));
                if (V(a) < 0) begin
                    if (foo < 0) begin
                        if (bar < 0) begin
                            I(a, c)  <+ tmp;
                        end
                    end
                end
                if (foo < 0) begin
                    if (bar < 0) begin
                        I(a, c)  <+ ddt(V(c));
                    end
                end
            end
        endmodule
    "#};

    assert(src);
}
#[test]
fn collapsible_ddt() {
    cov_mark::check!(collapsible_ddt);
    let src = indoc! {r#"
        `include "disciplines.vams"
        module collapsible_ddt(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0;
            real tmp;
            analog begin
                if (foo < 0) begin
                    I(a, c)  <+ V(a) * ddt(V(c));
                end
            end
        endmodule
    "#};

    assert(src);
}

/// Noise doesn't really create its own equation (its just a small
/// signal source) so it can be used in conditions directly and can
/// stay linear even in conditional code.
#[test]
fn conditional_noise() {
    cov_mark::check!(linear_operator);
    let src = indoc! {r#"
        `include "disciplines.vams"
        module conditional_noise(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0, bar=2.0;
            real tmp;
            analog begin
                I(a, c) <+ V(a);
                if (V(a) < 0) begin
                    I(a, c)  <+ white_noise(foo*bar);
                end
            end
        endmodule
    "#};

    assert(src);
}

#[test]
fn unused_noise() {
    cov_mark::check!(dead_noise);
    let src = indoc! {r#"
        `include "disciplines.vams"
        module unused_noise(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0, bar=2.0;
            real tmp;
            analog begin
                tmp = V(a) + white_noise(2);
                if (tmp < 0) begin
                    I(a, c)  <+ V(a);
                end
            end
        endmodule
    "#};

    assert(src);
}

/// This test tests two things:
/// * that a noise source that is used in multiple times is correctly transformed to a noise source.
/// * that the contributions (to external nodes in this case) are correctly transformed to small
///   signal contributions
#[test]
fn correlated_noise() {
    cov_mark::check!(prune_small_signal);
    cov_mark::check!(port_not_small_signal);
    let src = indoc! {r#"
        `include "disciplines.vams"
        module correlated_noise(inout a, inout c);
            electrical a, c;
            parameter real foo=1.0, bar=2.0;
            real correlated_noise;
            analog begin
                correlated_noise = white_noise(foo);
                I(a) <+ V(a) + white_noise(foo) + bar * correlated_noise;
                I(c) <+ correlated_noise;
            end
        endmodule
    "#};

    assert(src);
}

#[test]
fn manual_correlated_noise() {
    cov_mark::check!(node_is_small_signal);
    let src = indoc! {r#"
        `include "disciplines.vams"
        module manual_correlated_noise(inout a, inout c);
            electrical noise, a, c;
            parameter real foo=1.0, bar=2.0;
            real correlated_noise;
            analog begin
                I(noise) <+ V(noise) - white_noise(foo);
                I(a, c) <+ V(a,c) / foo + 2*V(noise);
            end
        endmodule
    "#};

    assert(src);
}

#[test]
fn psp103() {
    let src = indoc! {r#"
        `include "disciplines.vams"
        module psp103(inout GP, inout SI, inout DI);
            electrical NOI, GP, SI, DI;
            branch (NOI) NOII;
            branch (NOI) NOIR;
            branch (NOI) NOIC;
            parameter real nt=0.0, mig=0.0, CGeff=0.0, MULT_i=0.0, migid=0.0, sqid=0.0, c_igid=0.0, sigVds=0.0;
            analog begin
                // subcircuit
                I(NOII) <+ white_noise((nt / mig));
                I(NOIR) <+ V(NOIR) / mig;
                I(NOIC) <+ ddt(CGeff * V(NOIC));
                // noise sources ids, igs, and igd
                I(GP,SI) <+ -ddt(sqrt(MULT_i) * 0.5 * CGeff * V(NOIC));
                I(GP,DI) <+ -ddt(sqrt(MULT_i) * 0.5 * CGeff * V(NOIC));
                I(DI,SI) <+ sigVds * sqrt(MULT_i) * migid * I(NOII);
                I(DI,SI) <+ white_noise(MULT_i * sqid * sqid * (1.0 - c_igid * c_igid));
            end
        endmodule
    "#};

    assert(src);
}
