use ahash::AHashMap;
use expect_test::expect;
use float_cmp::assert_approx_eq;
use sourcegen::project_root;

#[test]
fn diode() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimised MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file = project_root().join("integration_tests").join("DIODE").join("diode.va");
    let (db, mir, mut literals) = super::compile_to_mir(&root_file);

    // printout of the matrix entries
    // (first entry is the current, second entry the voltage)
    let matrix = expect![[r#"
        (A, A) = v98
        (A, CI) = v112
        (CI, A) = v111
        (CI, CI) = v120
        (CI, C) = v121
        (C, CI) = v122
        (C, C) = v106
    "#]];

    // Note: this produces an error if the matrix changes
    // You can update the string by running the test with UPDATE_EXPECT=1
    matrix.assert_eq(&mir.matrix.print(&db));

    // prepare inputs
    let temp = 298.5;
    let mut params = AHashMap::default();
    let mut node_voltages = AHashMap::default();

    params.insert("Is", 1e-9.into());
    params.insert("Rs", 1.0.into());
    params.insert("N", 1.0.into());
    params.insert("Cj0", 1e-12.into());
    params.insert("Vj", 1.0.into());
    params.insert("M", 0.5.into());

    node_voltages.insert("A", 1.0);
    node_voltages.insert("CI", 0.5);
    node_voltages.insert("C", 0.0);

    // run the interpreter
    let result = mir.interpret(&db, &mut literals, &params, &node_voltages, temp);

    // now we can read the matrix entries
    let ia_va: f64 = result.read(98u32.into());
    let ia_vci: f64 = result.read(112u32.into());
    // TODO ... add the remaining stamps

    // TODO calculate the expected values for the stamps
    let ia_va_expect = 0.0;
    let ia_vci_expect = 0.0;
    // TODO ... add the remaining stamps

    // finally assert that the values are correct
    assert_approx_eq!(f64, ia_va, ia_va_expect);
    // you can change tolerance if you need to (values shown here are default)
    assert_approx_eq!(f64, ia_vci, ia_vci_expect, epsilon = f64::EPSILON, ulps = 4);
    // TODO add remaining stamps
}
