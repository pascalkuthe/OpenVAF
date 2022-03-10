use ahash::AHashMap;
use float_cmp::assert_approx_eq;
use sourcegen::project_root;

#[test]
fn resistor() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimized MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file = project_root().join("integration_tests").join("RESISTOR").join("resistor.va");
    let (db, mir, mut literals) = super::compile_to_mir(&root_file);

    //define parameters
    let r = 15.0;
    let zeta = 1.0;
    let tnom = 250.0;
    let va = 1.0;
    let vb = 0.0;

    // prepare inputs
    let temp = 298.5;
    let mut params = AHashMap::default();
    let mut node_voltages = AHashMap::default();

    params.insert("R", r.into());
    params.insert("zeta", zeta.into());
    params.insert("tnom", tnom.into());

    node_voltages.insert("A", va);
    node_voltages.insert("B", vb);

    // run the interpreter
    let result = mir.interpret(&db, &mut literals, &params, &node_voltages, temp);
    let stamps = mir.matrix.stamps(&db);

    // read the matrix entries
    let ia_va : f64  = result.read(stamps[&("A".to_owned(),"A".to_owned())]);
    let ia_vb : f64  = result.read(stamps[&("A".to_owned(),"B".to_owned())]);
    let ib_va : f64  = result.read(stamps[&("B".to_owned(),"A".to_owned())]);
    let ib_vb : f64  = result.read(stamps[&("B".to_owned(),"B".to_owned())]);

    // calculate the expected values for the stamps
    let vab = va-vb;
    let res = r*(temp/tnom).powf(zeta);
    let ir = vab/res;
    let g = ir/vab;

    // Resistor current flows from A into B, resistor voltage = Va-Vb
    let ia_va_expect = g;
    let ia_vb_expect = -g;
    let ib_vb_expect = g;
    let ib_va_expect = -g;

    // finally assert that the values are correct
    let epsilon=1e-5;
    assert_approx_eq!(f64, ia_va, ia_va_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ia_vb, ia_vb_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ib_vb, ib_vb_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ib_va, ib_va_expect, epsilon=epsilon);

}


#[test]
fn diode() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimized MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file = project_root().join("integration_tests").join("DIODE").join("diode.va");
    let (db, mir, mut literals) = super::compile_to_mir(&root_file);

    //define parameters
    let is = 1e-9;
    let rs = 1.0;
    let n = 1.0;
    let cj0 = 1e-12;
    let vj = 1.0;
    let m = 0.5;
    let va = 1.0;
    let vci = 0.5;
    let vc = 0.0;

    // prepare inputs
    let temp = 298.5;
    let mut params = AHashMap::default();
    let mut node_voltages = AHashMap::default();

    params.insert("Is", is.into());
    params.insert("Rs", rs.into());
    params.insert("N", n.into());
    params.insert("Cj0", cj0.into());
    params.insert("Vj", vj.into());
    params.insert("M", m.into());

    node_voltages.insert("A", va);
    node_voltages.insert("CI", vci);
    node_voltages.insert("C", vc);

    // run the interpreter
    let result = mir.interpret(&db, &mut literals, &params, &node_voltages, temp);
    let stamps = mir.matrix.stamps(&db);

    // read the matrix entries
    let ia_va : f64  = result.read(stamps[&("A".to_owned(),"A".to_owned())]);
    let ia_vci : f64  = result.read(stamps[&("A".to_owned(),"CI".to_owned())]);
    let ici_va : f64  = result.read(stamps[&("CI".to_owned(),"A".to_owned())]);
    let ici_vci : f64  = result.read(stamps[&("CI".to_owned(),"CI".to_owned())]);
    let ici_vc : f64  = result.read(stamps[&("CI".to_owned(),"C".to_owned())]);
    let ic_vci : f64  = result.read(stamps[&("C".to_owned(),"CI".to_owned())]);
    let ic_vc : f64  = result.read(stamps[&("C".to_owned(),"C".to_owned())]);

    // calculate the expected values for the stamps
    let pk =  1.3806503e-23;
    let pq =  1.602176462e-19;
    let vt = temp*pk/pq;
    let vaci = va-vci;
    let id = is * ((vaci / (n * vt)).exp() - 1.0);
    let gd = id/vt;
    let g = 1.0/rs;

    // Diode current flows from Ci into A, diode voltage = Va-Vci
    // Resistor current flows from C into Ci, resistor voltage = Vci-Vc
    let ia_va_expect = gd;
    let ia_vci_expect = -gd;
    let ici_va_expect = -gd;
    let ici_vci_expect = gd + g;
    let ici_vc_expect = - g;
    let ic_vci_expect = -g;
    let ic_vc_expect = g;

    // finally assert that the values are correct
    let epsilon=1e-5;
    assert_approx_eq!(f64, ia_va, ia_va_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ia_vci, ia_vci_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ici_va, ici_va_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ici_vci, ici_vci_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ici_vc, ici_vc_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ic_vc, ic_vc_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ic_vci, ic_vci_expect, epsilon=epsilon);

}
