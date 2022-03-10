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
fn current_source() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimized MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file = project_root().join("integration_tests").join("CURRENT_SOURCE").join("current_source.va");
    let (db, mir, mut literals) = super::compile_to_mir(&root_file);

    //define parameters
    let i = 1.0;
    let r = 1e3;

    // prepare inputs
    let temp = 298.5;
    let mut params = AHashMap::default();
    let mut node_voltages = AHashMap::default();

    params.insert("Io", i.into());
    params.insert("R", r.into());

    node_voltages.insert("Np", 1.0);
    node_voltages.insert("Nm", 0.0);

    // run the interpreter
    let result = mir.interpret(&db, &mut literals, &params, &node_voltages, temp);
    let stamps = mir.matrix.stamps(&db);

    // read the matrix entries
    let ip_vp : f64  = result.read(stamps[&("Np".to_owned(),"Np".to_owned())]);
    let ip_vm : f64  = result.read(stamps[&("Np".to_owned(),"Nm".to_owned())]);
    let im_vp : f64  = result.read(stamps[&("Nm".to_owned(),"Np".to_owned())]);
    let im_vm : f64  = result.read(stamps[&("Nm".to_owned(),"Nm".to_owned())]);

    // calculate the expected values for the stamps
    let g = 1.0/r;

    // Resistor current flows from A into B, resistor voltage = Va-Vb
    let ip_vp_expect = g;
    let ip_vm_expect = -g;
    let im_vm_expect = g;
    let im_vp_expect = -g;

    // finally assert that the values are correct
    let epsilon=1e-5;
    assert_approx_eq!(f64, ip_vp, ip_vp_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ip_vm, ip_vm_expect, epsilon=epsilon);
    assert_approx_eq!(f64, im_vm, im_vm_expect, epsilon=epsilon);
    assert_approx_eq!(f64, im_vp, im_vp_expect, epsilon=epsilon);

}

#[test]
fn cccs() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimized MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file = project_root().join("integration_tests").join("CCCS").join("cccs.va");
    let (db, mir, mut literals) = super::compile_to_mir(&root_file);

    //print assembly
    // println!("{}", mir.func.to_debug_string());

    //define parameters
    let rin = 1.0;
    let rout = 1e9;
    let g = 1e2;

    // prepare inputs
    let temp = 298.5;
    let mut params = AHashMap::default();
    let mut node_voltages = AHashMap::default();

    params.insert("G", g.into());
    params.insert("Rin", rin.into());
    params.insert("Rout", rout.into());

    node_voltages.insert("Inp", 1.0);
    node_voltages.insert("Inm", 0.0);
    node_voltages.insert("Outp", 1e-3);
    node_voltages.insert("Outm", 0.0);

    // run the interpreter
    let result = mir.interpret(&db, &mut literals, &params, &node_voltages, temp);
    let stamps = mir.matrix.stamps(&db);

    // read the matrix entries
    let op_op : f64  = result.read(stamps[&("Outp".to_owned(),"Outp".to_owned())]);
    let op_om : f64  = result.read(stamps[&("Outp".to_owned(),"Outm".to_owned())]);
    let om_op : f64  = result.read(stamps[&("Outm".to_owned(),"Outp".to_owned())]);
    let om_om : f64  = result.read(stamps[&("Outm".to_owned(),"Outm".to_owned())]);
    let ip_ip : f64  = result.read(stamps[&("Inp".to_owned(),"Inp".to_owned())]);
    let ip_im : f64  = result.read(stamps[&("Inp".to_owned(),"Inm".to_owned())]);
    let im_ip : f64  = result.read(stamps[&("Inm".to_owned(),"Inp".to_owned())]);
    let im_im : f64  = result.read(stamps[&("Inm".to_owned(),"Inm".to_owned())]);


    // calculate the expected values for the stamps
    let gin = 1.0/rin;
    let gout = 1.0/rout;

    let ip_ip_expect = gin;
    let ip_im_expect = -gin;
    let im_ip_expect = -gin;
    let im_im_expect = gin;
    let op_op_expect = gout;
    let op_om_expect = -gout;
    let om_op_expect = -gout;
    let om_om_expect = gout;


    // finally assert that the values are correct
    let epsilon=1e-5;
    assert_approx_eq!(f64, ip_ip, ip_ip_expect, epsilon=epsilon);
    assert_approx_eq!(f64, ip_im, ip_im_expect, epsilon=epsilon);
    assert_approx_eq!(f64, im_ip, im_ip_expect, epsilon=epsilon);
    assert_approx_eq!(f64, im_im, im_im_expect, epsilon=epsilon);
    assert_approx_eq!(f64, op_op, op_op_expect, epsilon=epsilon);
    assert_approx_eq!(f64, op_om, op_om_expect, epsilon=epsilon);
    assert_approx_eq!(f64, om_op, om_op_expect, epsilon=epsilon);
    assert_approx_eq!(f64, om_om, om_om_expect, epsilon=epsilon);

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
