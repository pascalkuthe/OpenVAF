use ahash::AHashMap;
use expect_test::expect;
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
    let stamps = mir.matrix.resistive_stamps(&db);
    let rhs = mir.residual.resistive_entries(&db);

    // read the matrix entries
    let ia_va: f64 = result.read(stamps[&("A".to_owned(), "A".to_owned())]);
    let ia_vb: f64 = result.read(stamps[&("A".to_owned(), "B".to_owned())]);
    let ib_va: f64 = result.read(stamps[&("B".to_owned(), "A".to_owned())]);
    let ib_vb: f64 = result.read(stamps[&("B".to_owned(), "B".to_owned())]);

    let i_a = result.read(rhs["A"]);
    let i_b = result.read(rhs["B"]);

    // calculate the expected values for the stamps
    let vab = va - vb;
    let res = r * (temp / tnom).powf(zeta);
    let ir = vab / res;
    let g = ir / vab;

    // Resistor current flows from A into B, resistor voltage = Va-Vb
    let ia_va_expect = g;
    let ia_vb_expect = -g;
    let ib_vb_expect = g;
    let ib_va_expect = -g;

    // finally assert that the values are correct
    let epsilon = 1e-5;
    assert_approx_eq!(f64, ia_va, ia_va_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ia_vb, ia_vb_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ib_vb, ib_vb_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ib_va, ib_va_expect, epsilon = epsilon);

    assert_approx_eq!(f64, i_b, -ir);
    assert_approx_eq!(f64, i_a, ir);
}

#[test]
fn current_source() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimized MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file =
        project_root().join("integration_tests").join("CURRENT_SOURCE").join("current_source.va");
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
    let stamps = mir.matrix.resistive_stamps(&db);

    // read the matrix entries
    let ip_vp: f64 = result.read(stamps[&("Np".to_owned(), "Np".to_owned())]);
    let ip_vm: f64 = result.read(stamps[&("Np".to_owned(), "Nm".to_owned())]);
    let im_vp: f64 = result.read(stamps[&("Nm".to_owned(), "Np".to_owned())]);
    let im_vm: f64 = result.read(stamps[&("Nm".to_owned(), "Nm".to_owned())]);

    // calculate the expected values for the stamps
    let g = 1.0 / r;

    // Resistor current flows from A into B, resistor voltage = Va-Vb
    let ip_vp_expect = g;
    let ip_vm_expect = -g;
    let im_vm_expect = g;
    let im_vp_expect = -g;

    // finally assert that the values are correct
    let epsilon = 1e-5;
    assert_approx_eq!(f64, ip_vp, ip_vp_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ip_vm, ip_vm_expect, epsilon = epsilon);
    assert_approx_eq!(f64, im_vm, im_vm_expect, epsilon = epsilon);
    assert_approx_eq!(f64, im_vp, im_vp_expect, epsilon = epsilon);
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
    let stamps = mir.matrix.resistive_stamps(&db);

    // read the matrix entries
    let op_op: f64 = result.read(stamps[&("Outp".to_owned(), "Outp".to_owned())]);
    let op_om: f64 = result.read(stamps[&("Outp".to_owned(), "Outm".to_owned())]);
    let om_op: f64 = result.read(stamps[&("Outm".to_owned(), "Outp".to_owned())]);
    let om_om: f64 = result.read(stamps[&("Outm".to_owned(), "Outm".to_owned())]);
    let ip_ip: f64 = result.read(stamps[&("Inp".to_owned(), "Inp".to_owned())]);
    let ip_im: f64 = result.read(stamps[&("Inp".to_owned(), "Inm".to_owned())]);
    let im_ip: f64 = result.read(stamps[&("Inm".to_owned(), "Inp".to_owned())]);
    let im_im: f64 = result.read(stamps[&("Inm".to_owned(), "Inm".to_owned())]);

    // calculate the expected values for the stamps
    let gin = 1.0 / rin;
    let gout = 1.0 / rout;

    let ip_ip_expect = gin;
    let ip_im_expect = -gin;
    let im_ip_expect = -gin;
    let im_im_expect = gin;
    let op_op_expect = gout;
    let op_om_expect = -gout;
    let om_op_expect = -gout;
    let om_om_expect = gout;

    // finally assert that the values are correct
    let epsilon = 1e-5;
    assert_approx_eq!(f64, ip_ip, ip_ip_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ip_im, ip_im_expect, epsilon = epsilon);
    assert_approx_eq!(f64, im_ip, im_ip_expect, epsilon = epsilon);
    assert_approx_eq!(f64, im_im, im_im_expect, epsilon = epsilon);
    assert_approx_eq!(f64, op_op, op_op_expect, epsilon = epsilon);
    assert_approx_eq!(f64, op_om, op_om_expect, epsilon = epsilon);
    assert_approx_eq!(f64, om_op, om_op_expect, epsilon = epsilon);
    assert_approx_eq!(f64, om_om, om_om_expect, epsilon = epsilon);
}

#[test]
fn vccs() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimized MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file = project_root().join("integration_tests").join("VCCS").join("vccs.va");
    let (db, mir, mut literals) = super::compile_to_mir(&root_file);

    //print assembly
    // println!("{}", mir.func.to_debug_string());

    //define parameters
    let rin = 1.0;
    let rout = 1e9;
    let g = 1e3;

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
    let stamps = mir.matrix.resistive_stamps(&db);

    // read the matrix entries
    let ip_ip: f64 = result.read(stamps[&("Inp".to_owned(), "Inp".to_owned())]);
    let ip_im: f64 = result.read(stamps[&("Inp".to_owned(), "Inm".to_owned())]);
    let im_ip: f64 = result.read(stamps[&("Inm".to_owned(), "Inp".to_owned())]);
    let im_im: f64 = result.read(stamps[&("Inm".to_owned(), "Inm".to_owned())]);

    let op_op: f64 = result.read(stamps[&("Outp".to_owned(), "Outp".to_owned())]);
    let op_om: f64 = result.read(stamps[&("Outp".to_owned(), "Outm".to_owned())]);
    let om_op: f64 = result.read(stamps[&("Outm".to_owned(), "Outp".to_owned())]);
    let om_om: f64 = result.read(stamps[&("Outm".to_owned(), "Outm".to_owned())]);

    let op_ip: f64 = result.read(stamps[&("Outp".to_owned(), "Inp".to_owned())]);
    let op_im: f64 = result.read(stamps[&("Outp".to_owned(), "Inm".to_owned())]);
    let om_ip: f64 = result.read(stamps[&("Outm".to_owned(), "Inp".to_owned())]);
    let om_im: f64 = result.read(stamps[&("Outm".to_owned(), "Inm".to_owned())]);

    // calculate the expected values for the stamps
    let gin = 1.0 / rin;
    let gout = 1.0 / rout;

    // input resistor
    let ip_ip_expect = gin;
    let ip_im_expect = -gin;
    let im_ip_expect = -gin;
    let im_im_expect = gin;

    // output resistor
    let op_op_expect = gout;
    let op_om_expect = -gout;
    let om_op_expect = -gout;
    let om_om_expect = gout;

    // voltage controlled output current
    let op_ip_expect = g;
    let op_im_expect = -g;
    let om_ip_expect = -g;
    let om_im_expect = g;

    // finally assert that the values are correct
    let epsilon = 1e-5;
    assert_approx_eq!(f64, ip_ip, ip_ip_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ip_im, ip_im_expect, epsilon = epsilon);
    assert_approx_eq!(f64, im_ip, im_ip_expect, epsilon = epsilon);
    assert_approx_eq!(f64, im_im, im_im_expect, epsilon = epsilon);
    assert_approx_eq!(f64, op_op, op_op_expect, epsilon = epsilon);
    assert_approx_eq!(f64, op_om, op_om_expect, epsilon = epsilon);
    assert_approx_eq!(f64, om_op, om_op_expect, epsilon = epsilon);
    assert_approx_eq!(f64, om_om, om_om_expect, epsilon = epsilon);
    assert_approx_eq!(f64, op_ip, op_ip_expect, epsilon = epsilon);
    assert_approx_eq!(f64, op_im, op_im_expect, epsilon = epsilon);
    assert_approx_eq!(f64, om_ip, om_ip_expect, epsilon = epsilon);
    assert_approx_eq!(f64, om_im, om_im_expect, epsilon = epsilon);
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
    let zetars = 5.0;
    let zetarth = 2.0;
    let n = 1.0;
    let cj0 = 1e-12;
    let vj = 1.0;
    let m = 0.5;
    let rth = 1e3;
    let va = 1.0;
    let vci = 0.5;
    let vc = 0.0;
    let vdtj = 10.0;
    let tnom = 350.0;
    let zetais = 2.0;
    let ea = 0.0;

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
    params.insert("Rth", rth.into());
    params.insert("Tnom", tnom.into());
    params.insert("zetars", zetars.into());
    params.insert("zetarth", zetarth.into());
    params.insert("zetais", zetais.into());
    params.insert("ea", ea.into());
    params.insert("minr", 1e-3.into());

    node_voltages.insert("A", va);
    node_voltages.insert("CI", vci);
    node_voltages.insert("C", vc);
    node_voltages.insert("dT", vdtj);

    // run the interpreter
    let result = mir.interpret(&db, &mut literals, &params, &node_voltages, temp);
    let stamps = mir.matrix.resistive_stamps(&db);
    // TODO check reactive component
    // TODO check RHS
    let _stamps_react = mir.matrix.reactive_stamps(&db);
    let _rhs = mir.residual.resistive_entries(&db);
    let _rhs_react = mir.residual.reactive_entries(&db);

    let matrix_res = expect![[r#"
        (A, dT) = v470
        (A, A) = v471
        (A, CI) = v488
        (CI, dT) = v473
        (CI, A) = v474
        (CI, CI) = v492
        (CI, C) = v493
        (C, dT) = v476
        (C, CI) = v478
        (C, C) = v496
        (dT, dT) = v479
        (dT, A) = v480
        (dT, CI) = v498
        (dT, C) = v499
    "#]];

    let matrix_react = expect![[r#"
        (A, dT) = v482
        (A, A) = v483
        (A, CI) = v500
        (CI, dT) = v485
        (CI, A) = v486
        (CI, CI) = v503
    "#]];

    matrix_res.assert_eq(&mir.matrix.print_resistive_stamps(&db));
    matrix_react.assert_eq(&mir.matrix.print_reactive_stamps(&db));

    // // Note: this produces an error if the matrix changes
    // // You can update the string by running the test with UPDATE_EXPECT=1
    // matrix.assert_eq(&mir.matrix.print_resistive_stamps(&db));

    // read the matrix entries
    let ia_va: f64 = result.read(stamps[&("A".to_owned(), "A".to_owned())]);
    let ia_vci: f64 = result.read(stamps[&("A".to_owned(), "CI".to_owned())]);
    let ia_dtj: f64 = result.read(stamps[&("A".to_owned(), "dT".to_owned())]);
    let ici_va: f64 = result.read(stamps[&("CI".to_owned(), "A".to_owned())]);
    let ici_vci: f64 = result.read(stamps[&("CI".to_owned(), "CI".to_owned())]);
    let ici_vc: f64 = result.read(stamps[&("CI".to_owned(), "C".to_owned())]);
    let ici_dtj: f64 = result.read(stamps[&("CI".to_owned(), "dT".to_owned())]);
    let ic_vci: f64 = result.read(stamps[&("C".to_owned(), "CI".to_owned())]);
    let ic_vc: f64 = result.read(stamps[&("C".to_owned(), "C".to_owned())]);
    let itj_dtj: f64 = result.read(stamps[&("dT".to_owned(), "dT".to_owned())]);
    let itj_va: f64 = result.read(stamps[&("dT".to_owned(), "A".to_owned())]);
    let itj_vci: f64 = result.read(stamps[&("dT".to_owned(), "CI".to_owned())]);
    let itj_vc: f64 = result.read(stamps[&("dT".to_owned(), "C".to_owned())]);

    // calculate the expected values for the stamps:
    // first some basic pre-calculations
    let pk = 1.3806503e-23;
    let pq = 1.602176462e-19;
    let t_dev = temp + vdtj;
    let tdev_tnom = t_dev / tnom;
    let rs_t = rs * tdev_tnom.powf(zetars);
    let rth_t = rth * tdev_tnom.powf(zetarth);
    let is_t = is * tdev_tnom.powf(zetais);
    let rs_dt = zetars * rs * tdev_tnom.powf(zetars - 1.0) / tnom;
    let rth_dt = zetarth * rth * tdev_tnom.powf(zetarth - 1.0) / tnom;
    let is_dt = zetais * is * tdev_tnom.powf(zetais - 1.0) / tnom;
    let vt = t_dev * pk / pq;
    let vt_tj = pk / pq;
    let vaci = va - vci;
    let vcic = vci - vc;

    let id = is_t * ((vaci / (n * vt)).exp() - 1.0);
    let gd = is_t / vt * (vaci / (n * vt)).exp();
    let gdt = -is_t * (vaci / (n * vt)).exp() * vaci / n / vt / vt * vt_tj
        + 1.0 * ((vaci / (n * vt)).exp() - 1.0) * is_dt;

    let irs = vcic / rs_t;
    let g = 1.0 / rs_t;
    let grt = -irs / rs_t * rs_dt;

    let irth = vdtj / rth_t;
    let gt = 1.0 / rth_t - irth / rth_t * rth_dt;

    // let ith = id * vaci + vcic.powf(2.0) / rs_t;
    let ith_vtj = gdt * vaci - vcic.powf(2.0) / rs_t / rs_t * rs_dt;
    let ith_vc = 0.0 - 2.0 * vcic / rs_t;
    let ith_va = gd * vaci + id;
    let ith_vci = -gd * vaci - id + 2.0 * vcic / rs_t;

    // Diode current flows from Ci into A, diode voltage = Va-Vci
    // Resistor current flows from C into Ci, resistor voltage = Vci-Vc

    // stamp diode
    let ia_va_expect = gd;
    let ia_vci_expect = -gd;
    let ici_va_expect = -gd;
    let ici_vci_expect = gd;

    //diode thermal
    let ia_vtj_expect = gdt;
    let ici_vtj_expect = -gdt;

    // stamp resistor
    let ici_vci_expect = ici_vci_expect + g;
    let ici_vc_expect = -g;
    let ic_vci_expect = -g;
    let ic_vc_expect = g;

    // resistor thermal
    let ici_vtj_expect = ici_vtj_expect + grt;
    // let ic_vtj_expect = -grt;

    //stamp rth flowing into T node dTj/rth
    let it_vt_expect = gt;

    //stamp ith flowing out of T node
    let it_vt_expect = it_vt_expect - ith_vtj;
    let it_vci_expect = -ith_vci;
    let it_vc_expect = -ith_vc;
    let it_va_expect = -ith_va;

    // finally assert that the values are correct
    let epsilon = 1e-5;
    assert_approx_eq!(f64, ia_va, ia_va_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ia_vci, ia_vci_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ia_dtj, ia_vtj_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ici_va, ici_va_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ici_vci, ici_vci_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ici_vc, ici_vc_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ici_dtj, ici_vtj_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ic_vc, ic_vc_expect, epsilon = epsilon);
    assert_approx_eq!(f64, ic_vci, ic_vci_expect, epsilon = epsilon);
    assert_approx_eq!(f64, itj_dtj, it_vt_expect, epsilon = epsilon);
    assert_approx_eq!(f64, itj_va, it_va_expect, epsilon = epsilon);
    assert_approx_eq!(f64, itj_vc, it_vc_expect, epsilon = epsilon);
    assert_approx_eq!(f64, itj_vci, it_vci_expect, epsilon = epsilon);
}

#[test]
fn hicum() {
    // compile model and obtain:
    // a data base that contains all info about the model available to the compiler
    // the optimized MIR that represents the actual compiled code (including matrix entries)
    // the interned string literals (unintersting)
    let root_file = project_root().join("integration_tests").join("HICUML2").join("hicuml2.va");
    let (db, mir, mut literals) = super::compile_to_mir(&root_file);

    ///////////////////////////////////////
    //define modelcard
    //Transfer current
    let c10 = 2.0E-30;
    let qp0 = 2.0E-30;
    let ich = 0.0;
    let hf0 = 1.0;
    let hfe = 1.0;
    let hfc = 1.0;
    let hjei = 1.0;
    let ahjei = 0.0;
    let rhjei = 1.0;
    let hjci = 1.0;

    //Base-Emitter diode currents
    let ibeis = 1.0E-15;
    let mbei = 1.0;
    let ireis = 0.0;
    let mrei = 2.0;
    let ibeps = 0.0;
    let mbep = 1.0;
    let ireps = 0.0;
    let mrep = 2.0;
    let mcf = 1.0;

    //Transit time for excess recombination current at b-c barrier
    let tbhrec = 0.0;

    //Base-Collector diode currents
    let ibcis = 1.0E-15;
    let mbci = 1.0;
    let ibcxs = 0.0;
    let mbcx = 1.0;

    //Base-Emitter tunneling current
    let ibets = 0.0;
    let abet = 40;
    let tunode = 1;

    //Base-Collector avalanche current
    let favl = 0.0;
    let qavl = 0.0;
    let kavl = 0.0;
    let alfav = 0.0;
    let alqav = 0.0;
    let alkav = 0.0;

    //Series resistances
    let rbi0 = 0.0;
    let rbx = 0.0;
    let fgeo = 0.655;
    let fdqr0 = 0.0;
    let fcrbi = 0.0;
    let fqi = 1.0;
    let re = 0.0;
    let rcx = 0.0;

    //Substrate transistor
    let itss = 0.0;
    let msf = 1.0;
    let iscs = 0.0;
    let msc = 1.0;
    let tsf = 0.0;

    //Intra-device substrate coupling
    let rsu = 0.0;
    let csu = 0.0;

    //Depletion Capacitances
    let cjei0 = 1.0E-15;
    let vdei = 0.9;
    let zei = 0.5;
    let ajei = 2.5;
    let cjep0 = 1.0E-15;
    let vdep = 0.9;
    let zep = 0.5;
    let ajep = 2.5;
    let cjci0 = 1.0E-15;
    let vdci = 0.7;
    let zci = 0.4;
    let vptci = 100;
    let cjcx0 = 1.0E-15;
    let vdcx = 0.7;
    let zcx = 0.4;
    let vptcx = 100;
    let fbcpar = 0.0;
    let fbepar = 1.0;
    let cjs0 = 0.0;
    let vds = 0.6;
    let zs = 0.5;
    let vpts = 100;
    let cscp0 = 0.0;
    let vdsp = 0.6;
    let zsp = 0.5;
    let vptsp = 100;

    //Diffusion Capacitances
    let t0 = 0.0;
    let dt0h = 0.0;
    let tbvl = 0.0;
    let tef0 = 0.0;
    let gtfe = 1.0;
    let thcs = 0.0;
    let ahc = 0.1;
    let fthc = 0.0;
    let rci0 = 150;
    let vlim = 0.5;
    let vces = 0.1;
    let vpt = 100.0;
    let aick = 1e-3;
    let delck = 2.0;
    let tr = 0.0;
    let vcbar = 0.0;
    let icbar = 0.0;
    let acbar = 0.01;

    //Isolation Capacitances
    let cbepar = 0.0;
    let cbcpar = 0.0;

    //Non-quasi-static Effect
    let alqf = 0.167;
    let alit = 0.333;
    let flnqs = 0;

    //Noise
    let kf = 0.0;
    let af = 2.0;
    let cfbe = -1;
    let flcono = 0;

    let kfre = 0.0;
    let afre = 2.0;

    //Lateral Geometry Scaling (at high current densities)
    let latb = 0.0;
    let latl = 0.0;

    //Temperature dependence
    let vgb = 1.17;
    let alt0 = 0.0;
    let kt0 = 0.0;
    let zetaci = 0.0;
    let alvs = 0.0;
    let alces = 0.0;
    let zetarbi = 0.0;
    let zetarbx = 0.0;
    let zetarcx = 0.0;
    let zetare = 0.0;
    let zetacx = 1.0;
    let vge = 1.17;
    let vgc = 1.17;
    let vgs = 1.17;
    let f1vg = -1.023;
    let f2vg = 4.321;
    let zetact = 3.0;
    let zetabet = 3.5;
    let alb = 0.0;
    let dvgbe = 0;
    let zetahjei = 1;
    let zetavgbe = 1;

    //Self-Heating
    let flsh = 0;
    let rth = 0.0;
    let zetarth = 0.0;
    let alrth = 0.0;
    let cth = 0.0;

    //Compatibility with V2.1
    let flcomp = 0.0;

    //Circuit simulator specific parameters
    let tnom = 27.0;
    let dt = 0.0;
    let typpe = 1;
    //end modelcard
    ///////////////////////////////////////

    //define node potentials
    let vc = 1.0;
    let vci = 0.9;
    let vb = 1.0;
    let vbp = 0.95;
    let vbi = 0.9;
    let vei = 0.1;
    let ve = 0.0;
    let vs = 0.0;
    let vsi = 0.01;
    let vtnode = 0.0;
    let vxf1 = 0.0;
    let vxf2 = 0.0;
    let vxf = 0.0;
    let vn1 = 0.0;
    let vn2 = 0.0;

    // prepare inputs
    let temp = 298.5;
    let mut params = AHashMap::default();
    let mut node_voltages = AHashMap::default();

    // insert modelcard
    params.insert("c10", c10.into());
    params.insert("qp0", qp0.into());
    params.insert("ich", ich.into());
    params.insert("hf0", hf0.into());
    params.insert("hfe", hfe.into());
    params.insert("hfc", hfc.into());
    params.insert("hjei", hjei.into());
    params.insert("ahjei", ahjei.into());
    params.insert("rhjei", rhjei.into());
    params.insert("hjci", hjci.into());

    //Base-Emitter diode currents
    params.insert("ibeis", ibeis.into());
    params.insert("mbei", mbei.into());
    params.insert("ireis", ireis.into());
    params.insert("mrei", mrei.into());
    params.insert("ibeps", ibeps.into());
    params.insert("mbep", mbep.into());
    params.insert("ireps", ireps.into());
    params.insert("mrep", mrep.into());
    params.insert("mcf", mcf.into());

    //Transit time for excess recombination current at b-c barrier
    params.insert("tbhrec", tbhrec.into());

    //Base-Collector diode currents
    params.insert("ibcis", ibcis.into());
    params.insert("mbci", mbci.into());
    params.insert("ibcxs", ibcxs.into());
    params.insert("mbcx", mbcx.into());

    //Base-Emitter tunneling current
    params.insert("ibets", ibets.into());
    params.insert("abet", abet.into());
    params.insert("tunode", tunode.into());

    //Base-Collector avalanche current
    params.insert("favl", favl.into());
    params.insert("qavl", qavl.into());
    params.insert("kavl", kavl.into());
    params.insert("alfav", alfav.into());
    params.insert("alqav", alqav.into());
    params.insert("alkav", alkav.into());

    //Series resistances
    params.insert("rbi0", rbi0.into());
    params.insert("rbx", rbx.into());
    params.insert("fgeo", fgeo.into());
    params.insert("fdqr0", fdqr0.into());
    params.insert("fcrbi", fcrbi.into());
    params.insert("fqi", fqi.into());
    params.insert("re", re.into());
    params.insert("rcx", rcx.into());

    //Substrate transistor
    params.insert("itss", itss.into());
    params.insert("msf", msf.into());
    params.insert("iscs", iscs.into());
    params.insert("msc", msc.into());
    params.insert("tsf", tsf.into());

    //Intra-device substrate coupling
    params.insert("rsu", rsu.into());
    params.insert("csu", csu.into());

    //Depletion Capacitances
    params.insert("cjei0", cjei0.into());
    params.insert("vdei", vdei.into());
    params.insert("zei", zei.into());
    params.insert("ajei", ajei.into());
    params.insert("cjep0", cjep0.into());
    params.insert("vdep", vdep.into());
    params.insert("zep", zep.into());
    params.insert("ajep", ajep.into());
    params.insert("cjci0", cjci0.into());
    params.insert("vdci", vdci.into());
    params.insert("zci", zci.into());
    params.insert("vptci", vptci.into());
    params.insert("cjcx0", cjcx0.into());
    params.insert("vdcx", vdcx.into());
    params.insert("zcx", zcx.into());
    params.insert("vptcx", vptcx.into());
    params.insert("fbcpar", fbcpar.into());
    params.insert("fbepar", fbepar.into());
    params.insert("cjs0", cjs0.into());
    params.insert("vds", vds.into());
    params.insert("zs", zs.into());
    params.insert("vpts", vpts.into());
    params.insert("cscp0", cscp0.into());
    params.insert("vdsp", vdsp.into());
    params.insert("zsp", zsp.into());
    params.insert("vptsp", vptsp.into());

    //Diffusion Capacitances
    params.insert("t0", t0.into());
    params.insert("dt0h", dt0h.into());
    params.insert("tbvl", tbvl.into());
    params.insert("tef0", tef0.into());
    params.insert("gtfe", gtfe.into());
    params.insert("thcs", thcs.into());
    params.insert("ahc", ahc.into());
    params.insert("fthc", fthc.into());
    params.insert("rci0", rci0.into());
    params.insert("vlim", vlim.into());
    params.insert("vces", vces.into());
    params.insert("vpt", vpt.into());
    params.insert("aick", aick.into());
    params.insert("delck", delck.into());
    params.insert("tr", tr.into());
    params.insert("vcbar", vcbar.into());
    params.insert("icbar", icbar.into());
    params.insert("acbar", acbar.into());

    //Isolation Capacitances
    params.insert("cbepar", cbepar.into());
    params.insert("cbcpar", cbcpar.into());

    //Non-quasi-static Effect
    params.insert("alqf", alqf.into());
    params.insert("alit", alit.into());
    params.insert("flnqs", flnqs.into());

    //Noise
    params.insert("kf", kf.into());
    params.insert("af", af.into());
    params.insert("cfbe", cfbe.into());
    params.insert("flcono", flcono.into());

    params.insert("kfre", kfre.into());
    params.insert("afre", afre.into());

    //Lateral Geometry Scaling (at high current densities)
    params.insert("latb", latb.into());
    params.insert("latl", latl.into());

    //Temperature dependence
    params.insert("vgb", vgb.into());
    params.insert("alt0", alt0.into());
    params.insert("kt0", kt0.into());
    params.insert("zetaci", zetaci.into());
    params.insert("alvs", alvs.into());
    params.insert("alces", alces.into());
    params.insert("zetarbi", zetarbi.into());
    params.insert("zetarbx", zetarbx.into());
    params.insert("zetarcx", zetarcx.into());
    params.insert("zetare", zetare.into());
    params.insert("zetacx", zetacx.into());
    params.insert("vge", vge.into());
    params.insert("vgc", vgc.into());
    params.insert("vgs", vgs.into());
    params.insert("f1vg", f1vg.into());
    params.insert("f2vg", f2vg.into());
    params.insert("zetact", zetact.into());
    params.insert("zetabet", zetabet.into());
    params.insert("alb", alb.into());
    params.insert("dvgbe", dvgbe.into());
    params.insert("zetahjei", zetahjei.into());
    params.insert("zetavgbe", zetavgbe.into());

    //Self-Heating
    params.insert("flsh", flsh.into());
    params.insert("rth", rth.into());
    params.insert("zetarth", zetarth.into());
    params.insert("alrth", alrth.into());
    params.insert("cth", cth.into());

    //Compatibility with V2.1
    params.insert("flcomp", flcomp.into());

    //Circuit simulator specific parameters
    params.insert("tnom", tnom.into());
    params.insert("dt", dt.into());
    params.insert("type", typpe.into());

    // define node voltages
    node_voltages.insert("c", vc);
    node_voltages.insert("ci", vci);
    node_voltages.insert("b", vb);
    node_voltages.insert("bp", vbp);
    node_voltages.insert("bi", vbi);
    node_voltages.insert("ei", vei);
    node_voltages.insert("e", ve);
    node_voltages.insert("s", vs);
    node_voltages.insert("si", vsi);
    node_voltages.insert("tnode", vtnode);
    node_voltages.insert("xf1", vxf1);
    node_voltages.insert("xf2", vxf2);
    node_voltages.insert("xf", vxf);
    node_voltages.insert("n1", vn1);
    node_voltages.insert("n2", vn2);

    // run the interpreter
    let _result = mir.interpret(&db, &mut literals, &params, &node_voltages, temp);
    let _stamps_res = mir.matrix.resistive_stamps(&db);
    let _stamps_react = mir.matrix.reactive_stamps(&db);
    let _rhs_res = mir.residual.resistive_entries(&db);
    let _rhs_react = mir.residual.reactive_entries(&db);

    let matrix_res = expect![[r#"
        (bi, bi) = v129225
        (bi, ei) = v129226
        (ei, bi) = v129227
        (ei, ei) = v129228
        (bi, ci) = v129229
        (ei, ci) = v129230
        (bi, bp) = v129231
        (ei, bp) = v129232
        (bi, tnode) = v129241
        (ei, tnode) = v129242
        (ei, e) = v129244
        (ei, xf2) = v129248
        (bi, n1) = v129251
        (ei, n1) = v129252
        (ei, n2) = v129252
        (ci, bi) = v129255
        (ci, ei) = v129256
        (ci, ci) = v129257
        (ci, bp) = v129258
        (ci, si) = v129260
        (ci, c) = v129262
        (ci, tnode) = v129263
        (ci, xf2) = v129266
        (ci, n2) = v129251
        (bp, bi) = v129270
        (bp, ei) = v129271
        (bp, ci) = v129272
        (bp, bp) = v129273
        (bp, b) = v129274
        (bp, si) = v129275
        (bp, tnode) = v129278
        (b, bp) = v129274
        (b, b) = v129289
        (b, tnode) = v129293
        (si, ci) = v129302
        (si, bp) = v129303
        (si, si) = v129305
        (si, s) = v129306
        (si, tnode) = v129308
        (c, ci) = v129262
        (c, c) = v129322
        (c, tnode) = v129323
        (e, ei) = v129244
        (e, tnode) = v129338
        (e, e) = v129339
        (s, si) = v129306
        (s, s) = v129351
        (tnode, bi) = v129360
        (tnode, ei) = v129361
        (tnode, ci) = v129362
        (tnode, bp) = v129363
        (tnode, b) = v129364
        (tnode, si) = v129365
        (tnode, c) = v129367
        (tnode, tnode) = v129368
        (tnode, e) = v129369
        (xf1, bi) = v129375
        (xf1, ei) = v129376
        (xf1, ci) = v129377
        (xf1, tnode) = v129383
        (xf1, xf1) = v129385
        (xf1, xf2) = v129386
        (xf2, bi) = v129390
        (xf2, ei) = v129391
        (xf2, ci) = v129392
        (xf2, tnode) = v129398
        (xf2, xf1) = v129400
        (xf2, xf2) = v129401
        (xf, bi) = v129405
        (xf, ei) = v129406
        (xf, ci) = v129407
        (xf, tnode) = v129413
        (xf, xf) = v129417
        (n1, n1) = v129433
        (n2, n2) = v129433
    "#]];

    let matrix_react = expect![[r#"
        (ci, bi) = v129452
        (ci, ei) = v129453
        (b, ci) = v129454
        (ci, ci) = v129455
        (ci, bp) = v129457
        (b, b) = v129458
        (ci, b) = v129459
        (ci, si) = v129461
        (b, tnode) = v129466
        (ci, tnode) = v129467
        (b, e) = v129468
        (bp, bi) = v129480
        (bp, ei) = v129481
        (bp, ci) = v129482
        (bp, bp) = v129483
        (bp, tnode) = v129488
        (bp, e) = v129489
        (bi, bi) = v129495
        (bi, ei) = v129496
        (bi, ci) = v129497
        (bi, bp) = v129498
        (bi, tnode) = v129503
        (bi, xf) = v129266
        (bi, n1) = v129508
        (bi, n2) = v129509
        (ei, bi) = v129510
        (ei, ei) = v129511
        (ei, ci) = v129512
        (ei, bp) = v129513
        (ei, tnode) = v129518
        (ei, xf) = v129248
        (ei, n1) = v129523
        (ei, n2) = v129524
        (e, b) = v129525
        (e, e) = v129526
        (e, bp) = v129527
        (si, ci) = v129461
        (si, si) = v129533
        (si, s) = v129534
        (si, tnode) = v129536
        (s, si) = v129534
        (s, s) = v129555
        (s, c) = v129556
        (c, s) = v129556
        (c, c) = v129558
        (s, tnode) = v129559
        (c, tnode) = v129560
        (tnode, tnode) = v129581
        (xf1, xf1) = v129598
        (xf2, xf2) = v129614
        (xf, xf) = v129630
    "#]];

    matrix_res.assert_eq(&mir.matrix.print_resistive_stamps(&db));
    matrix_react.assert_eq(&mir.matrix.print_reactive_stamps(&db));
}
