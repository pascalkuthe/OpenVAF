use std::f64::consts;
use std::path::Path;

use camino::Utf8Path;
use expect_test::expect_file;
use float_cmp::assert_approx_eq;
use llvm::OptLevel;
use mini_harness::{harness, Result};
use openvaf::{CompilationDestination, CompilationTermination};
use stdx::{ignore_dev_tests, openvaf_test_data, project_root};
use target::spec::Target;

use crate::load::{load_osdi_lib, EvalFlags, OsdiDescriptor};
use crate::mock_sim::{MockSimulation, ALPHA};

mod load;
mod mock_sim;

fn compile_and_load(root_file: &Utf8Path) -> &'static OsdiDescriptor {
    let openvaf_opts = openvaf::Opts {
        defines: Vec::new(),
        codegen_opts: Vec::new(),
        lints: Vec::new(),
        input: root_file.to_path_buf(),
        output: CompilationDestination::Path { lib_file: root_file.with_extension("osdi") },
        include: Vec::new(),
        opt_lvl: OptLevel::Aggressive,
        target: Target::host_target().unwrap(),
        target_cpu: "native".to_owned(),
        dry_run: false,
    };

    let res = openvaf::compile(&openvaf_opts).unwrap();
    let lib_file = match res {
        CompilationTermination::Compiled { lib_file } => lib_file,
        CompilationTermination::FatalDiagnostic => {
            panic!("openvaf: compilation of {root_file} failed");
        }
    };
    let libs = unsafe { load_osdi_lib(&lib_file).unwrap() };
    assert_eq!(libs.len(), 1);
    &libs[0]
}

// fn integration_test(dir: &str) -> Result {
//     let path: Utf8PathBuf = project_root().join("integration_tests").try_into().unwrap();
//     let name = dir.to_lowercase();
//     let main_file = path.join(dir).join(format!("{name}.va"));
//     let device = compile_and_load(&main_file);

//     Ok(())
// }

fn integration_test(dir: &Path) -> Result {
    let name = dir.file_name().unwrap().to_str().unwrap().to_lowercase();
    let main_file = dir.join(format!("{name}.va"));
    test_descriptor(&main_file)?;
    Ok(())
}

fn test_descriptor(main_file: &Path) -> Result<&'static OsdiDescriptor> {
    let main_file: &Utf8Path = main_file.try_into().unwrap();
    let name = main_file.file_stem().unwrap();
    let desc = compile_and_load(main_file);
    let expect = format!("{desc:?}");
    let test_dir = openvaf_test_data("osdi");
    expect_file![test_dir.join(format!("{name}.snap"))].assert_eq(&expect);
    let default_model = desc.new_model();
    default_model.process_params()?;
    let mut instance = default_model.new_instance();
    instance.process_params(&default_model, desc.num_terminals, 300.0)?;
    Ok(desc)
}

macro_rules! assert_approx_eq {
    ($val: expr, $resist: expr, $react: expr) => {
        let (resist, react) = $val;
        let resist_ref: f64 = $resist;
        if (resist - resist_ref).abs() / resist.min(resist_ref) >= 0.01 {
            float_cmp::assert_approx_eq!(f64, resist, resist_ref, epsilon = 1e-10)
        }
        let react_ref: f64 = $react;
        if (react - react_ref).abs() / react.min(react_ref) >= 0.01 {
            float_cmp::assert_approx_eq!(f64, react, react_ref, epsilon = 1e-10)
        }
    };
}

fn test_limit() -> Result<()> {
    const KB: f64 = 1.3806488e-23;
    const Q: f64 = 1.602176565e-19;
    const VT: f64 = KB * 300.0 / Q;
    const IS: f64 = 1e-12;
    const CJ0: f64 = 10e-9;
    let vcrit = VT * f64::ln(VT / (consts::SQRT_2 * IS));
    let check_dae_equations = |sim: &MockSimulation, vd_lim, vd| {
        let id = |vd| IS * (f64::exp(vd / VT) - 1.0);
        let id_vd = |vd| IS / VT * f64::exp(vd / VT);
        let cj = |vd| CJ0 * vd;
        assert_approx_eq!(sim.read_jacobian("A", "A"), id_vd(vd_lim), CJ0);
        assert_approx_eq!(sim.read_jacobian("C", "C"), id_vd(vd_lim), CJ0);
        assert_approx_eq!(sim.read_jacobian("A", "C"), -id_vd(vd_lim), -CJ0);
        assert_approx_eq!(sim.read_jacobian("C", "A"), -id_vd(vd_lim), -CJ0);
        assert_approx_eq!(
            sim.read_residual("A"),
            id(vd_lim) - id_vd(vd_lim) * (vd_lim - vd),
            cj(vd_lim) - CJ0 * (vd_lim - vd)
        );
        assert_approx_eq!(
            sim.read_residual("C"),
            id_vd(vd_lim) * (vd_lim - vd) - id(vd_lim),
            CJ0 * (vd_lim - vd) - cj(vd_lim)
        );
    };

    let check_spice_equations = |sim: &MockSimulation, vd_lim, vd| {
        let id = |vd| IS * (f64::exp(vd / VT) - 1.0);
        let id_vd = |vd| IS / VT * f64::exp(vd / VT);
        let cj = |vd| CJ0 * vd;
        assert_approx_eq!(
            sim.read_residual("A"),
            id_vd(vd_lim) * vd_lim - id(vd_lim) + ALPHA * (CJ0 * vd_lim - cj(vd_lim)),
            0.0
        );
        assert_approx_eq!(
            sim.read_residual("C"),
            id_vd(vd_lim) * (vd_lim - vd) - id(vd_lim),
            CJ0 * (vd_lim - vd) - cj(vd_lim)
        );
        assert_approx_eq!(sim.read_jacobian("A", "A"), id_vd(vd_lim) + ALPHA * CJ0, 0.0);
        assert_approx_eq!(sim.read_jacobian("C", "C"), id_vd(vd_lim) + ALPHA * CJ0, 0.0);
        assert_approx_eq!(sim.read_jacobian("A", "C"), -id_vd(vd_lim) - ALPHA * CJ0, 0.0);
        assert_approx_eq!(sim.read_jacobian("C", "A"), -id_vd(vd_lim) - ALPHA * CJ0, 0.0);
    };

    // compile model and setup simulation
    let desc = test_descriptor(&openvaf_test_data("osdi").join("diode_lim.va"))?;
    let model = desc.new_model();
    model.set_real_param(1, IS);
    model.set_real_param(5, CJ0);
    model.process_params()?;
    let mut instance = model.new_instance();
    let mut sim = instance.mock_simulation(&model, desc.num_terminals, 300.0)?;

    instance.eval(&model, &mut sim, EvalFlags::INIT_LIM | EvalFlags::ENABLE_LIM);
    instance.load_dae(&model, &mut sim);
    check_dae_equations(&sim, vcrit, 0.0);
    sim.clear();
    instance.load_spice(&model, &mut sim);
    check_spice_equations(&sim, vcrit, 0.0);

    sim.next_iter();
    sim.set_voltage("A", 2.0 * vcrit);
    instance.eval(&model, &mut sim, EvalFlags::ENABLE_LIM);
    instance.load_dae(&model, &mut sim);
    check_dae_equations(&sim, 1.5 * vcrit, 2.0 * vcrit);
    sim.clear();
    instance.load_spice(&model, &mut sim);
    check_spice_equations(&sim, 1.5 * vcrit, 2.0 * vcrit);
    Ok(())
}

harness! {
    // TODO: run this in CI, somehow this test is flakey tough regarding the linker invocation (and really slow)
    Test::from_dir("integration", &integration_test, &ignore_dev_tests, &project_root().join("integration_tests")),
    [Test::new("$limit", &test_limit)]
}
