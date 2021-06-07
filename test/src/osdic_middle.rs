use crate::{middle_test, test_session, PrettyError};
use openvaf_derivatives::generate_derivatives;
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::{MultiDiagnostic, StandardPrinter};
use openvaf_middle::cfg::ControlFlowGraph;
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::{CallType, Local, Mir};
use openvaf_transformations::{
    BackwardSlice, BuildPDG, CalculateDataDependence, RemoveDeadLocals, Simplify, SimplifyBranches,
    Verify,
};
use osdic_middle::{run_frontend, run_frontend_from_ts, GeneralOsdiCall, OsdiFunctions};
use osdic_target::sim::Simulator;
use rand::{thread_rng, Rng};
use std::fs::File;
use std::path::{Path, PathBuf};
use tracing::{info, info_span};

fn osdic_hir_lowering_test(model: &'static str, sim: &'static str) -> Result<(), PrettyError> {
    let tspan = info_span!(target: "test", "HIR_LOWERING", model = model);
    let _enter = tspan.enter();
    let mut main_file = PathBuf::new();

    main_file.push("integration");
    main_file.push(model);

    let mut file_name = model.to_lowercase();
    file_name.push_str(".va");

    test_session(main_file.join(file_name), |ts| {
        let sim = Simulator::search(sim)?;
        let mir = run_frontend_from_ts::<StandardPrinter>(ts, &sim)?;

        for (id, module) in mir.modules.iter_enumerated() {
            let mut cfg = module.analog_cfg.borrow_mut();

            mir.print_to_file_with_shared(main_file.join(format!("{}_osdic.mir", model)), id, &cfg)
                .unwrap();

            let malformations = cfg.run_pass(Verify(&mir));
            malformations
                .print_to_file(main_file.join(format!("{}.log", model)))
                .unwrap();

            if !malformations.is_empty() {
                eprintln!("{}", malformations);
                mir.print_to_file_with_shared(
                    main_file.join(format!("{}_osdic_invalid.mir", model)),
                    id,
                    &cfg,
                )
                .unwrap();
                unreachable!("Invalid cfg resulted from simplify")
            }

            drop(cfg);

            let mut functions = OsdiFunctions::create_from_analog_block_by_automatic_division(&mir);

            test_cfg(
                &main_file,
                &mir,
                model,
                "model_init",
                &mut functions.model_init.cfg,
            );
            test_cfg(
                &main_file,
                &mir,
                model,
                "model_temp_update",
                &mut functions.model_temp_update.cfg,
            );
            test_cfg(
                &main_file,
                &mir,
                model,
                "instance_init",
                &mut functions.instance_init.cfg,
            );
            test_cfg(
                &main_file,
                &mir,
                model,
                "instance_temp_update",
                &mut functions.instance_temp_update.cfg,
            );
            test_cfg(
                &main_file,
                &mir,
                model,
                "dc_load",
                &mut functions.load.dc_load,
            );
            test_cfg(
                &main_file,
                &mir,
                model,
                "ac_load",
                &mut functions.load.ac_load,
            );
        }

        Ok(())
    })
}

fn test_cfg<C: CallType>(
    main_file: &Path,
    mir: &Mir<GeneralOsdiCall>,
    model: &'static str,
    name: &'static str,
    cfg: &mut ControlFlowGraph<C>,
) {
    cfg.print(
        mir,
        File::create(main_file.join(format!("{}_osdic_{}.mir", model, name))).unwrap(),
    )
    .unwrap();

    let malformations = cfg.run_pass(Verify(mir));
    malformations
        .print_to_file(main_file.join(format!("{}.log", model)))
        .unwrap();

    if !malformations.is_empty() {
        eprintln!("{}", malformations);
        cfg.print(
            mir,
            File::create(main_file.join(format!("{}_osdic_{}_invalid.mir", model, name))).unwrap(),
        )
        .unwrap();
        unreachable!(
            "Invalid cfg obtained for the {} function of {}",
            name, model
        )
    }
}

macro_rules! middle_tests {
    ($model:ident for $($sims: ident),*) => {
        $(
            paste::item! {
                #[test]
                pub fn [< $model _OSDIC_HIR_LOWERING_ $sims>]()-> Result<(), PrettyError> {
                        osdic_hir_lowering_test(stringify!($model), stringify!($sims))
                }
            }
        )*
    };
}

middle_tests!(DIODE for melange);
