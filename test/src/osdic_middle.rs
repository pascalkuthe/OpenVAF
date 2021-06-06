use crate::{middle_test, test_session, PrettyError};
use openvaf_derivatives::generate_derivatives;
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::{MultiDiagnostic, StandardPrinter};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::Local;
use openvaf_transformations::{
    BackwardSlice, BuildPDG, CalculateDataDependence, RemoveDeadLocals, Simplify, SimplifyBranches,
    Verify,
};
use osdic_middle::{divide_analog_block, run_frontend, run_frontend_from_ts};
use osdic_target::sim::Simulator;
use rand::{thread_rng, Rng};
use std::fs::File;
use std::path::PathBuf;
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

            let (model_init, _, _) = divide_analog_block(&mir);

            let mut cfg = model_init.cfg;

            cfg.print(
                &mir,
                File::create(main_file.join(format!("{}_osdic_init.mir", model))).unwrap(),
            )
            .unwrap();

            let malformations = cfg.run_pass(Verify(&mir));
            malformations
                .print_to_file(main_file.join(format!("{}.log", model)))
                .unwrap();

            if !malformations.is_empty() {
                eprintln!("{}", malformations);
                cfg.print(
                    &mir,
                    File::create(main_file.join(format!("{}_osdic_init_invalid.mir", model)))
                        .unwrap(),
                )
                .unwrap();
                unreachable!("Invalid cfg resulted from simplify")
            }
        }

        Ok(())
    })
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
