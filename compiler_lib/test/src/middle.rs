use crate::{middle_test, PrettyError};
use openvaf_derivatives::generate_derivatives;
use openvaf_diagnostics::lints::Linter;
use openvaf_diagnostics::{MultiDiagnostic, StandardPrinter};
use openvaf_middle::const_fold::ConstantPropagation;
use openvaf_middle::{Local, LocalKind, VariableLocalKind};
use openvaf_transformations::{
    BackwardSlice, BuildPDG, CalculateDataDependence, RemoveDeadLocals, Simplify, SimplifyBranches,
    Verify,
};
use rand::{thread_rng, Rng};
use std::ops::Deref;
use std::path::PathBuf;
use tracing::{info, info_span};

fn hir_lowering_test(model: &'static str) -> Result<(), PrettyError> {
    let tspan = info_span!(target: "test", "HIR_LOWERING", model = model);
    let _enter = tspan.enter();
    let mut main_file = PathBuf::new();

    main_file.push("integration");
    main_file.push(model);

    let mut file_name = model.to_lowercase();
    file_name.push_str(".va");

    middle_test(main_file.join(file_name), |mir| {
        for x in &mir.parameters {
            if x.ident.as_str() == "vptci" {
                print!("{:?}", x.kind.borrow())
            }
        }

        //if cfg!(feature="middle_dump"){
        for (id, module) in mir.modules.iter_enumerated() {
            let mut cfg = module.analog_cfg.borrow_mut();

            let mut errors = MultiDiagnostic(Vec::new());
            generate_derivatives(&mut cfg, &mir, &mut errors);

            if !errors.is_empty() {
                return Err(errors.user_facing::<StandardPrinter>().into());
            }

            cfg.insert_variable_declarations(&mir);

            mir.print_to_file_with_shared(main_file.join(format!("{}.mir", model)), id, &cfg);

            let malformations = cfg.run_pass(Verify(&mir));
            malformations.print_to_file(main_file.join(format!("{}.log", model)));

            if !malformations.is_empty() {
                eprintln!("{}", malformations);
                mir.print_to_file_with_shared(
                    main_file.join(format!("{}_invalid.mir", model)),
                    id,
                    &cfg,
                );
                unreachable!("Invalid cfg resulted from simplify")
            }

            cfg.insert_variable_declarations(&mir);

            let malformations = cfg.run_pass(Verify(&mir));
            malformations.print_to_file(main_file.join(format!("{}.log", model)));

            if !malformations.is_empty() {
                eprintln!("{}", malformations);
                mir.print_to_file_with_shared(
                    main_file.join(format!("{}_invalid.mir", model)),
                    id,
                    &cfg,
                );
                unreachable!("Invalid cfg resulted from variable delcaration insertion")
            }

            /*let local = cfg
            .locals
            .position(|x| {
                if let LocalKind::Variable(var, VariableLocalKind::User) = x.kind {
                    mir[var].ident.as_str().deref() == "Vbici"
                } else {
                    false
                }
            })
            .unwrap(); */
            let local = thread_rng().gen_range(0..cfg.locals.len());
            let local = Local::new(local);
            info!(
                id = local.index(),
                local = debug(cfg.locals[local].clone()),
                "Backward slice local chosen"
            );

            cfg.run_pass(ConstantPropagation::default());
            cfg.run_pass(SimplifyBranches);
            cfg.run_pass(Simplify);

            let locations = cfg.intern_locations();
            let pdg = cfg.run_pass(BuildPDG {
                locations: &locations,
                data_dependence: CalculateDataDependence,
            });
            cfg.run_pass(BackwardSlice::new(&pdg, &locations).requiring_local_everywhere(local));

            let malformations = cfg.run_pass(Verify(&mir));
            malformations.print_to_file(main_file.join(format!("{}.log", model)));

            if !malformations.is_empty() {
                eprintln!("{}", malformations);
                mir.print_to_file_with_shared(
                    main_file.join(format!("{}_invalid.mir", model)),
                    id,
                    &cfg,
                );
                unreachable!("Invalid cfg resulted from slice")
            }

            cfg.run_pass(Simplify);

            cfg.run_pass(ConstantPropagation::default());

            cfg.run_pass(Simplify);

            let malformations = cfg.run_pass(Verify(&mir));
            malformations.print_to_file(main_file.join(format!("{}.log", model)));

            if !malformations.is_empty() {
                eprintln!("{}", malformations);
                mir.print_to_file_with_shared(
                    main_file.join(format!("{}_invalid.mir", model)),
                    id,
                    &cfg,
                );
                unreachable!("Invalid cfg resulted from simplify")
            }

            cfg.run_pass(RemoveDeadLocals);

            println!("{}", Linter::late_user_diagnostics::<StandardPrinter>()?);

            let malformations = cfg.run_pass(Verify(&mir));
            malformations.print_to_file(main_file.join(format!("{}.log", model)));

            if !malformations.is_empty() {
                eprintln!("{}", malformations);
                mir.print_to_file_with_shared(
                    main_file.join(format!("{}_invalid.mir", model)),
                    id,
                    &cfg,
                );
                unreachable!("Invalid cfg resulted from remove dead locals")
            }

            mir.print_to_file_with_shared(
                main_file.join(format!("{}_sliced.mir", model)),
                id,
                &cfg,
            );
        }

        Ok(())
    })
}

macro_rules! middle_tests {
    ($model:ident) => {
        paste::item! {
            #[test]
            pub fn [< $model _ HIR_LOWERING>]()-> Result<(), PrettyError> {
                hir_lowering_test(stringify!($model))
            }
        }
    };
}

middle_tests!(HICUML2);
//middle_tests!(HICUML0);
middle_tests!(BSIMSOI);
middle_tests!(BSIMBULK);
middle_tests!(BSIMCMG);
middle_tests!(BSIMIMG);
middle_tests!(VBIC_4T_IT_XF_HO);
middle_tests!(DIODE);
// middle_tests!(ASMHEMT);
