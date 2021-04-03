use crate::sim::{SimulatorResult, Simulator, LimFunction};
use osdi_types::Type;

pub fn simulator() -> SimulatorResult{
    let res = Simulator{
        name: "melange".to_owned(),
        is_builtin: true,
        min_version: "0.1.0".to_string(),
        lim_functions: vec![
            LimFunction{
                name: "pnjlim".to_owned(),
                args: vec![("vt".to_owned(),Type::REAL),("vte".to_owned(),Type::REAL)].into_boxed_slice()
            },
            LimFunction{
                name: "limvds".to_owned(),
                args: Box::new([].into())
            },
            LimFunction{
                name: "limvds".to_owned(),
                args: Box::new([].into())
            },
            LimFunction{
                name: "fetlim".to_owned(),
                args: vec![("vto".to_owned(),Type::REAL)].into_boxed_slice()
            },
        ].into_boxed_slice(),
        sim_paras_real: Box::new([].into()),
        sim_paras_str: Box::new([].into()),
    };

    Ok(res)
}