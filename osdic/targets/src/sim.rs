use osdi_types::Type;
use std::str::FromStr;

pub struct Simulator {
    pub name: String,
    pub is_builtin: bool,
    pub min_version: String,
    pub lim_functions: IndexBox<[LimFunction]>,
    pub sim_paras_real: Box<[RealSimParam]>,
    pub sim_paras_str: Box<[SimParamStr]>,
}

impl Simulator{
    pub fn search_lim_function(&self, name: &str)->Option<&LimFunction>{
        for fun in self.lim_functions.iter(){
            if fun.name == name{
                return Some(fun)
            }
        }
        None
    }
    pub fn search_real_sim_parameters(&self, name: &str)->Option<&RealSimParam>{
        for param in self.sim_paras_real.iter(){
            if param.name == name{
                return Some(param)
            }
        }
        None
    }

    pub fn search_str_sim_parameters(&self, name: &str)->Option<&SimParamStr>{
        for param in self.sim_paras_str.iter(){
            if param.name == name{
                return Some(param)
            }
        }
        None
    }
}

// TODO is it enough to expose the symbols from the simulator or are they required at compile time
// TODO custom Simulator from File

pub struct SimParamStr {
    pub name: String,
    pub val: Option<String>,
    pub is_dynamic: bool,
}

#[derive(Error, Debug)]
pub enum LoadSimulatorError {
    #[error("simulator not found: {0}")]
    BuiltinSimulatorNotFound(String),

    #[error("{0}")]
    Other(String),
}

impl Simulator {
    pub fn search(simulator: &str) -> Result<Simulator, LoadSimulatorError> {
        load_specific(simulator)
    }
}

pub type SimulatorResult = Result<Simulator, String>;

pub struct RealSimParam {
    pub name: String,
    pub val: Option<f64>,
    pub is_dynamic: bool,
}

pub struct LimFunction {
    name: String,
    args: Box<[(String, Type)]>,
}

macro_rules! supported_simulator{
    ( $(($( $triple:literal, )+ $module:ident ),)+ ) => {
        $ ( mod $ module; ) +

        /// List of supported targets
        pub const SIMULATORS: &[&str] = &[$($($triple),+),+];

        fn load_specific(target: &str) -> Result<Simulator, LoadSimulatorError> {
            match target {
                $(
                    $($triple)|+ => {
                        let mut t = $module::simulator()
                            .map_err(LoadSimulatorError::Other)?;
                        t.options.is_builtin = true;

                        tracing::debug!("got builtin simulator", t);
                        Ok(t)
                    },
                )+
                    _ => Err(LoadSimulatorError::BuiltinSimulatorNotFound(
                        format!("Unable to find simulator: {}", target)))
            }
        }
    }
}

supported_simulator!(
    //("ngspice", ngspice),
    ("melange", melange),
);

// TODO load from file