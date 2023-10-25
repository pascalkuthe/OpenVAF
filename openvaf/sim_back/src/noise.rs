use hir::{Branch, Node};
use hir_lower::{CallBackKind, ImplicitEquation};
use lasso::Spur;
use mir::{Function, Inst, Value};
use mir_build::SSAVariableBuilder;
use stdx::Ieee64;

use crate::residual::Residual;
use crate::SimUnknown;

#[derive(Debug, Clone)]
pub enum NoiseSourceKind {
    WhiteNoise { pwr: Value },
    FlickerNoise { pwr: Value, exp: Value },
    NoiseTable { log: bool, vals: Box<[(Ieee64, Ieee64)]> },
}

#[derive(Debug)]
pub struct NoiseSource {
    pub name: Option<Spur>,
    pub kind: NoiseSourceKind,
    pub hi: SimUnknown,
    pub lo: Option<SimUnknown>,
    pub factor: Value,
}
