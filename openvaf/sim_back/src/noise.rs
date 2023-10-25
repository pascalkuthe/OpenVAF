use lasso::Spur;
use mir::Value;
use stdx::Ieee64;

use crate::topology::SimUnknown;

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
