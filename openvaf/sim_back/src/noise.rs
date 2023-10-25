use lasso::Spur;
use mir::Value;
use stdx::Ieee64;

use crate::dae::SimUnknown;

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

impl NoiseSource {
    pub fn map_vals(&mut self, mut f: impl FnMut(Value) -> Value) {
        self.factor = f(self.factor);
        match &mut self.kind {
            NoiseSourceKind::WhiteNoise { pwr } => *pwr = f(*pwr),
            NoiseSourceKind::FlickerNoise { pwr, exp } => {
                *pwr = f(*pwr);
                *exp = f(*exp);
            }
            NoiseSourceKind::NoiseTable { .. } => (),
        }
    }
}
