use crate::veriloga::*;
use bitflags::bitflags;

bitflags! {
    pub struct EvalFlags: u32 {
        const CALC_RESIST_JACOBIAN = CALC_RESIST_JACOBIAN;
        const CALC_REACT_JACOBIAN = CALC_REACT_JACOBIAN;
        const CALC_RESIST_RESIDUAL = CALC_RESIST_RESIDUAL;
        const CALC_REACT_RESIDUAL = CALC_REACT_RESIDUAL;
        const CALC_NOISE = CALC_NOISE;
        const ANALYSIS_DC = ANALYSIS_DC;
        const ANALYSIS_AC = ANALYSIS_AC;
        const ANALYSIS_STATIC = ANALYSIS_STATIC;
        const ANALYSIS_NOISE = ANALYSIS_NOISE;
        const ANALYSIS_TRAN = ANALYSIS_TRAN;
        const ANALYSIS_IC = ANALYSIS_IC;
    }
}

macro_rules! private_flags {
    ($($vis: vis const $name: ident = $val: expr;)*) => {

        $(impl EvalFlags{
            $vis const $name: Self = Self{ bits: $val };
            }
        )*
    };
}

private_flags! {
    pub(super) const OP =
        CALC_RESIST_JACOBIAN | CALC_RESIST_RESIDUAL | ANALYSIS_STATIC;
    pub(super) const DC_OP = Self::OP.bits | ANALYSIS_DC;
    pub(super) const AC_OP = Self::OP.bits | ANALYSIS_AC;
    // pub(super) const NOISE_OP = Self::OP.bits | ANALYSIS_NOISE;
    // pub(super) const LARGE_SIGNAL_IC_OP = Self::OP.bits | ANALYSIS_TRAN | ANALYSIS_IC;

    pub(super) const AC = CALC_RESIST_JACOBIAN | CALC_REACT_JACOBIAN | ANALYSIS_AC;
    // pub(super) const NOISE = CALC_RESIST_JACOBIAN | CALC_REACT_JACOBIAN | CALC_NOISE | ANALYSIS_NOISE;
    // pub(super) const LARGE_SIGNAL = ANALYSIS_TRAN
    //     | CALC_RESIST_JACOBIAN
    //     | CALC_RESIST_RESIDUAL
    //     | CALC_REACT_JACOBIAN
    //     | CALC_REACT_RESIDUAL;
}

// impl EvalFlags {
//     pub(super) const TRAN_IC_OP: Self = Self::LARGE_SIGNAL_IC_OP;
//     pub(super) const HB_IC_OP: Self = Self::LARGE_SIGNAL_IC_OP;
//     pub(super) const HB: Self = Self::LARGE_SIGNAL;
//     pub(super) const TRAN: Self = Self::LARGE_SIGNAL;
// }

#[derive(PartialEq, Eq, Clone, Copy)]
pub(super) enum OperatingPointAnalysis {
    DC,
    AC,
    // Noise,
    // TranIc,
    // Tran,
    // HBIc,
}

impl OperatingPointAnalysis {
    pub fn eval_flags(self) -> EvalFlags {
        match self {
            OperatingPointAnalysis::DC => EvalFlags::DC_OP,
            OperatingPointAnalysis::AC => EvalFlags::AC_OP,
            // OperatingPointAnalysis::Noise => EvalFlags::NOISE_OP,
            // OperatingPointAnalysis::TranIc => EvalFlags::TRAN_IC_OP,
            // OperatingPointAnalysis::Tran => EvalFlags::TRAN,
            // OperatingPointAnalysis::HBIc => EvalFlags::HB_IC_OP,
        }
    }

    pub fn time_integration(self) -> bool {
        // matches!(self, OperatingPointAnalysis::Tran)
        false
    }

    pub fn solution_flags(self) -> SimulationState {
        match self {
            OperatingPointAnalysis::DC => SimulationState::AT_DC_OP,
            OperatingPointAnalysis::AC => SimulationState::AT_AC_OP,
            // OperatingPointAnalysis::Noise => SimulationState::AT_NOISE_OP,
            // OperatingPointAnalysis::TranIc => todo!(),
            // OperatingPointAnalysis::Tran => todo!(),
            // OperatingPointAnalysis::HBIc => todo!(),
        }
    }
}

bitflags! {
    pub(super) struct SimulationState: u32 {
        const AT_DC_OP = 0b00000001;
        const AT_AC_OP = 0b00000010;
        // const AT_NOISE_OP = 0b00000100;
        const HAS_AC_EVAL = 0b00001000;
        const AT_AC = 0b00010000;
        const AT_OP = Self::AT_DC_OP.bits | Self::AT_AC_OP.bits;// | Self::AT_NOISE_OP.bits;
    }
}

impl SimulationState {
    pub fn clear(&mut self) {
        *self = SimulationState::empty()
    }
}
