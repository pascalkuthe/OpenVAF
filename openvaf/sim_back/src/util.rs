//! various utilities used in this crate

use bitset::BitSet;
use hir::BranchWrite;
use hir_lower::{Dimension, HirInterner, ParamKind, PlaceKind};
use mir::{Function, Inst, InstructionData, Opcode, Value, ValueDef, F_ZERO};

pub fn strip_optbarrier(func: &Function, mut val: Value) -> Value {
    while let Some(inst) = func.dfg.value_def(val).inst() {
        if let InstructionData::Unary { opcode: Opcode::OptBarrier, arg } = func.dfg.insts[inst] {
            val = arg;
        } else {
            break;
        }
    }
    val
}

pub fn is_op_dependent(
    func: &Function,
    val: Value,
    op_dependent_insts: &BitSet<Inst>,
    intern: &HirInterner,
) -> bool {
    match func.dfg.value_def(val) {
        ValueDef::Result(inst, _) => op_dependent_insts.contains(inst),
        ValueDef::Param(param) => intern.params.get_index(param).unwrap().0.op_dependent(),
        ValueDef::Const(_) | ValueDef::Invalid => false,
    }
}

pub fn get_contrib(
    func: &Function,
    intern: &HirInterner,
    branch: BranchWrite,
    dim: Dimension,
    voltage_src: bool,
) -> Value {
    let contrib = PlaceKind::Contribute { dst: branch, dim, voltage_src };

    intern
        .outputs
        .get(&contrib)
        .and_then(|val| val.expand())
        .map(|val| strip_optbarrier(func, val))
        .unwrap_or(F_ZERO)
}

pub fn get_contrib_with_barrier(
    intern: &HirInterner,
    branch: BranchWrite,
    dim: Dimension,
    voltage_src: bool,
) -> Value {
    let contrib = PlaceKind::Contribute { dst: branch, dim, voltage_src };

    intern.outputs.get(&contrib).and_then(|val| val.expand()).unwrap_or(F_ZERO)
}

pub fn has_any_contrib(
    func: &Function,
    intern: &HirInterner,
    branch: BranchWrite,
    voltage_src: bool,
) -> bool {
    intern.dims.keys().any(|dim| get_contrib(func, intern, branch, dim, voltage_src) != F_ZERO)
}

pub struct SwitchBranchInfo {
    pub op_dependent: bool,
    pub introduce_unknown: bool,
    pub non_trivial_voltage: bool,
}

impl SwitchBranchInfo {
    pub fn analyze(
        func: &Function,
        intern: &HirInterner,
        op_dependent_insts: &BitSet<Inst>,
        is_voltage_src: Value,
        branch: BranchWrite,
    ) -> SwitchBranchInfo {
        let requires_unknown = intern.is_param_live(func, &ParamKind::Current(branch.into()));
        let op_dependent = is_op_dependent(func, is_voltage_src, op_dependent_insts, intern);

        let non_trivial_voltage = has_any_contrib(func, intern, branch, true);
        let introduce_unknown = requires_unknown || non_trivial_voltage;
        SwitchBranchInfo { op_dependent, introduce_unknown, non_trivial_voltage }
    }

    pub fn just_current_src(&self) -> bool {
        !self.op_dependent && !self.introduce_unknown
    }
}
