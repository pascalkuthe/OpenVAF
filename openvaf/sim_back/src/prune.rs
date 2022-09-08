//! During pruning the equivalent circuit is simplied to improve performance of both the compiler
//! and the runtime. This process will remove unkowns

use std::mem::take;

use bitset::{BitSet, HybridBitSet};
use hir_def::db::HirDefDB;
use hir_def::NodeId;
use hir_lower::{
    HirInterner, ImplicitEquationKind, ParamKind, PlaceKind, REACTIVE_DIM, RESISTIVE_DIM,
};
use hir_ty::inference::BranchWrite;
use indexmap::map::Entry;
use indexmap::{IndexMap, IndexSet};
use mir::{
    DominatorTree, Function, Inst, InstructionData, Opcode, Param, Value, ValueDef, FALSE, F_ZERO,
    TRUE,
};
use mir_opt::propagate_taint;
use workqueue::WorkStack;

use crate::util::{
    get_contrib, get_contrib_with_barrier, is_op_depenent, strip_optbarrier, SwitchBranchInfo,
};
use crate::CompilationDB;

/// Algorithm that iterativly prunes an equivalent-circuit of unneded unkowns.
/// Initally all ddt and noise functions create an implicit unkown.
/// This algorithm transforms these to just dimensional contributes whenever possible.
/// Furthermore the algorithm finds any nodes that are always zero (or only have non-zero values
/// during noise analysis) and removes them/allows ddt to be dimensional
pub fn prune_unkowns(
    db: &CompilationDB,
    func: &mut Function,
    intern: &mut HirInterner,
    dom_tree: &DominatorTree,
    op_dependent_vals: &IndexSet<Value, ahash::RandomState>,
) -> (BitSet<Inst>, HybridBitSet<Param>, HybridBitSet<Param>) {
    // initial op dependent insts is conservative and assumes that all nodes are op dependent
    let mut op_dependent_insts = BitSet::new_empty(func.dfg.num_insts());
    let mut is_noise = HybridBitSet::new_empty();
    let mut pruned = HybridBitSet::new_empty();
    propagate_taint(func, dom_tree, op_dependent_vals, &mut op_dependent_insts);
    let num_insts = func.dfg.num_insts();

    let num_vals = func.dfg.num_values();
    let mut pruner = UnkownPruner {
        db,
        func,
        intern,
        dom_tree,
        op_dependent_vals,
        op_dependent_insts: &mut op_dependent_insts,
        op_dependent_vals_no_noise: IndexSet::default(),
        op_dependent_insts_no_noise: BitSet::new_empty(num_insts),
        queue: WorkStack::with_none(num_vals),
        is_noise: &mut is_noise,
        pruned: &mut pruned,
    };
    pruner.run();

    (op_dependent_insts, is_noise, pruned)
}

struct UnkownPruner<'a> {
    db: &'a CompilationDB,
    func: &'a mut Function,
    intern: &'a mut HirInterner,
    dom_tree: &'a DominatorTree,
    is_noise: &'a mut HybridBitSet<Param>,
    pruned: &'a mut HybridBitSet<Param>,

    op_dependent_vals: &'a IndexSet<Value, ahash::RandomState>,
    op_dependent_insts: &'a mut BitSet<Inst>,

    op_dependent_vals_no_noise: IndexSet<Value, ahash::RandomState>,
    op_dependent_insts_no_noise: BitSet<Inst>,
    queue: WorkStack<Value>,
}

impl<'a> UnkownPruner<'a> {
    fn is_op_depenent<const NO_NOISE: bool>(&self, val: Value) -> bool {
        if !NO_NOISE {
            return is_op_depenent(self.func, val, self.op_dependent_insts, self.intern);
        }
        match self.func.dfg.value_def(val) {
            ValueDef::Result(inst, _) => self.op_dependent_insts_no_noise.contains(inst),
            ValueDef::Param(_) => self.op_dependent_vals_no_noise.contains(&val),
            ValueDef::Const(_) | ValueDef::Invalid => false,
        }
    }

    fn run(&mut self) {
        let mut candidates = self.collect_candidates();

        if candidates.is_empty() {
            self.remove_linear_ddt_unkowns(true, true);
            return;
        }

        self.remove_linear_ddt_unkowns(true, false);
        self.op_dependent_vals_no_noise = self.op_dependent_vals.clone();
        self.op_dependent_insts_no_noise.copy_from(self.op_dependent_insts);

        let mut buf = BitSet::new_empty(self.func.dfg.num_insts());
        while !candidates.is_empty() {
            let mut changed = false;
            while self.prune_noise_and_trival(&mut candidates, &mut buf) {
                changed = true;
            }

            if !changed {
                break;
            }

            self.op_dependent_insts.clear();
            propagate_taint(
                self.func,
                self.dom_tree,
                self.op_dependent_vals,
                self.op_dependent_insts,
            );

            if self.is_noise.is_empty() {
                self.op_dependent_insts_no_noise.copy_from(self.op_dependent_insts);
            } else {
                self.op_dependent_insts_no_noise.clear();
                propagate_taint(
                    self.func,
                    self.dom_tree,
                    &self.op_dependent_vals_no_noise,
                    &mut self.op_dependent_insts_no_noise,
                );
            }

            let changed = self.remove_linear_ddt_unkowns(false, false);

            if !changed {
                break;
            }
        }

        self.remove_linear_ddt_unkowns(false, true);
    }

    fn has_noise(&self, branch: BranchWrite, voltage_src: bool) -> bool {
        self.intern
            .dims
            .keys()
            .skip(2)
            .any(|dim| get_contrib(self.func, self.intern, branch, dim, voltage_src) != F_ZERO)
    }

    fn collect_candidates(&mut self) -> PruneCandidates {
        // reuse allocation here because its still empty at this point
        let mut buf = take(&mut self.op_dependent_insts_no_noise);

        let mut res: PruneCandidates = IndexMap::default();
        for (kind, val) in self.intern.outputs.iter() {
            let branch = if let PlaceKind::IsVoltageSrc(branch) = *kind {
                branch
            } else {
                continue;
            };

            let is_voltage_src = val.unwrap_unchecked();
            let node = if let (node, None) = branch.nodes(self.db) {
                node
            } else {
                continue;
            };

            if self.db.node_data(node).is_port() {
                continue;
            }

            let mut err = false;

            let voltage = if let Some(val) =
                self.intern.params.raw.get(&ParamKind::Voltage { hi: node, lo: None })
            {
                if self.func.dfg.value_dead(*val) {
                    continue;
                }
                val
            } else {
                continue;
            };

            let current = self
                .intern
                .params
                .index_and_val(&ParamKind::Current(branch.into()))
                .filter(|(_, val)| !self.func.dfg.value_dead(**val))
                .map(|(param, _)| param);

            let is_voltage_src = match is_voltage_src {
                FALSE => false,
                TRUE => true,
                _ => {
                    let br_info = SwitchBranchInfo::analyze(
                        self.func,
                        self.intern,
                        self.op_dependent_insts,
                        is_voltage_src,
                        branch,
                    );
                    if br_info.non_trivial_voltage || br_info.op_dependent {
                        err = true;
                    }

                    false
                }
            };

            let resist =
                get_contrib_with_barrier(self.intern, branch, RESISTIVE_DIM, is_voltage_src);
            let react = get_contrib_with_barrier(self.intern, branch, REACTIVE_DIM, is_voltage_src);

            let has_resist = strip_optbarrier(self.func, resist) != F_ZERO;
            let has_react = strip_optbarrier(self.func, react) != F_ZERO;
            let has_noise = self.has_noise(branch, is_voltage_src);

            let contrib = RemainingContrib {
                resist: has_resist.then_some(resist),
                react: has_react.then_some(react),
                current,
                has_noise,
            };

            let (err, is_linear) = if err {
                (true, false)
            } else {
                let mut known_linear = HybridBitSet::new_empty();
                self.op_dependent_vals_no_noise.insert(*voltage);
                propagate_taint(
                    self.func,
                    self.dom_tree,
                    &self.op_dependent_vals_no_noise,
                    &mut buf,
                );
                let resist_dep = self.analyse_dep(resist, *voltage, &mut known_linear, &buf);
                let react_dep = self.analyse_dep(react, *voltage, &mut known_linear, &buf);
                err = resist_dep == Dependency::NonLinear || react_dep == Dependency::NonLinear;
                let is_linear = resist_dep == Dependency::Linear || react_dep == Dependency::Linear;
                buf.clear();
                (err, is_linear)
            };

            match res.entry(node) {
                Entry::Occupied(mut entry) => {
                    let candidate = entry.get_mut();

                    if is_voltage_src || err {
                        candidate.status = CandidateStatus::Invalid;
                    } else {
                        candidate.status.update(is_linear);
                    }
                    if candidate.status == CandidateStatus::Invalid {
                        continue;
                    }

                    if has_resist || has_react || current.is_some() {
                        candidate.remaining.push(contrib);
                        candidate.remaining_no_noise.push(contrib);
                    }
                    candidate.has_noise = candidate.has_noise || has_noise
                }
                Entry::Vacant(entry) => {
                    let status = if err {
                        CandidateStatus::Invalid
                    } else {
                        CandidateStatus::new(is_voltage_src, is_linear)
                    };

                    let mut remaining = Vec::new();
                    let mut remaining_no_noise = Vec::new();
                    if status != CandidateStatus::Invalid
                        && (has_resist || has_react || current.is_some())
                    {
                        remaining.push(contrib);
                        remaining_no_noise.push(contrib);
                    }

                    entry.insert(PruneCandidate {
                        remaining,
                        remaining_no_noise,
                        status,
                        has_noise,
                    });
                }
            }
        }

        for kind in self.intern.outputs.keys() {
            let branch = if let PlaceKind::IsVoltageSrc(branch) = *kind {
                branch
            } else {
                continue;
            };

            let (node1, node2) = if let (node1, Some(node2)) = branch.nodes(self.db) {
                (node1, node2)
            } else {
                continue;
            };

            res.remove(&node1);
            res.remove(&node2);
        }

        res.retain(|_, candidate| {
            !matches!(candidate.status, CandidateStatus::Invalid | CandidateStatus::CurrentSrc)
        });

        self.op_dependent_insts_no_noise = buf;

        res
    }

    fn prune_noise_and_trival(
        &mut self,
        candidates: &mut PruneCandidates,
        buf: &mut BitSet<Inst>,
    ) -> bool {
        let mut changed = false;
        candidates.retain(|node, candidate| {
            let pot = self.intern.params.unwrap_index(&ParamKind::Voltage { hi: *node, lo: None });
            self.pruned.insert(pot, self.intern.params.len());
            candidate.remaining.retain_mut(|it| {
                !it.is_zero(self.func, self.intern, self.pruned, buf, &mut changed)
            });
            if candidate.remaining.is_empty() {
                let val = self.intern.params[pot];
                if candidate.has_noise {
                    self.is_noise.insert(pot, self.intern.params.len());
                    self.op_dependent_vals_no_noise.remove(&val);
                } else {
                    self.is_noise.remove(pot);
                    self.func.dfg.replace_uses(val, F_ZERO);
                }

                changed = true;
                return false;
            }
            self.pruned.remove(pot);

            self.is_noise.insert(pot, self.intern.params.len());
            candidate.remaining_no_noise.retain_mut(|it| {
                !it.is_zero(self.func, self.intern, self.pruned, buf, &mut changed)
            });
            if candidate.remaining_no_noise.is_empty() {
                let val = self.intern.params[pot];
                self.op_dependent_vals_no_noise.remove(&val);
                changed = true;
            } else {
                self.is_noise.remove(pot);
            }

            true
        });

        changed
    }

    fn analyse_dep(
        &self,
        val: Value,
        fac: Value,
        known_linear: &mut HybridBitSet<Inst>,
        dependent_insts: &BitSet<Inst>,
    ) -> Dependency {
        let inst = match self.func.dfg.value_def(val) {
            ValueDef::Result(inst, _) => inst,
            ValueDef::Param(_) if val == fac => {
                return Dependency::Linear;
            }
            ValueDef::Const(_) | ValueDef::Param(_) => return Dependency::None,
            ValueDef::Invalid => unreachable!(),
        };

        if !dependent_insts.contains(inst) {
            return Dependency::None;
        }

        if known_linear.contains(inst) {
            return Dependency::Linear;
        }

        match self.func.dfg.insts[inst] {
            InstructionData::Binary {
                opcode: Opcode::Fadd | Opcode::Fsub | Opcode::Fmul,
                args,
            } => {
                let mut found = false;
                for arg in args {
                    match self.analyse_dep(arg, fac, known_linear, dependent_insts) {
                        Dependency::Linear => {
                            found = true;
                        }
                        Dependency::NonLinear => return Dependency::NonLinear,
                        Dependency::None => (),
                    }
                }

                if found {
                    Dependency::Linear
                } else {
                    Dependency::None
                }
            }
            InstructionData::PhiNode(ref phi) => {
                let mut found_linear = false;
                let mut found_none = false;
                for (_, arg) in self.func.dfg.phi_edges(phi) {
                    match self.analyse_dep(arg, fac, known_linear, dependent_insts) {
                        Dependency::Linear => {
                            known_linear.insert(inst, self.func.dfg.num_insts());
                            found_linear = true;
                        }
                        Dependency::NonLinear => return Dependency::NonLinear,
                        Dependency::None => {
                            found_none = true;
                        }
                    }
                }

                known_linear.remove(inst);

                match (found_linear, found_none) {
                    (true, _) => Dependency::Linear,
                    (false, true) => Dependency::None,
                    (false, false) => Dependency::NonLinear,
                }
            }

            InstructionData::Binary { opcode: Opcode::Fdiv, args: [numerator, denominator] } => {
                let denom_fac = self.analyse_dep(denominator, fac, known_linear, dependent_insts);
                if denom_fac != Dependency::None {
                    return Dependency::NonLinear;
                }
                self.analyse_dep(numerator, fac, known_linear, dependent_insts)
            }

            InstructionData::Unary { opcode: Opcode::Fneg | Opcode::OptBarrier, arg } => {
                self.analyse_dep(arg, fac, known_linear, dependent_insts)
            }
            _ => Dependency::NonLinear,
        }
    }

    fn is_linear_contrib(&mut self, arg: Value, allow_op: bool) -> bool {
        let mut is_linear = true;
        self.queue.insert(arg);

        while let Some(val) = self.queue.take() {
            for use_ in self.func.dfg.uses(val) {
                let (inst, arg) = self.func.dfg.use_to_operand(use_);
                let non_linear = match self.func.dfg.insts[inst] {
                    InstructionData::Unary {
                        opcode: Opcode::Fneg | Opcode::OptBarrier, ..
                    }
                    | InstructionData::Binary { opcode: Opcode::Fsub | Opcode::Fadd, .. }
                    | InstructionData::Call { .. }
                    | InstructionData::PhiNode(_) => false,
                    InstructionData::Binary { opcode: Opcode::Fmul, args } => {
                        let factor = args[(arg == 0) as usize];
                        allow_op || self.is_op_depenent::<false>(factor)
                    }

                    InstructionData::Binary { opcode: Opcode::Fdiv, args: [_, factor] }
                        if arg == 0 =>
                    {
                        allow_op || self.is_op_depenent::<false>(factor)
                    }

                    _ => true,
                };

                if non_linear {
                    is_linear = false;
                    break;
                }

                for res in self.func.dfg.inst_results(inst) {
                    self.queue.insert(*res);
                }
            }
        }

        self.queue.clear();
        is_linear
    }

    fn remove_linear_ddt_unkowns(&mut self, first: bool, permanent: bool) -> bool {
        let mut changed = false;

        for equation in self.intern.implicit_equations.keys() {
            if let ImplicitEquationKind::UnresolvedDdt(arg) =
                self.intern.implicit_equations[equation]
            {
                let (param, &unkown) =
                    self.intern.params.unwrap_index_and_val(&ParamKind::ImplicitUnknown(equation));

                if self.func.dfg.value_dead(unkown) {
                    continue;
                }

                // we have a constant..
                if !self.is_op_depenent::<false>(arg) {
                    self.func.dfg.replace_uses(unkown, F_ZERO);
                    self.func.dfg.replace_uses(arg, F_ZERO);
                    let arg = strip_optbarrier(self.func, arg);
                    if self.func.dfg.value_def(arg).inst().is_some() {
                        self.func.dfg.replace_uses(arg, F_ZERO);
                    }
                    self.intern.implicit_equations[equation] = ImplicitEquationKind::LinearDdt;
                    changed = true;
                    continue;
                }

                let is_small_signal = !first && !self.is_op_depenent::<true>(arg);
                let requires_unkown = !self.is_linear_contrib(arg, is_small_signal);

                let new_arg = if requires_unkown {
                    if !permanent {
                        if is_small_signal {
                            self.op_dependent_vals_no_noise.remove(&unkown);
                            self.is_noise.insert(param, self.intern.params.len());
                        }
                        continue;
                    }

                    self.intern.implicit_equations[equation] = ImplicitEquationKind::Ddt;
                    F_ZERO
                } else {
                    self.func.dfg.replace_uses(unkown, F_ZERO);
                    self.intern.implicit_equations[equation] = ImplicitEquationKind::LinearDdt;
                    strip_optbarrier(self.func, arg)
                };

                if !permanent {
                    changed = true;
                }

                self.func.dfg.replace_uses(arg, new_arg);
            }
        }

        changed
    }
}

fn is_zero(
    func: &Function,
    val: Value,
    zeros: &HybridBitSet<Param>,
    visited: &mut BitSet<Inst>,
) -> bool {
    if val == F_ZERO {
        return true;
    }

    let inst = match func.dfg.value_def(val) {
        ValueDef::Result(inst, _) => inst,

        ValueDef::Param(param) if zeros.contains(param) => {
            return true;
        }
        ValueDef::Const(_) | ValueDef::Param(_) => return false,
        ValueDef::Invalid => unreachable!(),
    };

    match func.dfg.insts[inst] {
        InstructionData::Binary { opcode: Opcode::Fadd | Opcode::Fsub | Opcode::Hypot, args } => {
            args.into_iter().all(|arg| is_zero(func, arg, zeros, visited))
        }
        InstructionData::Binary { opcode: Opcode::Fmul, args } => {
            args.into_iter().any(|arg| is_zero(func, arg, zeros, visited))
        }
        InstructionData::PhiNode(ref phi) => {
            if !visited.insert(inst) {
                visited.remove(inst);
                return false;
            }
            let res = func.dfg.phi_edges(phi).all(|(_, val)| is_zero(func, val, zeros, visited));
            visited.remove(inst);
            res
        }

        InstructionData::Binary { opcode: Opcode::Fdiv, args: [arg, _] }
        | InstructionData::Unary {
            opcode:
                Opcode::Fneg
                | Opcode::OptBarrier
                | Opcode::Sin
                | Opcode::Tan
                | Opcode::Tanh
                | Opcode::Sinh
                | Opcode::Atan
                | Opcode::Atanh
                | Opcode::Sqrt,
            arg,
        } => is_zero(func, arg, zeros, visited),
        _ => false,
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum CandidateStatus {
    Invalid,
    VoltageSrc,
    CurrentSrc,
    EmulatedVoltageSrc,
}

impl CandidateStatus {
    fn update(&mut self, is_linear: bool) {
        match *self {
            CandidateStatus::CurrentSrc => {
                if is_linear {
                    *self = CandidateStatus::EmulatedVoltageSrc;
                }
            }
            CandidateStatus::Invalid | CandidateStatus::EmulatedVoltageSrc => (),
            _ => {
                *self = CandidateStatus::Invalid;
            }
        }
    }

    fn new(is_voltage_src: bool, is_linear: bool) -> CandidateStatus {
        if is_voltage_src {
            CandidateStatus::VoltageSrc
        } else if is_linear {
            CandidateStatus::EmulatedVoltageSrc
        } else {
            CandidateStatus::CurrentSrc
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
struct RemainingContrib {
    resist: Option<Value>,
    react: Option<Value>,
    current: Option<Param>,
    has_noise: bool,
}

impl RemainingContrib {
    fn is_zero(
        &mut self,
        func: &mut Function,
        intern: &HirInterner,
        zeros: &mut HybridBitSet<Param>,
        visited: &mut BitSet<Inst>,
        changed: &mut bool,
    ) -> bool {
        if let Some(resist) = self.resist.take() {
            if !is_zero(func, resist, zeros, visited) {
                self.resist = Some(resist);
                return false;
            }
        }

        if let Some(react) = self.react.take() {
            if !is_zero(func, react, zeros, visited) {
                self.react = Some(react);
                return false;
            }
        }

        if let Some(current) = self.current {
            if self.has_noise {
                zeros.insert(current, intern.params.len());
            } else {
                let val = intern.params[current];
                func.dfg.replace_uses(val, F_ZERO);
            }
            *changed = true;
        }

        true
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct PruneCandidate {
    remaining: Vec<RemainingContrib>,
    remaining_no_noise: Vec<RemainingContrib>,
    status: CandidateStatus,
    has_noise: bool,
}

type PruneCandidates = IndexMap<NodeId, PruneCandidate, ahash::RandomState>;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Dependency {
    Linear,
    NonLinear,
    None,
}
