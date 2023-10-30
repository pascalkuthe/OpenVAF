use std::mem::take;

use hir::Node;
use indexmap::IndexMap;
use mir::{Const, InstructionData, Opcode, Value, ValueDef, FALSE, F_ZERO};

use crate::topology::Builder;
use crate::util::{add, update_optbarrier};

/// This is somewhat similar to the value lattices/flat sets used during
/// constant propagation. By representing the set of possible constants
/// that can be taken on by a value. The Elem variant is replaced by two
/// other variants representing zero and the top set represents any
/// set of non-zero values.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum FlatSet {
    /// Known to be non-zero
    Top,
    /// Value is not yet known
    Bottom,
    /// Value is know to be zero
    Zero,
}

#[derive(Debug)]
struct Candidate {
    kind: CandidateKind,
    resist: Vec<Value>,
    react: Vec<Value>,
}

impl Candidate {
    fn as_node(&self) -> Option<Value> {
        match self.kind {
            CandidateKind::Node { potential, .. } => Some(potential),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq)]
enum Dependency {
    NonLinear,
    Linear,
    /// values statically known to be non-zero (constants)
    NonZero,
    Independent,
}

#[derive(Debug)]
enum CandidateKind {
    Node { potential: Value },
    Flow { flow: Value },
}

impl Builder<'_> {
    /// explores all `candidates` to extract the noise network
    fn solve(&mut self, candidates: &mut Vec<Candidate>) {
        loop {
            let mut changed = false;
            candidates.retain_mut(|candidate| {
                if let Some(node) = candidate.as_node() {
                    self.topology.small_signal_vals.insert(node);
                }
                let mut set = FlatSet::Zero;
                for &val in &candidate.resist {
                    set = set.min(self.analyze_value(val, 20));
                }
                for &val in &candidate.react {
                    set = set.min(self.analyze_value(val, 20));
                }
                if set == FlatSet::Zero {
                    match candidate.kind {
                        CandidateKind::Node { .. } => {
                            cov_mark::hit!(node_is_small_signal);
                        }
                        CandidateKind::Flow { flow } => {
                            self.topology.small_signal_vals.insert(flow);
                        }
                    }
                    changed = true;
                } else if let Some(node) = candidate.as_node() {
                    self.topology.small_signal_vals.remove(&node);
                }
                set == FlatSet::Bottom
            });
            if !changed {
                break;
            }
        }
    }

    /// analyzes a value (with limited recursion depht) to potenitally determine if
    /// its statically always known to be zero.
    fn analyze_value(&self, val: Value, mut recurse: u32) -> FlatSet {
        if let Some(it) = recurse.checked_sub(1) {
            recurse = it;
        } else {
            return FlatSet::Top;
        }

        let is_op_dependent = |val| {
            if let Some(inst) = self.func.dfg.value_def(val).inst() {
                self.op_dependent_insts.contains(inst)
            } else {
                self.op_dependent_vals.contains(&val)
            }
        };
        let inst = match self.func.dfg.value_def(val) {
            ValueDef::Result(inst, _) => inst,
            ValueDef::Param(_) if self.topology.small_signal_vals.contains(&val) => {
                return FlatSet::Zero;
            }
            ValueDef::Const(_) if val == F_ZERO => return FlatSet::Zero,
            ValueDef::Const(_) => return FlatSet::Top,
            ValueDef::Param(_) => return FlatSet::Bottom,
            ValueDef::Invalid => unreachable!(),
        };

        match self.func.dfg.insts[inst] {
            InstructionData::Binary { opcode: Opcode::Fadd | Opcode::Fsub, args } => {
                let arg0 = self.analyze_value(args[0], recurse);
                let arg1 = self.analyze_value(args[1], recurse);
                arg0.min(arg1)
            }
            InstructionData::Binary { opcode: Opcode::Fdiv, args } => {
                if is_op_dependent(args[1]) {
                    return FlatSet::Top;
                }
                self.analyze_value(args[0], recurse)
            }
            InstructionData::Binary { opcode: Opcode::Fmul, args } => {
                let arg0 = self.analyze_value(args[0], recurse);
                let arg1 = self.analyze_value(args[1], recurse);
                arg0.max(arg1)
            }
            InstructionData::PhiNode(ref phi) => {
                let mut res = FlatSet::Zero;
                for (_, arg) in self.func.dfg.phi_edges(phi) {
                    let dep = self.analyze_value(arg, recurse);
                    if dep < res {
                        res = dep;
                        if res == FlatSet::Top {
                            break;
                        }
                    }
                }
                res
            }

            InstructionData::Unary { opcode: Opcode::Fneg | Opcode::OptBarrier, arg } => {
                self.analyze_value(arg, recurse)
            }
            _ => FlatSet::Top,
        }
    }

    fn analyze_dependency(&self, mut recurse: u32, val: Value, unknown: Value) -> Dependency {
        let inst = match self.func.dfg.value_def(val) {
            ValueDef::Result(inst, _) => inst,
            ValueDef::Param(_) if unknown == val => {
                return Dependency::Linear;
            }
            ValueDef::Const(Const::Float(val_)) if val != F_ZERO && val_.is_finite() => {
                return Dependency::NonZero
            }
            ValueDef::Param(_) | ValueDef::Const(_) => return Dependency::Independent,
            ValueDef::Invalid => unreachable!(),
        };
        if !self.scratch_buf.contains(inst) {
            return Dependency::Independent;
        }
        if let Some(it) = recurse.checked_sub(1) {
            recurse = it;
        } else {
            return Dependency::NonLinear;
        }

        match self.func.dfg.insts[inst] {
            InstructionData::Binary { opcode: Opcode::Fadd | Opcode::Fsub, args } => {
                let arg0 = self.analyze_dependency(recurse, args[0], unknown);
                if arg0 == Dependency::NonLinear {
                    return Dependency::NonLinear;
                }
                let arg1 = self.analyze_dependency(recurse, args[1], unknown);
                match (arg0, arg1) {
                    (Dependency::NonLinear, _)
                    | (_, Dependency::NonLinear)
                    | (Dependency::Linear, Dependency::Linear) => Dependency::NonLinear,
                    (Dependency::Linear, _) | (_, Dependency::Linear) => Dependency::Linear,
                    _ => Dependency::Independent,
                }
            }
            InstructionData::Binary { opcode: Opcode::Fdiv, args } => {
                let arg0 = self.analyze_dependency(recurse, args[0], unknown);
                if arg0 == Dependency::NonLinear {
                    return Dependency::NonLinear;
                }
                let arg1 = self.analyze_dependency(recurse, args[1], unknown);
                match (arg0, arg1) {
                    (_, Dependency::Linear | Dependency::NonLinear)
                    | (Dependency::NonLinear, _) => Dependency::NonLinear,
                    (Dependency::Linear, Dependency::NonZero | Dependency::Independent) => {
                        Dependency::Linear
                    }
                    _ => Dependency::Independent,
                }
            }
            InstructionData::Binary { opcode: Opcode::Fmul, args } => {
                let arg0 = self.analyze_dependency(recurse, args[0], unknown);
                if arg0 == Dependency::NonLinear {
                    return Dependency::NonLinear;
                }
                let arg1 = self.analyze_dependency(recurse, args[1], unknown);
                match (arg0, arg1) {
                    (_, Dependency::Independent) | (Dependency::Independent, _) => {
                        Dependency::Independent
                    }
                    _ => Dependency::Linear,
                }
            }
            InstructionData::PhiNode(ref phi) => {
                let mut is_linear = true;
                for (_, arg) in self.func.dfg.phi_edges(phi) {
                    match self.analyze_dependency(recurse, arg, unknown) {
                        Dependency::Linear => (),
                        Dependency::NonZero | Dependency::Independent => {
                            is_linear = false;
                        }
                        Dependency::NonLinear => return Dependency::NonLinear,
                    }
                }
                if is_linear {
                    Dependency::Linear
                } else {
                    Dependency::Independent
                }
            }

            InstructionData::Unary { opcode: Opcode::Fneg | Opcode::OptBarrier, arg } => {
                self.analyze_dependency(recurse, arg, unknown)
            }
            _ => Dependency::Independent,
        }
    }

    fn has_linear_dependency(&mut self, vals: impl Iterator<Item = Value>, unknown: Value) -> bool {
        let Self { func, scratch_buf, postorder, .. } = self;

        postorder.clear();
        scratch_buf.clear();
        let mut transversal =
            func.dfg.uses_postorder_with(unknown, (take(scratch_buf), Vec::new()), |_| true);
        (&mut transversal).for_each(|_| ());
        *scratch_buf = transversal.visited;
        let mut found_linear = false;
        for val in vals {
            match self.analyze_dependency(20, val, unknown) {
                Dependency::NonLinear => return false,
                Dependency::Linear if found_linear => return false,
                Dependency::Linear => found_linear = true,
                Dependency::NonZero | Dependency::Independent => (),
            }
        }
        found_linear
    }

    /// Moves contributions form the small signal network to the large network
    /// into a separate dimension where possible. This avoids the creation
    /// of unneeded derivatives
    pub(super) fn prune_small_signal(&mut self) {
        let mut candidates = self.collect_candidates();
        self.solve(&mut candidates);
        let small_signal_vals = take(&mut self.topology.small_signal_vals);
        for &val in &small_signal_vals {
            if let Some(contributes) = self.collect_linear_contributes(val) {
                // create placeholder since all uses of val will be replaced with 0
                // but we obviously still need it
                let placeholder = self.func.dfg.make_invalid_value();
                self.create_dimension(placeholder, val);
                for contribute in contributes {
                    let dimension = self.val_map[&contribute];
                    let contribute = self.topology.as_contribution(contribute).unwrap();
                    let reactive = contribute.is_reactive();
                    cov_mark::hit!(prune_small_signal);
                    let contribute = self.topology.get_mut(contribute);
                    let val = if reactive {
                        &mut contribute.react_small_signal
                    } else {
                        &mut contribute.resist_small_signal
                    };
                    update_optbarrier(self.func, val, |mut val, cursor| {
                        add(cursor, &mut val, dimension, false);
                        val
                    });
                }
                self.func.dfg.replace_uses(placeholder, val);
            }
        }
        self.topology.small_signal_vals = small_signal_vals;
    }

    fn collect_candidates(&mut self) -> Vec<Candidate> {
        let mut nodes = IndexMap::with_capacity_and_hasher(32, ahash::RandomState::new());
        let mut candidates = Vec::new();
        for (_, (&branch, contributes)) in self.topology.branches() {
            let (hi, lo) = branch.nodes(self.db);
            let is_current_src = contributes.is_voltage_src == FALSE;
            let potential =
                contributes.voltage_src.unknown.filter(|&val| !self.func.dfg.value_dead(val));
            let flow =
                contributes.current_src.unknown.filter(|&val| !self.func.dfg.value_dead(val));
            if let Some(potential) = potential {
                let mut register_node = |node: Node, valid: bool| {
                    if node.is_port(self.db) {
                        cov_mark::hit!(port_not_small_signal)
                    } else {
                        let (candidate, valid_) = nodes.entry(node).or_insert((
                            Candidate {
                                kind: CandidateKind::Node { potential },
                                resist: Vec::new(),
                                react: Vec::new(),
                            },
                            true,
                        ));
                        *valid_ = *valid_ && valid;
                        if contributes.current_src.resist != F_ZERO {
                            candidate.resist.push(contributes.current_src.resist);
                        }
                        if contributes.current_src.react != F_ZERO {
                            candidate.react.push(contributes.current_src.react);
                        }
                    }
                };
                if let Some(lo) = lo {
                    register_node(hi, false);
                    register_node(lo, false);
                } else {
                    register_node(hi, is_current_src);
                }
            }
            if let Some(flow) = flow.filter(|_| is_current_src) {
                let resist = if contributes.current_src.resist == F_ZERO {
                    Vec::new()
                } else {
                    vec![contributes.current_src.resist]
                };
                let react = if contributes.current_src.react == F_ZERO {
                    Vec::new()
                } else {
                    vec![contributes.current_src.react]
                };
                if !resist.is_empty() || !react.is_empty() {
                    candidates.push(Candidate { kind: CandidateKind::Flow { flow }, resist, react })
                }
            }
        }
        candidates.extend(nodes.into_values().filter_map(|(candidate, mut valid)| {
            if valid {
                let unknown = candidate.as_node().unwrap();
                valid = self.has_linear_dependency(candidate.resist.iter().copied(), unknown);
            }
            valid.then_some(candidate)
        }));
        candidates
    }

    /// returns the list of contributes that `val` is used in if and only if turning
    /// these contributions value into a separate dimension is valid. Specifically that
    /// means that all partial deriveves of these contrbituions (and the contributions themselves)
    /// do not changed their value if `val` zero (unless derived by itself)
    fn collect_linear_contributes(&mut self, val: Value) -> Option<Vec<Value>> {
        let Self { func, scratch_buf, postorder, .. } = self;

        postorder.clear();
        scratch_buf.clear();
        let mut transversal =
            func.dfg.uses_postorder_with(val, (take(scratch_buf), Vec::new()), |_| true);
        postorder.extend(&mut transversal);
        *scratch_buf = transversal.visited;

        let is_op_dependent = |val| {
            if let Some(inst) = func.dfg.value_def(val).inst() {
                self.op_dependent_insts.contains(inst)
            } else {
                self.op_dependent_vals.contains(&val)
            }
        };
        let mut res = Vec::new();
        for &inst in postorder.iter() {
            match func.dfg.insts[inst] {
                InstructionData::Call { .. } | InstructionData::Branch { .. } => {
                    continue;
                }
                InstructionData::Binary {
                    opcode: Opcode::Fadd | Opcode::Fsub | Opcode::Fmul,
                    ..
                }
                | InstructionData::PhiNode(_)
                | InstructionData::Unary { opcode: Opcode::Fneg | Opcode::OptBarrier, .. } => {}
                InstructionData::Binary { opcode: Opcode::Fdiv, args } => {
                    if is_op_dependent(args[1]) {
                        return None;
                    }
                }
                _ => {
                    return None;
                }
            }
            let val = func.dfg.first_result(inst);
            if self.topology.as_contribution(val).is_some() {
                res.push(val);
            }
        }
        (!res.is_empty()).then_some(res)
    }
}
