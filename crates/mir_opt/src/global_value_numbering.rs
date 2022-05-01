use std::cmp::Ordering;
use std::hash::{BuildHasher, Hash, Hasher};

use std::mem::{swap, ManuallyDrop};
use std::ops::{Index, IndexMut};

use ahash::RandomState;
use bitset::{BitSet, HybridBitSet};
use hashbrown::raw::RawTable;
use mir::{Block, FuncRef, Function, Inst, InstructionData, Opcode, Value, ValueDef, ValueList};
use stdx::packed_option::PackedOption;
use stdx::{impl_idx_from, impl_idx_math};
use typed_index_collections::TiVec;

use crate::dominators::DominatorTree;
use crate::simplify::SimplifyCtx;

struct GVNExpression {
    opcode: Opcode,
    payload: GVNExprPayLoad,
}

impl Clone for GVNExpression {
    fn clone(&self) -> Self {
        let payload = unsafe { std::ptr::read(&self.payload) };
        Self { opcode: self.opcode, payload }
    }
}

union GVNExprPayLoad {
    default: DefaultExprPayLoad,
    // this is basically copy but ihat just makes the API of ValueList really akward
    phi: ManuallyDrop<PhiExprPayLoad>,
    call: ManuallyDrop<CallExprPayLoad>,
}

impl GVNExprPayLoad {
    fn default(&self) -> DefaultExprPayLoad {
        unsafe { self.default }
    }
    fn phi(&self) -> &PhiExprPayLoad {
        unsafe { &*self.phi }
    }

    fn phi_mut(&mut self) -> &mut PhiExprPayLoad {
        unsafe { &mut *self.phi }
    }

    fn call(&self) -> &CallExprPayLoad {
        unsafe { &*self.call }
    }

    fn call_mut(&mut self) -> &mut CallExprPayLoad {
        unsafe { &mut *self.call }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct DefaultExprPayLoad {
    val1: Value,
    val2: PackedOption<Value>,
}

#[derive(Clone, Debug)]
struct PhiExprPayLoad {
    bb: Block,
    args: ValueList,
}

#[derive(Clone, Debug)]
struct CallExprPayLoad {
    func_ref: FuncRef,
    args: ValueList,
}

enum ExprResult {
    Expr(GVNExpression),
    Simplified(ClassId),
}

impl ExprResult {
    fn to_class(self, inst: Inst, gvn: &mut GVN, func: &mut Function) -> ClassId {
        match self {
            ExprResult::Expr(expr) => gvn.class_map.insert_expr(inst, expr, func),
            ExprResult::Simplified(class) => class,
        }
    }
}

impl GVNExpression {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(gvn: &mut GVN, func: &mut Function, inst: Inst) -> Option<ExprResult> {
        let (opcode, val1, val2) = match func.dfg.insts[inst].clone() {
            InstructionData::Unary { opcode, mut arg } if opcode != Opcode::OptBarrier => {
                arg = gvn.get_lead_val(arg, func);

                let simplified_val = gvn.simplify_ctx(func).simplify_unary_op(opcode, arg);
                if let Some(val) = simplified_val {
                    if let Some(expr) = gvn.check_simplified(val, func) {
                        return Some(expr);
                    }
                }

                (opcode, arg, None.into())
            }
            InstructionData::Binary { opcode, args: [mut val1, mut val2] } => {
                val1 = gvn.get_lead_val(val1, func);
                val2 = gvn.get_lead_val(val2, func);
                if opcode.is_commutative() && gvn.should_swap_operands(val1, val2, func) {
                    swap(&mut val1, &mut val2);
                }

                let simplified_val = gvn.simplify_ctx(func).simplify_binop(opcode, val1, val2);
                if let Some(val) = simplified_val {
                    if let Some(expr) = gvn.check_simplified(val, func) {
                        return Some(expr);
                    }
                }

                (opcode, val1, val2.into())
            }

            InstructionData::PhiNode(phi) => {
                let simplified_val = gvn.simplify_ctx(func).simplify_phi(phi.clone());
                if let Some(val) = simplified_val {
                    if let Some(expr) = gvn.check_simplified(val, func) {
                        return Some(expr);
                    }
                }

                let bb = func.layout.inst_block(inst).unwrap();
                let mut vals = ValueList::new();

                for (_, i) in phi.blocks.iter(&func.dfg.phi_forest) {
                    let pool = &mut func.dfg.insts.value_lists;
                    let mut val = phi.args.get(i as usize, pool).unwrap();
                    val = gvn.class_map.get_lead_val(val, func);
                    let pool = &mut func.dfg.insts.value_lists;
                    vals.push(val, pool);
                }

                return Some(ExprResult::Expr(GVNExpression {
                    opcode: Opcode::Phi,
                    payload: GVNExprPayLoad {
                        phi: ManuallyDrop::new(PhiExprPayLoad { bb, args: vals }),
                    },
                }));
            }

            InstructionData::Call { func_ref, args }
                if !func.dfg.signatures[func_ref].has_sideeffects =>
            {
                let mut args = args.deep_clone(&mut func.dfg.insts.value_lists);
                for i in 0..args.len(&func.dfg.insts.value_lists) {
                    let arg =
                        unsafe { args.get(i, &func.dfg.insts.value_lists).unwrap_unchecked() };
                    let arg = gvn.class_map.get_lead_val(arg, func);
                    unsafe {
                        *args.get_mut(i, &mut func.dfg.insts.value_lists).unwrap_unchecked() = arg;
                    };
                }

                return Some(ExprResult::Expr(GVNExpression {
                    opcode: Opcode::Call,
                    payload: GVNExprPayLoad {
                        call: ManuallyDrop::new(CallExprPayLoad { func_ref, args }),
                    },
                }));
            }

            _ => return None,
        };

        Some(ExprResult::Expr(GVNExpression {
            opcode,
            payload: GVNExprPayLoad { default: DefaultExprPayLoad { val1, val2 } },
        }))
    }

    pub fn new_const(val: Value) -> GVNExpression {
        GVNExpression {
            // we don't value number optbarrier instructions so this is a nice niece optimization
            opcode: Opcode::OptBarrier,
            payload: GVNExprPayLoad {
                default: DefaultExprPayLoad { val1: val, val2: None.into() },
            },
        }
    }

    fn hash(&self, state: &RandomState, func: &Function) -> u64 {
        let mut hasher = state.build_hasher();
        self.opcode.hash(&mut hasher);
        match self.opcode {
            Opcode::Phi => {
                let PhiExprPayLoad { bb, args } = self.payload.phi();
                bb.hash(&mut hasher);
                let args = args.as_slice(&func.dfg.insts.value_lists);
                for arg in args {
                    arg.hash(&mut hasher)
                }
            }

            Opcode::Call => {
                let CallExprPayLoad { func_ref, args } = self.payload.call();
                func_ref.hash(&mut hasher);
                let args = args.as_slice(&func.dfg.insts.value_lists);
                for arg in args {
                    arg.hash(&mut hasher)
                }
            }

            _ => {
                let DefaultExprPayLoad { val1, val2 } = self.payload.default();
                val1.hash(&mut hasher);
                val2.hash(&mut hasher);
            }
        }
        hasher.finish()
    }

    fn eq(&self, other: &Self, func: &Function) -> bool {
        if self.opcode != other.opcode {
            return false;
        }

        match self.opcode {
            Opcode::Phi => {
                let PhiExprPayLoad { bb: bb1, args: args1 } = self.payload.phi();
                let PhiExprPayLoad { bb: bb2, args: args2 } = other.payload.phi();
                if bb1 != bb2 {
                    return false;
                }
                let arg1 = args1.as_slice(&func.dfg.insts.value_lists);
                let arg2 = args2.as_slice(&func.dfg.insts.value_lists);
                arg1.iter().eq(arg2)
            }
            Opcode::Call => {
                let CallExprPayLoad { func_ref: func_ref_1, args: args1 } = self.payload.call();
                let CallExprPayLoad { func_ref: func_ref_2, args: args2 } = self.payload.call();
                if func_ref_1 != func_ref_2 {
                    return false;
                }
                let vals1 = args1.as_slice(&func.dfg.insts.value_lists);
                let vals2 = args2.as_slice(&func.dfg.insts.value_lists);
                vals1.iter().eq(vals2)
            }
            _ => {
                let payload1 = self.payload.default();
                let payload2 = other.payload.default();
                payload1 == payload2
            }
        }
    }

    fn destroy(&mut self, func: &mut Function) {
        match self.opcode {
            Opcode::Phi => {
                self.payload.phi_mut().args.clear(&mut func.dfg.insts.value_lists);
            }
            Opcode::Call => {
                self.payload.call_mut().args.clear(&mut func.dfg.insts.value_lists);
            }
            _ => (),
        }
    }
}

struct EquivalenceClass {
    leader: PackedOption<Inst>,
    expr: GVNExpression,
    next_leader: (PackedOption<Inst>, DFSId),
    // insts: IndexSet<Inst, ahash::RandomState>,
    insts: HybridBitSet<DFSId>,
}

impl EquivalenceClass {
    pub fn reset_next_leader(&mut self) {
        self.next_leader = (None.into(), u32::MAX.into())
    }

    pub fn add_possible_next_leader(&mut self, inst: Inst, dfs_num: DFSId) {
        if dfs_num < self.next_leader.1 {
            self.next_leader = (inst.into(), dfs_num);
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord, Debug)]
struct DFSId(u32);
impl_idx_from!(DFSId(u32));
impl_idx_math!(DFSId(u32));

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord, Debug)]
pub struct ClassId(u32);
impl_idx_from!(ClassId(u32));
impl_idx_math!(ClassId(u32));

#[derive(Default)]
struct DFSMapping {
    inst_to_dfs: TiVec<Inst, PackedOption<DFSId>>,
    dfs_to_inst: TiVec<DFSId, Inst>,
    param_off: u32,
}

impl DFSMapping {
    fn populate(&mut self, func: &Function, dom_tree: &DominatorTree, num_params: u32) {
        self.param_off = num_params + 1;
        self.inst_to_dfs.resize(func.dfg.num_insts(), None.into());
        for bb in dom_tree.cfg_postorder().iter().rev() {
            self.map_block(*bb, func)
        }
    }
    fn map_block(&mut self, bb: Block, func: &Function) {
        for inst in func.layout.block_insts(bb) {
            let dfs_id = self.dfs_to_inst.push_and_get_key(inst);
            // if !func.dfg.inst_dead(inst, true) {
            self.inst_to_dfs[inst] = dfs_id.into();
            // }
        }
    }

    fn get_rank(&self, val: Value, func: &Function) -> u32 {
        let inst = match func.dfg.value_def(val) {
            ValueDef::Result(inst, _) => inst,
            ValueDef::Param(param) => return 1 + u32::from(param),
            ValueDef::Const(_) | ValueDef::Invalid => return 0,
        };

        match self.inst_to_dfs[inst].expand() {
            Some(val) => u32::from(val) + self.param_off,
            None => u32::MAX,
        }
    }

    fn should_swap(&self, val1: Value, val2: Value, func: &Function) -> bool {
        let rank1 = self.get_rank(val1, func);
        let rank2 = self.get_rank(val2, func);
        match rank1.cmp(&rank2) {
            Ordering::Equal => val1 > val2,
            Ordering::Less => false,
            Ordering::Greater => true,
        }
    }

    fn clear(&mut self) {
        self.inst_to_dfs.clear();
        self.dfs_to_inst.clear();
    }
}

#[derive(Default)]
struct ClassMap {
    inst_class: TiVec<Inst, PackedOption<ClassId>>,
    expr_class: RawTable<ClassId>,
    classes: TiVec<ClassId, EquivalenceClass>,
    state: RandomState,
}

impl ClassMap {
    fn init(&mut self, func: &Function) {
        self.inst_class.resize(func.dfg.num_insts(), None.into());
        self.expr_class.reserve(func.dfg.num_insts(), |_| unreachable!());
    }

    fn remove_expr_class(&mut self, func: &mut Function, class: ClassId) {
        self.classes[class].insts = HybridBitSet::new_empty();
        let hash = self.classes[class].expr.hash(&self.state, func);
        self.expr_class.remove_entry(hash, |it| *it == class);
        self.classes[class].expr.destroy(func)
    }

    fn insert_expr(&mut self, inst: Inst, mut expr: GVNExpression, func: &mut Function) -> ClassId {
        let hash = expr.hash(&self.state, func);
        if let Some(class) =
            self.expr_class.get(hash, |class| self.classes[*class].expr.eq(&expr, func))
        {
            expr.destroy(func);
            *class
        } else {
            let new_class = EquivalenceClass {
                leader: inst.into(),
                expr,
                next_leader: (None.into(), u32::MAX.into()),
                insts: HybridBitSet::new_empty(),
            };
            let new_class = self.classes.push_and_get_key(new_class);
            unsafe {
                self.expr_class.insert_no_grow(hash, new_class);
            }

            new_class
        }
    }

    fn get_lead_val(&self, val: Value, func: &Function) -> Value {
        if let ValueDef::Result(inst, _) = func.dfg.value_def(val) {
            if let Some(class) = self.inst_class[inst].expand() {
                let class = &self.classes[class];
                if class.expr.opcode == Opcode::OptBarrier {
                    return class.expr.payload.default().val1;
                } else {
                    return func.dfg.first_result(class.leader.unwrap_unchecked());
                }
            }
        }

        val
    }

    fn clear(&mut self, func: &mut Function) {
        for class in self.classes.iter_mut() {
            class.expr.destroy(func)
        }

        self.inst_class.clear();
        self.expr_class.clear_no_drop();
        self.classes.clear();
    }
}

impl Index<ClassId> for ClassMap {
    type Output = EquivalenceClass;

    fn index(&self, index: ClassId) -> &Self::Output {
        &self.classes[index]
    }
}

impl IndexMut<ClassId> for ClassMap {
    fn index_mut(&mut self, index: ClassId) -> &mut Self::Output {
        &mut self.classes[index]
    }
}

#[derive(Default)]
pub struct GVN {
    class_map: ClassMap,
    dfs_map: DFSMapping,
    leader_changes: HybridBitSet<DFSId>,
    touched_insts: BitSet<DFSId>,
}

impl GVN {
    pub fn init(&mut self, func: &Function, dom_tree: &DominatorTree, num_params: u32) {
        self.dfs_map.populate(func, dom_tree, num_params);
        self.class_map.init(func);
        self.touched_insts.insert_all();
        self.touched_insts.ensure_enabled(self.dfs_map.dfs_to_inst.len())
    }

    pub fn inst_class(&self, inst: Inst) -> PackedOption<ClassId> {
        self.class_map.inst_class[inst]
    }

    pub fn num_class(&self) -> usize {
        self.class_map.classes.len()
    }

    pub fn clear(&mut self, func: &mut Function) {
        self.class_map.clear(func);
        self.dfs_map.clear();
        self.leader_changes = HybridBitSet::new_empty();
    }

    fn simplify_ctx<'a>(
        &'a mut self,
        func: &'a mut Function,
    ) -> SimplifyCtx<f64, impl Fn(Value, &Function) -> Value + 'a> {
        SimplifyCtx::new(func, |val, func| self.class_map.get_lead_val(val, func))
    }

    fn check_simplified(&self, val: Value, func: &Function) -> Option<ExprResult> {
        let inst = match func.dfg.value_def(val) {
            ValueDef::Result(inst, _) => inst,
            // can't get any better than this ;)
            ValueDef::Param(_) | ValueDef::Const(_) => {
                return Some(ExprResult::Expr(GVNExpression::new_const(val)))
            }

            ValueDef::Invalid => unreachable!(),
        };

        self.class_map.inst_class[inst].expand().map(ExprResult::Simplified)
    }

    fn get_lead_val(&self, val: Value, func: &Function) -> Value {
        self.class_map.get_lead_val(val, func)
    }

    fn should_swap_operands(&self, val1: Value, val2: Value, func: &Function) -> bool {
        self.dfs_map.should_swap(val1, val2, func)
    }

    pub fn solve(&mut self, func: &mut Function) {
        loop {
            for dfs_id in 0..self.dfs_map.dfs_to_inst.len() {
                let dfs_id = dfs_id.into();
                if self.touched_insts.remove(dfs_id) {
                    let inst = self.dfs_map.dfs_to_inst[dfs_id];
                    self.process_inst(func, inst)
                }
            }
            if self.touched_insts.is_empty() {
                break;
            }
        }
    }

    pub fn remove_unnecessary_insts(&mut self, func: &mut Function, dom_tree: &DominatorTree) {
        for class in self.class_map.classes.iter_mut() {
            if class.expr.opcode == Opcode::OptBarrier {
                let const_val = class.expr.payload.default().val1;
                for dfs_id in class.insts.iter() {
                    let inst = self.dfs_map.dfs_to_inst[dfs_id];
                    let val = func.dfg.first_result(inst);
                    func.dfg.replace_uses(val, const_val);
                }
                class.insts.clear();
                continue;
            }

            if matches!(&class.insts , HybridBitSet::Sparse(insts) if insts.len() < 2) {
                continue;
            }

            let mut leader = None;
            let mut leaders = HybridBitSet::new_empty();
            for dfs_id in class.insts.iter() {
                let inst = self.dfs_map.dfs_to_inst[dfs_id];

                if let Some(leader_inst) = leader {
                    let block = func.layout.inst_block(inst).unwrap();
                    let dominator = func.layout.inst_block(leader_inst).unwrap();
                    if dom_tree.dominates(block, dominator) {
                        // a equivalent instruction dominates `inst` we can remove it
                        let dest = func.dfg.first_result(inst);
                        let src = func.dfg.first_result(leader_inst);
                        func.dfg.replace_uses(dest, src);
                        func.dfg.zap_inst(inst);
                        func.layout.remove_inst(inst);
                        continue;
                    }
                }

                leaders.insert(dfs_id, self.dfs_map.dfs_to_inst.len());
                leader = Some(inst);
            }

            class.insts = leaders
        }
    }

    fn process_inst(&mut self, func: &mut Function, inst: Inst) {
        if let Some(res) = GVNExpression::new(self, func, inst) {
            let eclass = res.to_class(inst, self, func);
            self.update_congurence_class(func, inst, eclass)
        }
    }

    fn update_congurence_class(&mut self, func: &mut Function, inst: Inst, eclass: ClassId) {
        let iclass = self.class_map.inst_class[inst];
        let class_changed = iclass != eclass.into();

        if class_changed {
            self.move_to_class(func, inst, iclass, eclass);
            debug_assert_ne!(self.class_map.inst_class[inst], None.into());
        }

        let dfs_id = self.dfs_map.inst_to_dfs[inst].unwrap_unchecked();
        let leader_changed = self.leader_changes.remove(dfs_id);

        if leader_changed || class_changed {
            for use_ in func.dfg.inst_uses(inst) {
                let inst = func.dfg.use_to_operand(use_).0;
                let dfs_id = self.dfs_map.inst_to_dfs[inst].unwrap_unchecked();
                self.touched_insts.insert(dfs_id);
            }
        }
    }

    /// Move a value, currently in OldClass, to be part of NewClass
    /// Update OldClass and NewClass for the move (including changing leaders, etc).
    fn move_to_class(
        &mut self,
        func: &mut Function,
        inst: Inst,
        old_class: PackedOption<ClassId>,
        new_class: ClassId,
    ) {
        let dfs_id = self.dfs_map.inst_to_dfs[inst].unwrap_unchecked();

        if let Some(old_class) = old_class.expand() {
            let old_class_ = &mut self.class_map[old_class];
            if old_class_.next_leader.0 == inst.into() {
                old_class_.reset_next_leader();
            }

            old_class_.insts.remove(dfs_id);
            if old_class_.insts.is_empty() {
                self.class_map.remove_expr_class(func, old_class)
            }
        }

        let new_class_ = &mut self.class_map[new_class];

        new_class_.insts.insert(dfs_id, func.dfg.num_insts());

        if new_class_.leader != inst.into() {
            new_class_.add_possible_next_leader(inst, dfs_id);
        }

        self.class_map.inst_class[inst] = new_class.into();

        if let Some(old_class) = old_class.expand() {
            let old_class_ = &mut self.class_map[old_class];
            if old_class_.insts.is_empty() {
                old_class_.leader = None.into();
            } else if old_class_.leader == inst.into() {
                let leader = if let Some(next_leader) = old_class_.next_leader.0.expand() {
                    next_leader
                } else {
                    let dfs_id = old_class_.insts.iter().max().unwrap();

                    self.dfs_map.dfs_to_inst[dfs_id]
                };
                old_class_.leader = leader.into();
                old_class_.reset_next_leader();
                self.touched_insts.union(&old_class_.insts);
                self.leader_changes.union(&old_class_.insts, func.dfg.num_insts());
            }
        }
    }
}
