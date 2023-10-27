use ahash::{AHashMap, AHashSet, RandomState};
use bitset::{BitSet, SparseBitMatrix};
use hir::{CompilationDB, Type};
use hir_lower::{HirInterner, ParamKind, PlaceKind};
use indexmap::IndexMap;
use mir::builder::InstBuilder;
use mir::cursor::{Cursor, FuncCursor};
use mir::{
    strip_optbarrier, Block, ControlFlowGraph, DominatorTree, FuncRef, Function, Inst,
    InstructionData, Opcode, Value, FALSE,
};
use mir_opt::{aggressive_dead_code_elimination, simplify_cfg, ClassId, GVN};
use stdx::packed_option::PackedOption;
use stdx::{impl_debug_display, impl_idx_from};
use typed_indexmap::TiMap;

use crate::context::Context;
use crate::util::strip_optbarrier_if_const;

#[cfg(test)]
mod tests;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct CacheSlot(pub u32);
impl_idx_from!(CacheSlot(u32));
impl_debug_display! {match CacheSlot{CacheSlot(id) => "cslot{id}";}}

/// The part of the model that is operating point independent and can be
/// computed at the start of the simulation and cached afterwards
#[derive(Debug)]
pub struct Initialization {
    pub func: Function,
    pub intern: HirInterner,
    pub cached_vals: IndexMap<Value, CacheSlot, RandomState>,
    pub cache_slots: TiMap<CacheSlot, (PackedOption<ClassId>, u32), hir::Type>,
}

impl Initialization {
    pub(super) fn new(cx: &mut Context<'_>, gvn: GVN) -> Initialization {
        let mut builder = Builder::new(cx);
        for _ in 0..builder.func.layout.num_blocks() {
            builder.init.func.layout.make_block();
        }
        let mut blocks = builder.func.layout.blocks_cursor();
        while let Some(bb) = blocks.next(&builder.func.layout) {
            builder.split_block(bb);
        }
        let collapse_implicit = builder.build_init_itern();
        builder.build_init_cache(&gvn, &collapse_implicit);
        builder.optimize(collapse_implicit);
        builder.init
    }
}

struct Builder<'a> {
    init: Initialization,
    init_cache: IndexMap<Value, Inst, RandomState>,
    // inputs
    func: &'a mut Function,
    cfg: &'a mut ControlFlowGraph,
    dom_tree: &'a mut DominatorTree,
    intern: &'a mut HirInterner,
    op_dependent_insts: &'a BitSet<Inst>,
    output_values: &'a BitSet<Value>,
    db: &'a CompilationDB,
    // temporary state
    val_map: AHashMap<Value, Value>,
    control_dep: SparseBitMatrix<Block, Block>,
}

impl<'a> Builder<'a> {
    fn new(ctx: &'a mut Context) -> Builder<'a> {
        Builder {
            init: Initialization {
                func: Function::with_name(format!("{}_init", &ctx.func.name)),
                cached_vals: IndexMap::with_capacity_and_hasher(128, RandomState::new()),
                cache_slots: TiMap::default(),
                intern: HirInterner::default(),
            },
            init_cache: IndexMap::with_capacity_and_hasher(256, RandomState::default()),
            func: &mut ctx.func,
            cfg: &mut ctx.cfg,
            dom_tree: &mut ctx.dom_tree,
            intern: &mut ctx.intern,
            op_dependent_insts: &ctx.op_dependent_insts,
            output_values: &ctx.output_values,
            db: ctx.db,
            val_map: AHashMap::with_capacity(1024),
            control_dep: SparseBitMatrix::new_square(0),
        }
    }

    fn split_block(&mut self, bb: Block) {
        self.init.func.layout.append_block(bb);
        let mut bb_cursor = self.func.layout.block_inst_cursor(bb);
        while let Some(inst) = bb_cursor.next(&self.func.layout) {
            // don't copy operating point dependent instructions to the new
            // function
            if self.op_dependent_insts.contains(inst) {
                match self.func.dfg.insts[inst] {
                    // We need to copy terminators to ensure the control structure remains intact
                    InstructionData::Branch { else_dst: destination, .. }
                    | InstructionData::Jump { destination } => {
                        FuncCursor::new(&mut self.init.func).at_bottom(bb).ins().jump(destination);
                        let srcloc = self.func.srclocs.get(inst).copied().unwrap_or_default();
                        self.init.func.srclocs.push(srcloc);
                    }
                    // Some callbacks (collapse hints) have no meaning if not operating point dependent
                    InstructionData::Call { func_ref, .. }
                        if self.intern.callbacks[func_ref].ignore_if_op_dependent() =>
                    {
                        cov_mark::hit!(ignore_if_op_dependent);
                        self.func.dfg.zap_inst(inst);
                        self.func.layout.remove_inst(inst);
                    }
                    _ => (),
                }
            } else {
                self.copy_instruction(inst, bb)
            }
        }
    }

    fn copy_callback(&mut self, cb: FuncRef) -> FuncRef {
        let cb = self.intern.callbacks[cb].clone();
        let signature = cb.signature();
        let (func, changed) = self.init.intern.callbacks.ensure(cb);
        if changed {
            let func_ = self.init.func.import_function(signature);
            debug_assert_eq!(func_, func);
        }
        func
    }

    fn copy_instruction(&mut self, inst: Inst, bb: Block) {
        // copy the instruction to the new function
        let mut inst_data = self.func.dfg.insts[inst].to_pool(
            &self.func.dfg.insts.value_lists,
            &self.func.dfg.phi_forest,
            &mut self.init.func.dfg.insts.value_lists,
            &mut self.init.func.dfg.phi_forest,
        );
        if let InstructionData::Call { func_ref, .. } = &mut inst_data {
            *func_ref = self.copy_callback(*func_ref)
        }
        let mut map_val = |val| {
            *self.val_map.entry(val).or_insert_with(|| {
                if let Some(val) = self.func.dfg.value_def(val).as_const() {
                    self.init.func.dfg.values.make_const(val)
                } else {
                    self.init.func.dfg.values.make_invalid_value()
                }
            })
        };
        for &val in self.func.dfg.inst_results(inst) {
            map_val(val);
        }
        for val in inst_data.arguments_mut(&mut self.init.func.dfg.insts.value_lists) {
            *val = map_val(*val);
        }
        let new_inst = self.init.func.dfg.make_inst(inst_data);
        self.init.func.dfg.make_inst_results_reusing(
            new_inst,
            self.func.dfg.inst_results(inst).iter().map(|val| Some(self.val_map[val])),
        );
        self.init.func.layout.append_inst_to_bb(new_inst, bb);
        let srcloc = self.func.srclocs.get(inst).copied().unwrap_or_default();
        let new_inst_ = self.init.func.srclocs.push_and_get_key(srcloc);
        debug_assert_eq!(new_inst_, new_inst);

        // we cache all instructions that produce an output or were tagged
        // (writte to a variable by a user). These instructions can be removed.
        // Otherwise we only remove the inrsuction insrtuction if possible. If
        // its not possible we let DCE deal with later
        //
        // outputs receive special treatment as we always want to cache them
        // if possible but outputs may not be places. There are also some special
        // consideratinos required for optbarriers.
        let is_output = self.func.dfg.insts[inst].opcode() == Opcode::OptBarrier
            && self.output_values.contains(self.func.dfg.first_result(inst));
        let cache_inst = !is_output
            && self.func.dfg.inst_results(inst).iter().any(|val| self.func.dfg.tag(*val).is_some());

        if is_output {
            cov_mark::hit!(op_independent_output);
            let val = self.func.dfg.first_result(inst);
            let arg = strip_optbarrier(&*self.func, val);
            // if the argument is already cached, just keep the opt barrier
            let needs_cache = self.func.dfg.tag(arg).is_none();
            let inst = self.func.dfg.value_def(arg).inst().filter(|_| needs_cache);
            if let Some(inst) = inst {
                cov_mark::hit!(cache_output);
                let param = self.init_cache.insert_full(arg, inst).0 + self.intern.params.len();
                // needed to ensure the type is calculated correctly
                if let Some(tag) = self.func.dfg.tag(val) {
                    self.func.dfg.set_tag(arg, Some(tag));
                }
                self.func.dfg.values.make_param_at(param.into(), arg);
            }
            return;
        } else if cache_inst {
            for &val in self.func.dfg.insts.results(inst) {
                let param = self.init_cache.insert_full(val, inst).0 + self.intern.params.len();
                self.func.dfg.values.make_param_at(param.into(), val);
            }
        }

        if cache_inst
            || self.func.dfg.instr_safe_to_remove(inst)
                && !self.func.dfg.insts[inst].is_terminator()
        {
            self.func.dfg.zap_inst(inst);
            self.func.layout.remove_inst(inst);
        }
    }

    fn build_init_cache(&mut self, gvn: &GVN, collapse_implicit: &AHashSet<Value>) {
        // first run deadcode elimination on the main function to figure out which cached
        // initialization values are actually used
        self.dom_tree.compute_postdom_frontiers(self.cfg, &mut self.control_dep);
        aggressive_dead_code_elimination(
            self.func,
            self.cfg,
            &|val, _| self.output_values.contains(val),
            &self.control_dep,
        );

        // now create a cache slot for every equivalence class of values
        // that was cached
        let mut extra_class = 0;
        let mut ensure_cache_slot = |inst: Option<Inst>, res, ty| {
            let class = inst.and_then(|inst| gvn.inst_class(inst).expand());
            let equiv_class = class.unwrap_or_else(|| {
                let class = gvn.num_class() + extra_class;
                extra_class += 1;
                class.into()
            });
            self.init.cache_slots.insert_full((equiv_class.into(), res as u32), ty).0
        };
        self.init.cached_vals = self
            .init_cache
            .iter()
            .filter_map(|(&val, &old_inst)| {
                if self.func.dfg.value_dead(val)
                    && (!self.output_values.contains(val) || collapse_implicit.contains(&val))
                {
                    // make some other value here so there isn't an undefined parameter
                    self.func.dfg.values.fconst_at(0.0.into(), val);
                    return None;
                }

                let ty = if let Some(tag) = self.func.dfg.tag(val) {
                    let idx = usize::from(tag);
                    let place = self.intern.outputs.get_index(idx).unwrap().0;
                    place.ty(self.db)
                } else if collapse_implicit.contains(&val) {
                    Type::Bool
                } else {
                    Type::Real
                };

                let idx = self
                    .func
                    .dfg
                    .inst_results(old_inst)
                    .iter()
                    .position(|&res| res == val)
                    .unwrap();
                let cache_slot = ensure_cache_slot(Some(old_inst), idx, ty);

                let new_val = self.val_map[&val];
                let (new_inst, _) = self.init.func.dfg.value_def(new_val).unwrap_result();

                self.func
                    .dfg
                    .values
                    .make_param_at((cache_slot.0 as usize + self.intern.params.len()).into(), val);
                let val = FuncCursor::new(&mut self.init.func)
                    .after_inst_no_phi(new_inst)
                    .ins()
                    .ensure_optbarrier(new_val);

                Some((val, cache_slot))
            })
            .collect();
    }

    fn optimize(&mut self, collapse_implicit: AHashSet<Value>) {
        // perform final optimization/DCE
        simplify_cfg(self.func, self.cfg);
        self.cfg.compute(&self.init.func);
        aggressive_dead_code_elimination(
            &mut self.init.func,
            self.cfg,
            &|val, _| self.init.cached_vals.contains_key(&val) || collapse_implicit.contains(&val),
            &self.control_dep,
        );
        simplify_cfg(&mut self.init.func, self.cfg);
    }

    fn build_init_itern(&mut self) -> AHashSet<Value> {
        for (&kind, val) in self.intern.params.iter() {
            if let Some(&val) = self.val_map.get(val) {
                let (param, _) = self.init.intern.params.insert_full(kind, val);
                self.init.func.dfg.values.make_param_at(param, val);
            }
        }
        let mut collapse_implicit = AHashSet::new();
        for (&kind, val) in &mut self.intern.outputs {
            if let PlaceKind::CollapseImplicitEquation(eq) = kind {
                if let Some(mut val) = val.take() {
                    val = strip_optbarrier_if_const(&self.func, val);

                    let eq_val = *self.intern.params.get(&ParamKind::ImplicitUnknown(eq)).unwrap();
                    if self.func.dfg.value_dead(eq_val) || val == FALSE {
                        continue;
                    }
                    if let Some(&val) = self.val_map.get(&val) {
                        collapse_implicit.insert(val);
                        self.init.intern.outputs.insert(kind, val.into());
                    }
                }
            }
        }
        collapse_implicit
    }
}
