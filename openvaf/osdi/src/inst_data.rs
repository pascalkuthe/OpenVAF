use ahash::RandomState;
use hir::{CompilationDB, ParamSysFun, Parameter, Variable};
use hir_lower::{HirInterner, LimitState, ParamKind, PlaceKind};
use indexmap::IndexMap;
use llvm::{
    IntPredicate, LLVMBuildFAdd, LLVMBuildFSub, LLVMBuildGEP2, LLVMBuildICmp, LLVMBuildIntCast2,
    LLVMBuildLoad2, LLVMBuildStore, LLVMBuildStructGEP2, LLVMConstInt, LLVMOffsetOfElement,
    LLVMSetFastMath, TargetData, UNNAMED,
};
use mir::{Const, Param, ValueDef, F_ZERO};
use mir_llvm::{CodegenCx, MemLoc};
use sim_back::dae::{self, MatrixEntryId, SimUnknown};
use sim_back::init::CacheSlot;
use stdx::packed_option::PackedOption;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::TiMap;

use crate::compilation_unit::{OsdiCompilationUnit, OsdiModule};
use crate::{bitfield, lltype, Offset};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum OsdiInstanceParam {
    Builtin(ParamSysFun),
    User(Parameter),
}

pub const NUM_CONST_FIELDS: u32 = 8;
pub const PARAM_GIVEN: u32 = 0;
pub const JACOBIAN_PTR_RESIST: u32 = 1;
pub const JACOBIAN_PTR_REACT: u32 = 2;
pub const NODE_MAPPING: u32 = 3;
pub const COLLAPSED: u32 = 4;
pub const TEMPERATURE: u32 = 5;
pub const CONNECTED: u32 = 6;
pub const STATE_IDX: u32 = 7;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum EvalOutput {
    Calculated(EvalOutputSlot),
    Const(Const, PackedOption<EvalOutputSlot>),
    Param(Param),
    Cache(CacheSlot),
}

impl EvalOutput {
    fn new<'ll>(
        module: &OsdiModule<'_>,
        val: mir::Value,
        eval_outputs: &mut TiMap<EvalOutputSlot, mir::Value, &'ll llvm::Type>,
        requires_slot: bool,
        ty: &'ll llvm::Type,
    ) -> EvalOutput {
        match module.eval.dfg.value_def(val) {
            ValueDef::Result(_, _) => (),
            ValueDef::Param(param) => {
                // parameters are already stored in the model anyway so no need to create a slot
                if let Some((&kind, _)) = module.intern.params.get_index(param) {
                    if matches!(
                        kind,
                        ParamKind::Param { .. }
                            | ParamKind::ParamSysFun { .. }
                            | ParamKind::Temperature { .. }
                    ) {
                        return EvalOutput::Param(param);
                    }
                } else {
                    let slot = usize::from(param) - module.intern.params.len();
                    return EvalOutput::Cache(slot.into());
                }
            }
            ValueDef::Const(const_val) => {
                let slot = requires_slot.then(|| eval_outputs.insert_full(val, ty).0);
                return EvalOutput::Const(const_val, slot.into());
            }
            ValueDef::Invalid => unreachable!(),
        }

        EvalOutput::Calculated(eval_outputs.insert_full(val, ty).0)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct EvalOutputSlot(u32);
impl_idx_from!(EvalOutputSlot(u32));
impl_debug_display! {match EvalOutputSlot{EvalOutputSlot(id) => "out{id}";}}

#[derive(Clone, Copy, Debug)]
pub struct Residual {
    pub resist: PackedOption<EvalOutputSlot>,
    pub react: PackedOption<EvalOutputSlot>,
    pub resist_lim_rhs: PackedOption<EvalOutputSlot>,
    pub react_lim_rhs: PackedOption<EvalOutputSlot>,
}

impl Residual {
    pub fn new<'ll>(
        residual: &dae::Residual,
        slots: &mut TiMap<EvalOutputSlot, mir::Value, &'ll llvm::Type>,
        ty_real: &'ll llvm::Type,
    ) -> Residual {
        let mut get_slot = |val| {
            if val == F_ZERO {
                None.into()
            } else {
                Some(slots.insert_full(val, ty_real).0).into()
            }
        };
        Residual {
            resist: get_slot(residual.resist),
            react: get_slot(residual.react),
            resist_lim_rhs: get_slot(residual.resist_lim_rhs),
            react_lim_rhs: get_slot(residual.react_lim_rhs),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct MatrixEntry {
    pub resist: Option<EvalOutput>,
    pub react: Option<EvalOutput>,
    pub react_off: PackedOption<Offset>,
}

impl MatrixEntry {
    pub fn new<'ll>(
        entry: &dae::MatrixEntry,
        module: &OsdiModule<'_>,
        slots: &mut TiMap<EvalOutputSlot, mir::Value, &'ll llvm::Type>,
        ty_real: &'ll llvm::Type,
        num_react: &mut u32,
    ) -> MatrixEntry {
        let mut get_output = |val| {
            if val == F_ZERO {
                None
            } else {
                Some(EvalOutput::new(module, val, slots, false, ty_real))
            }
        };
        let react_off = if entry.react == F_ZERO {
            None
        } else {
            *num_react += 1;
            Some(Offset(*num_react - 1))
        };
        MatrixEntry {
            resist: get_output(entry.resist),
            react: get_output(entry.react),
            react_off: react_off.into(),
        }
    }
}

pub struct OsdiInstanceData<'ll> {
    /// llvm type for the instance data struct
    pub ty: &'ll llvm::Type,

    // llvm types for static (always present) instance data struct fields
    pub param_given: &'ll llvm::Type,
    pub jacobian_ptr: &'ll llvm::Type,
    pub jacobian_ptr_react: &'ll llvm::Type,
    pub node_mapping: &'ll llvm::Type,
    pub state_idx: &'ll llvm::Type,
    pub collapsed: &'ll llvm::Type,

    // llvm types for dynamic instance data struct fields
    pub params: IndexMap<OsdiInstanceParam, &'ll llvm::Type, RandomState>,
    pub eval_outputs: TiMap<EvalOutputSlot, mir::Value, &'ll llvm::Type>,
    pub cache_slots: TiVec<CacheSlot, &'ll llvm::Type>,

    pub residual: TiVec<SimUnknown, Residual>,
    pub opvars: IndexMap<Variable, EvalOutput, RandomState>,
    pub jacobian: TiVec<MatrixEntryId, MatrixEntry>,
    pub bound_step: Option<EvalOutputSlot>,
}

impl<'ll> OsdiInstanceData<'ll> {
    pub fn new(db: &CompilationDB, module: &OsdiModule<'_>, cx: &CodegenCx<'_, 'll>) -> Self {
        let ty_f64 = cx.ty_double();
        let ty_u32 = cx.ty_int();

        let builtin_inst_params = ParamSysFun::iter().filter_map(|param| {
            let is_live = |intern: &HirInterner, func| {
                intern.is_param_live(func, &ParamKind::ParamSysFun(param))
            };
            let is_live = is_live(module.intern, module.eval)
                || is_live(&module.init.intern, &module.init.func);
            is_live.then_some((OsdiInstanceParam::Builtin(param), ty_f64))
        });
        let alias_inst_params = module
            .info
            .sys_fun_alias
            .keys()
            .map(|param| (OsdiInstanceParam::Builtin(*param), ty_f64));
        let user_inst_params = module.info.params.iter().filter_map(|(param, info)| {
            info.is_instance.then(|| (OsdiInstanceParam::User(*param), lltype(&param.ty(db), cx)))
        });
        let params: IndexMap<_, _, _> =
            builtin_inst_params.chain(alias_inst_params).chain(user_inst_params).collect();

        let mut eval_outputs = TiMap::default();
        let opvars = module
            .info
            .op_vars
            .keys()
            .map(|var| {
                let val = module.intern.outputs[&PlaceKind::Var(*var)].unwrap_unchecked();
                let ty = lltype(&var.ty(db), cx);
                let pos = EvalOutput::new(module, val, &mut eval_outputs, true, ty);
                (*var, pos)
            })
            .collect();
        let residual = module
            .dae_system
            .residual
            .iter()
            .map(|residual| Residual::new(residual, &mut eval_outputs, ty_f64))
            .collect();
        let mut num_react = 0;
        let jacobian = module
            .dae_system
            .jacobian
            .iter()
            .map(|entry| MatrixEntry::new(entry, module, &mut eval_outputs, ty_f64, &mut num_react))
            .collect();
        let bound_step = module.intern.outputs.get(&PlaceKind::BoundStep).and_then(|val| {
            let val = val.expand()?;
            let slot = eval_outputs.insert_full(val, ty_f64).0;
            Some(slot)
        });

        let param_given = bitfield::arr_ty(params.len() as u32, cx);
        let jacobian_ptr = cx.ty_array(cx.ty_ptr(), module.dae_system.jacobian.len() as u32);
        let jacobian_ptr_react = cx.ty_array(cx.ty_ptr(), num_react);
        let node_mapping = cx.ty_array(ty_u32, module.dae_system.unknowns.len() as u32);
        let collapsed = cx.ty_array(cx.ty_c_bool(), module.node_collapse.num_pairs());
        let temperature = cx.ty_double();
        let connected_ports = cx.ty_int();

        let cache_slots: TiVec<_, _> =
            module.init.cache_slots.raw.values().map(|ty| lltype(ty, cx)).collect();

        let state_idx = cx.ty_array(cx.ty_int(), module.intern.lim_state.len() as u32);
        let static_fields: [_; NUM_CONST_FIELDS as usize] = [
            param_given,
            jacobian_ptr,
            jacobian_ptr_react,
            node_mapping,
            collapsed,
            temperature,
            connected_ports,
            state_idx,
        ];

        let fields: Vec<_> = static_fields
            .into_iter()
            .chain(params.values().copied())
            .chain(cache_slots.iter().copied())
            .chain(eval_outputs.raw.values().copied())
            .collect();

        let name = &module.sym;
        let name = format!("osdi_inst_data_{name}");
        let ty = cx.ty_struct(&name, &fields);

        OsdiInstanceData {
            ty,
            param_given,
            jacobian_ptr,
            jacobian_ptr_react,
            node_mapping,
            state_idx,
            collapsed,
            params,
            eval_outputs,
            cache_slots,
            residual,
            opvars,
            jacobian,
            bound_step,
        }
    }

    pub unsafe fn store_bound_step(
        &self,
        ptr: &'ll llvm::Value,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
    ) {
        if let Some(slot) = self.bound_step {
            self.store_eval_output(slot, ptr, builder);
        }
    }

    pub fn bound_step_elem(&self) -> Option<u32> {
        let elem = self.eval_output_slot_elem(self.bound_step?);
        Some(elem)
    }

    pub unsafe fn param_ptr(
        &self,
        param: OsdiInstanceParam,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> Option<(&'ll llvm::Value, &'ll llvm::Type)> {
        let (pos, _, ty) = self.params.get_full(&param)?;
        let elem = NUM_CONST_FIELDS + pos as u32;
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED);
        Some((ptr, ty))
    }

    pub unsafe fn nth_param_ptr(
        &self,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let ty = self.params.get_index(pos as usize).unwrap().1;
        let elem = NUM_CONST_FIELDS + pos;
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED);
        (ptr, ty)
    }

    pub fn nth_param_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
    ) -> MemLoc<'ll> {
        let ty = self.params.get_index(pos as usize).unwrap().1;
        let elem = NUM_CONST_FIELDS + pos;
        MemLoc::struct_gep(ptr, self.ty, ty, elem, cx)
    }

    pub fn param_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        param: OsdiInstanceParam,
        ptr: &'ll llvm::Value,
    ) -> Option<MemLoc<'ll>> {
        let pos = self.params.get_index_of(&param)? as u32;
        let res = self.nth_param_loc(cx, pos, ptr);
        Some(res)
    }

    pub unsafe fn read_param(
        &self,
        param: OsdiInstanceParam,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> Option<&'ll llvm::Value> {
        let (ptr, ty) = self.param_ptr(param, ptr, llbuilder)?;
        let val = LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED);
        Some(val)
    }

    pub unsafe fn store_nth_param(
        &self,
        param_id: u32,
        ptr: &'ll llvm::Value,
        val: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let (ptr, _) = self.nth_param_ptr(param_id, ptr, llbuilder);
        LLVMBuildStore(llbuilder, val, ptr)
    }

    pub unsafe fn read_nth_param(
        &self,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let (ptr, ty) = self.nth_param_ptr(pos, ptr, llbuilder);
        LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED)
    }

    // pub unsafe fn opvar_ptr(
    //     &self,
    //     var: VarId,
    //     ptr: &'ll llvm::Value,
    //     llbuilder: &llvm::Builder<'ll>,
    // ) -> Option<(&'ll llvm::Value, &'ll llvm::Type)> {
    //     let (pos, _, ty) = self.opvars.get_full(&var)?;
    //     let elem = NUM_CONST_FIELDS + self.params.len() as u32 + pos as u32;
    //     let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED);
    //     Some((ptr, ty))
    // }

    pub fn residual_off(
        &self,
        node: SimUnknown,
        reactive: bool,
        target_data: &TargetData,
    ) -> Option<u32> {
        let residual = &self.residual[node];
        let slot = if reactive { &residual.react } else { &residual.resist };
        let elem = NUM_CONST_FIELDS
            + self.params.len() as u32
            + self.cache_slots.len() as u32
            + u32::from(slot.expand()?);

        let off = unsafe { LLVMOffsetOfElement(target_data, self.ty, elem) } as u32;
        Some(off)
    }

    pub fn lim_rhs_off(
        &self,
        node: SimUnknown,
        reactive: bool,
        target_data: &TargetData,
    ) -> Option<u32> {
        let residual = &self.residual[node];
        let residual = if reactive { &residual.react_lim_rhs } else { &residual.resist_lim_rhs };
        let slot = residual.expand()?;
        let elem = self.eval_output_slot_elem(slot);
        let off = unsafe { LLVMOffsetOfElement(target_data, self.ty, elem) } as u32;
        Some(off)
    }

    unsafe fn eval_output_slot_ptr(
        &self,
        llbuilder: &llvm::Builder<'ll>,
        ptr: &'ll llvm::Value,
        slot: EvalOutputSlot,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let elem = self.eval_output_slot_elem(slot);
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED);
        let ty = self.eval_outputs.get_index(slot).unwrap().1;
        (ptr, ty)
    }

    fn eval_output_slot_elem(&self, slot: EvalOutputSlot) -> u32 {
        NUM_CONST_FIELDS
            + self.params.len() as u32
            + self.cache_slots.len() as u32
            + u32::from(slot)
    }

    unsafe fn load_eval_output_slot(
        &self,
        llbuilder: &llvm::Builder<'ll>,
        ptr: &'ll llvm::Value,
        slot: EvalOutputSlot,
    ) -> &'ll llvm::Value {
        let (ptr, ty) = self.eval_output_slot_ptr(llbuilder, ptr, slot);
        LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED)
    }

    pub unsafe fn store_eval_output(
        &self,
        slot: EvalOutputSlot,
        inst_ptr: &'ll llvm::Value,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
    ) {
        let val = *self.eval_outputs.get_index(slot).unwrap().0;
        let val = builder.values[val].get(builder);
        let (ptr, _) = self.eval_output_slot_ptr(builder.llbuilder, inst_ptr, slot);
        builder.store(ptr, val)
    }

    // pub unsafe fn param_given_pointer_and_mask(
    //     &self,
    //     cx: &CodegenCx<'_, 'll>,
    //     param: OsdiInstanceParam,
    //     ptr: &'ll llvm::Value,
    //     llbuilder: &llvm::Builder<'ll>,
    // ) -> Option<(&'ll llvm::Value, &'ll llvm::Value)> {
    //     let pos = self.params.get_index_of(&param)?;
    //     Some(self.nth_param_given_pointer_and_mask(cx, pos as u32, ptr, llbuilder))
    // }

    // pub unsafe fn nth_param_given_pointer_and_mask(
    //     &self,
    //     cx: &CodegenCx<'_, 'll>,
    //     pos: u32,
    //     ptr: &'ll llvm::Value,
    //     llbuilder: &llvm::Builder<'ll>,
    // ) -> (&'ll llvm::Value, &'ll llvm::Value) {
    //     let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, PARAM_GIVEN, UNNAMED);
    //     bitfield::word_ptr_and_mask(cx, pos, arr_ptr, self.param_given, llbuilder)
    // }

    pub unsafe fn is_nth_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, PARAM_GIVEN, UNNAMED);
        bitfield::is_set(cx, pos, arr_ptr, self.param_given, llbuilder)
    }

    pub unsafe fn is_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        param: OsdiInstanceParam,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> Option<&'ll llvm::Value> {
        let pos = self.params.get_index_of(&param)?;
        let res = self.is_nth_param_given(cx, pos as u32, ptr, llbuilder);
        Some(res)
    }

    pub unsafe fn set_nth_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) {
        let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, PARAM_GIVEN, UNNAMED);
        bitfield::set_bit(cx, pos, arr_ptr, self.param_given, llbuilder)
    }

    // pub unsafe fn set_param_given(
    //     &self,
    //     cx: &CodegenCx<'_, 'll>,
    //     param: OsdiInstanceParam,
    //     ptr: &'ll llvm::Value,
    //     llbuilder: &llvm::Builder<'ll>,
    // ) -> bool {
    //     if let Some(pos) = self.params.get_index_of(&param) {
    //         self.set_nth_param_given(cx, pos as u32, ptr, llbuilder);
    //         true
    //     } else {
    //         false
    //     }
    // }

    pub unsafe fn read_node_off(
        &self,
        cx: &CodegenCx<'_, 'll>,
        node: SimUnknown,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, NODE_MAPPING, UNNAMED);
        let zero = cx.const_int(0);
        let node = cx.const_unsigned_int(node.into());
        let ptr =
            LLVMBuildGEP2(llbuilder, self.node_mapping, ptr, [zero, node].as_ptr(), 2, UNNAMED);
        LLVMBuildLoad2(llbuilder, cx.ty_int(), ptr, UNNAMED)
    }

    pub unsafe fn read_state_idx(
        &self,
        cx: &CodegenCx<'_, 'll>,
        idx: LimitState,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, STATE_IDX, UNNAMED);
        let zero = cx.const_int(0);
        let state = cx.const_unsigned_int(idx.into());
        let ptr = LLVMBuildGEP2(llbuilder, self.state_idx, ptr, [zero, state].as_ptr(), 2, UNNAMED);
        LLVMBuildLoad2(llbuilder, cx.ty_int(), ptr, UNNAMED)
    }

    pub unsafe fn read_node_voltage(
        &self,
        cx: &CodegenCx<'_, 'll>,
        node: SimUnknown,
        ptr: &'ll llvm::Value,
        prev_result: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let off = self.read_node_off(cx, node, ptr, llbuilder);
        let ptr = LLVMBuildGEP2(llbuilder, cx.ty_double(), prev_result, [off].as_ptr(), 1, UNNAMED);
        LLVMBuildLoad2(llbuilder, cx.ty_double(), ptr, UNNAMED)
    }

    pub unsafe fn read_residual(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
    ) -> Option<&'ll llvm::Value> {
        let residual = &self.residual[node];
        let residual = if reactive { &residual.react } else { &residual.resist };
        let val = self.load_eval_output_slot(llbuilder, ptr, residual.expand()?);
        Some(val)
    }

    pub unsafe fn store_lim_rhs(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm::Value,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        reactive: bool,
    ) -> bool {
        let dst = &self.residual[node];
        let slot = if reactive { dst.react_lim_rhs } else { dst.resist_lim_rhs };
        if let Some(slot) = slot.expand() {
            self.store_eval_output(slot, ptr, builder);
            true
        } else {
            false
        }
    }

    pub unsafe fn read_lim_rhs(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
    ) -> Option<&'ll llvm::Value> {
        let residual = &self.residual[node];
        let lim_rhs = if reactive { &residual.react_lim_rhs } else { &residual.resist_lim_rhs };
        let val = self.load_eval_output_slot(llbuilder, ptr, lim_rhs.expand()?);
        Some(val)
    }

    pub unsafe fn store_residual(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm::Value,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        reactive: bool,
    ) -> bool {
        let residual = &self.residual[node];
        let slot = if reactive { residual.react } else { residual.resist };
        if let Some(slot) = slot.expand() {
            self.store_eval_output(slot, ptr, builder);
            true
        } else {
            false
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub unsafe fn store_contrib(
        &self,
        cx: &CodegenCx<'_, 'll>,
        node: SimUnknown,
        ptr: &'ll llvm::Value,
        dst: &'ll llvm::Value,
        contrib: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        negate: bool,
    ) {
        let off = self.read_node_off(cx, node, ptr, llbuilder);
        let dst = LLVMBuildGEP2(llbuilder, cx.ty_double(), dst, [off].as_ptr(), 1, UNNAMED);
        let old = LLVMBuildLoad2(llbuilder, cx.ty_double(), dst, UNNAMED);
        let val = if negate {
            LLVMBuildFSub(llbuilder, old, contrib, UNNAMED)
        } else {
            LLVMBuildFAdd(llbuilder, old, contrib, UNNAMED)
        };
        LLVMSetFastMath(val);
        LLVMBuildStore(llbuilder, val, dst);
    }

    pub unsafe fn store_jacobian(
        &self,
        entry: MatrixEntryId,
        inst_ptr: &'ll llvm::Value,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        reactive: bool,
    ) {
        let entry = &self.jacobian[entry];
        let dst = if reactive { entry.react } else { entry.resist };
        if let Some(EvalOutput::Calculated(slot)) = dst {
            self.store_eval_output(slot, inst_ptr, builder)
        }
    }

    pub unsafe fn store_jacobian_contrib(
        &self,
        cx: &CodegenCx<'_, 'll>,
        entry: MatrixEntryId,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
        val: &'ll llvm::Value,
    ) {
        let field = if reactive { JACOBIAN_PTR_REACT } else { JACOBIAN_PTR_RESIST };
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, field, UNNAMED);
        let zero = cx.const_int(0);
        let entry = if reactive {
            self.jacobian[entry].react_off.unwrap_unchecked().into()
        } else {
            entry.into()
        };
        let entry = cx.const_unsigned_int(entry);
        let ty = if reactive { self.jacobian_ptr_react } else { self.jacobian_ptr };
        let ptr = LLVMBuildGEP2(llbuilder, ty, ptr, [zero, entry].as_ptr(), 2, UNNAMED);
        let dst = LLVMBuildLoad2(llbuilder, cx.ty_ptr(), ptr, UNNAMED);
        let old = LLVMBuildLoad2(llbuilder, cx.ty_double(), dst, UNNAMED);
        let val = LLVMBuildFAdd(llbuilder, old, val, UNNAMED);
        LLVMSetFastMath(val);
        LLVMBuildStore(llbuilder, val, dst);
    }

    pub fn cache_slot_elem(&self, slot: CacheSlot) -> u32 {
        NUM_CONST_FIELDS + self.params.len() as u32 + u32::from(slot)
    }

    fn cache_slot_ptr(
        &self,
        llbuilder: &llvm::Builder<'ll>,
        slot: CacheSlot,
        ptr: &'ll llvm::Value,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let elem = self.cache_slot_elem(slot);
        let ptr = unsafe { LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED) };
        let ty = self.cache_slots[slot];
        (ptr, ty)
    }

    pub unsafe fn load_cache_slot(
        &self,
        module: &OsdiModule,
        llbuilder: &llvm::Builder<'ll>,
        slot: CacheSlot,
        ptr: &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        let (ptr, ty) = self.cache_slot_ptr(llbuilder, slot, ptr);
        let mut val = LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED);

        if module.init.cache_slots[slot] == hir::Type::Bool {
            val = LLVMBuildICmp(
                llbuilder,
                IntPredicate::IntNE,
                val,
                LLVMConstInt(ty, 0, llvm::False),
                UNNAMED,
            );
        }

        val
    }

    pub unsafe fn store_cache_slot(
        &self,
        module: &OsdiModule,
        llbuilder: &llvm::Builder<'ll>,
        slot: CacheSlot,
        ptr: &'ll llvm::Value,
        mut val: &'ll llvm::Value,
    ) {
        let (ptr, ty) = self.cache_slot_ptr(llbuilder, slot, ptr);
        if module.init.cache_slots[slot] == hir::Type::Bool {
            val = LLVMBuildIntCast2(llbuilder, val, ty, llvm::False, UNNAMED);
        }
        LLVMBuildStore(llbuilder, val, ptr);
    }

    pub unsafe fn store_is_collapsible(
        &self,
        cx: &CodegenCx<'_, 'll>,
        llbuilder: &llvm::Builder<'ll>,
        ptr: &'ll llvm::Value,
        idx: &'ll llvm::Value,
    ) {
        let mut ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, COLLAPSED, UNNAMED);
        ptr = LLVMBuildGEP2(
            llbuilder,
            self.collapsed,
            ptr,
            [cx.const_unsigned_int(0), idx].as_ptr(),
            2,
            UNNAMED,
        );
        LLVMBuildStore(llbuilder, cx.const_c_bool(true), ptr);
    }

    pub unsafe fn temperature_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        ptr: &'ll llvm::Value,
    ) -> MemLoc<'ll> {
        MemLoc::struct_gep(ptr, self.ty, cx.ty_double(), TEMPERATURE, cx)
    }

    pub unsafe fn store_temperature(
        &self,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm::Value,
        val: &'ll llvm::Value,
    ) {
        let ptr = builder.struct_gep(self.ty, ptr, TEMPERATURE);
        builder.store(ptr, val)
    }

    pub unsafe fn load_connected_ports(
        &self,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        let ptr = builder.struct_gep(self.ty, ptr, CONNECTED);
        builder.load(builder.cx.ty_int(), ptr)
    }

    pub unsafe fn store_connected_ports(
        &self,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm::Value,
        val: &'ll llvm::Value,
    ) {
        let ptr = builder.struct_gep(self.ty, ptr, CONNECTED);
        builder.store(ptr, val)
    }
}

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    unsafe fn load_eval_output(
        &self,
        output: EvalOutput,
        inst_ptr: &'ll llvm::Value,
        model_ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let OsdiCompilationUnit { inst_data, model_data, cx, module, .. } = self;
        let (ptr, ty) = match output {
            EvalOutput::Calculated(slot) => {
                inst_data.eval_output_slot_ptr(llbuilder, inst_ptr, slot)
            }
            EvalOutput::Const(val, _) => {
                return cx.const_val(&val);
            }
            EvalOutput::Param(param) => {
                let intern = &module.intern;
                let (kind, _) = intern.params.get_index(param).unwrap();
                match *kind {
                    ParamKind::Param(param) => inst_data
                        .param_ptr(OsdiInstanceParam::User(param), inst_ptr, llbuilder)
                        .unwrap_or_else(|| {
                            model_data.param_ptr(param, model_ptr, llbuilder).unwrap()
                        }),
                    ParamKind::Temperature => (
                        LLVMBuildStructGEP2(
                            llbuilder,
                            cx.ty_double(),
                            inst_ptr,
                            TEMPERATURE,
                            UNNAMED,
                        ),
                        cx.ty_double(),
                    ),
                    ParamKind::ParamSysFun(func) => inst_data
                        .param_ptr(OsdiInstanceParam::Builtin(func), inst_ptr, llbuilder)
                        .unwrap(),

                    ParamKind::HiddenState(_) => todo!("hidden state"),

                    ParamKind::Voltage { .. }
                    | ParamKind::Current(_)
                    | ParamKind::PortConnected { .. }
                    | ParamKind::ParamGiven { .. }
                    | ParamKind::Abstime
                    | ParamKind::EnableIntegration
                    | ParamKind::EnableLim
                    | ParamKind::PrevState(_)
                    | ParamKind::NewState(_)
                    | ParamKind::ImplicitUnknown(_) => unreachable!(),
                }
            }
            EvalOutput::Cache(slot) => inst_data.cache_slot_ptr(llbuilder, slot, inst_ptr),
        };

        LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED)
    }

    pub unsafe fn load_jacobian_entry(
        &self,
        entry: MatrixEntryId,
        inst_ptr: &'ll llvm::Value,
        model_ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
    ) -> Option<&'ll llvm::Value> {
        let entry = &self.inst_data.jacobian[entry];
        let entry = if reactive { entry.react } else { entry.resist };
        let val = self.load_eval_output(entry?, inst_ptr, model_ptr, llbuilder);
        Some(val)
    }

    pub unsafe fn nth_opvar_ptr(
        &self,
        pos: u32,
        inst_ptr: &'ll llvm::Value,
        model_ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let OsdiCompilationUnit { inst_data, model_data, cx, module, .. } = self;
        match *inst_data.opvars.get_index(pos as usize).unwrap().1 {
            EvalOutput::Calculated(slot) => {
                inst_data.eval_output_slot_ptr(llbuilder, inst_ptr, slot)
            }
            EvalOutput::Const(val, slot) => {
                let (ptr, ty) = inst_data.eval_output_slot_ptr(llbuilder, inst_ptr, slot.unwrap());
                LLVMBuildStore(llbuilder, cx.const_val(&val), ptr);
                (ptr, ty)
            }
            EvalOutput::Param(param) => {
                let intern = &module.intern;
                let (kind, _) = intern.params.get_index(param).unwrap();
                match *kind {
                    ParamKind::Param(param) => inst_data
                        .param_ptr(OsdiInstanceParam::User(param), inst_ptr, llbuilder)
                        .unwrap_or_else(|| {
                            model_data.param_ptr(param, model_ptr, llbuilder).unwrap()
                        }),
                    ParamKind::Temperature => (
                        LLVMBuildStructGEP2(
                            llbuilder,
                            cx.ty_double(),
                            inst_ptr,
                            TEMPERATURE,
                            UNNAMED,
                        ),
                        cx.ty_double(),
                    ),
                    ParamKind::ParamSysFun(func) => inst_data
                        .param_ptr(OsdiInstanceParam::Builtin(func), inst_ptr, llbuilder)
                        .unwrap(),

                    ParamKind::HiddenState(_) => todo!("hidden state"),

                    ParamKind::Voltage { .. }
                    | ParamKind::Current(_)
                    | ParamKind::PortConnected { .. }
                    | ParamKind::ParamGiven { .. }
                    | ParamKind::EnableIntegration { .. }
                    | ParamKind::Abstime
                    | ParamKind::EnableLim
                    | ParamKind::PrevState(_)
                    | ParamKind::NewState(_)
                    | ParamKind::ImplicitUnknown(_) => unreachable!(),
                }
            }
            EvalOutput::Cache(slot) => inst_data.cache_slot_ptr(llbuilder, slot, inst_ptr),
        }
    }
}
