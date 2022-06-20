use ahash::RandomState;
use hir_def::{ParamId, ParamSysFun, Type, VarId};
use hir_lower::{ParamKind, PlaceKind};
use indexmap::IndexMap;
use llvm::{
    IntPredicate, LLVMBuildFAdd, LLVMBuildGEP2, LLVMBuildICmp, LLVMBuildIntCast2, LLVMBuildLoad2,
    LLVMBuildStore, LLVMBuildStructGEP2, LLVMConstInt, LLVMOffsetOfElement, LLVMSetFastMath,
    TargetData, UNNAMED,
};
use mir::{Const, Param, Value, ValueDef};
use mir_llvm::{CodegenCx, MemLoc};
use sim_back::CacheSlot;
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::TiVec;
use typed_indexmap::TiMap;

use crate::compilation_unit::{OsdiCompilationUnit, OsdiModule};
use crate::{bitfield, lltype, OsdiMatrixId, OsdiNodeId};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum OsdiInstanceParam {
    Builtin(ParamSysFun),
    User(ParamId),
}

pub const NUM_CONST_FIELDS: u32 = 7;
pub const PARAM_GIVEN: u32 = 0;
pub const JACOBIAN_PTR_RESIST: u32 = 1;
pub const JACOBIAN_PTR_REACT: u32 = 2;
pub const NODE_MAPPING: u32 = 3;
pub const COLLAPSED: u32 = 4;
pub const TEMPERATURE: u32 = 5;
pub const CONNECTED: u32 = 6;
// pub const MAX_STEP_SIZE: u32 = 7;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum EvalOutput {
    Calculated(EvalOutputSlot),
    Const(Const, Option<EvalOutputSlot>),
    Param(Param),
}

impl EvalOutput {
    fn new<'ll>(
        cgunit: &OsdiModule<'_>,
        val: Value,
        eval_outputs: &mut TiMap<EvalOutputSlot, Value, &'ll llvm::Type>,
        requires_slot: bool,
        ty: &'ll llvm::Type,
    ) -> EvalOutput {
        match cgunit.mir.eval_func.dfg.value_def(val) {
            ValueDef::Result(_, _) => (),
            ValueDef::Param(param) => {
                // all non-op dependent parameters are already stored in instance or model
                // ParamGiven and PortConnected are not possible here because they are bools but
                // there are not bool eval outputs
                if !matches!(
                    cgunit.mir.eval_intern.params.get_index(param),
                    Some((
                        ParamKind::Current(_)
                            | ParamKind::Voltage { .. }
                            | ParamKind::ParamGiven { .. }
                            | ParamKind::PortConnected { .. }
                            | ParamKind::ImplicitUnkown { .. }
                            | ParamKind::EnableIntegration { .. }
                            | ParamKind::Abstime,
                        _
                    ))
                ) {
                    return EvalOutput::Param(param);
                }
            }
            ValueDef::Const(const_val) => {
                let slot = requires_slot.then(|| eval_outputs.insert_full(val, ty).0);
                return EvalOutput::Const(const_val, slot);
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

pub struct OsdiInstanceData<'ll> {
    pub param_given: &'ll llvm::Type,
    pub jacobian_ptr: &'ll llvm::Type,
    pub jacobian_ptr_react: &'ll llvm::Type,
    pub node_mapping: &'ll llvm::Type,
    pub collapsed: &'ll llvm::Type,
    pub params: IndexMap<OsdiInstanceParam, &'ll llvm::Type, RandomState>,
    pub eval_outputs: TiMap<EvalOutputSlot, Value, &'ll llvm::Type>,
    pub cache_slots: TiVec<CacheSlot, &'ll llvm::Type>,
    pub jacobian_ptr_react_off: TiVec<OsdiMatrixId, Option<u32>>,

    pub residual_resist: TiVec<OsdiNodeId, Option<EvalOutputSlot>>,
    pub residual_react: TiVec<OsdiNodeId, Option<EvalOutputSlot>>,
    pub opvars: IndexMap<VarId, EvalOutput, RandomState>,
    pub matrix_resist: TiVec<OsdiMatrixId, Option<EvalOutput>>,
    pub matrix_react: TiVec<OsdiMatrixId, Option<EvalOutput>>,
    pub ty: &'ll llvm::Type,
}

impl<'ll> OsdiInstanceData<'ll> {
    pub fn new(cgunit: &OsdiModule<'_>, cx: &CodegenCx<'_, 'll>) -> Self {
        let ty_f64 = cx.ty_real();
        let ty_u32 = cx.ty_int();

        let mir = &cgunit.mir;
        let builtin_inst_params = mir.eval_intern.params.raw.iter().filter_map(|(kind, &val)| {
            if let ParamKind::ParamSysFun(param) = kind {
                if !mir.eval_func.dfg.value_dead(val) || !mir.init_inst_func.dfg.value_dead(val) {
                    return Some((OsdiInstanceParam::Builtin(*param), ty_f64));
                }
            }
            None
        });

        let user_inst_params = cgunit.base.params.iter().filter_map(|(param, info)| {
            info.is_instance.then(|| (OsdiInstanceParam::User(*param), lltype(&info.ty, cx)))
        });

        let params: IndexMap<_, _, _> = builtin_inst_params.chain(user_inst_params).collect();

        let mut eval_outputs = TiMap::default();

        let opvars = cgunit
            .base
            .op_vars
            .iter()
            .map(|(var, info)| {
                let val = cgunit.mir.eval_intern.outputs[&PlaceKind::Var(*var)].unwrap_unchecked();
                let ty = lltype(&info.ty, cx);
                let pos = EvalOutput::new(cgunit, val, &mut eval_outputs, true, ty);
                (*var, pos)
            })
            .collect();

        let ty_real = cx.ty_real();

        let matrix_resist: TiVec<_, _> = cgunit
            .matrix_ids
            .raw
            .iter()
            .map(|entry| {
                let entry = entry.to_middle(&cgunit.node_ids);
                let val = cgunit.mir.matrix.resistive.raw.get(&entry)?;
                let pos = EvalOutput::new(cgunit, *val, &mut eval_outputs, false, ty_real);
                Some(pos)
            })
            .collect();

        let matrix_react: TiVec<_, _> = cgunit
            .matrix_ids
            .raw
            .iter()
            .map(|entry| {
                let entry = entry.to_middle(&cgunit.node_ids);
                let val = cgunit.mir.matrix.reactive.raw.get(&entry)?;
                let pos = EvalOutput::new(cgunit, *val, &mut eval_outputs, false, ty_real);
                Some(pos)
            })
            .collect();

        let residual_resist: TiVec<_, _> = cgunit
            .node_ids
            .raw
            .iter()
            .map(|node| {
                let val = cgunit.mir.residual.resistive.raw.get(node)?;
                let pos = eval_outputs.insert_full(*val, ty_real).0;
                Some(pos)
            })
            .collect();

        let residual_react: TiVec<_, _> = cgunit
            .node_ids
            .raw
            .iter()
            .map(|node| {
                let val = cgunit.mir.residual.reactive.raw.get(node)?;
                let pos = eval_outputs.insert_full(*val, ty_real).0;
                Some(pos)
            })
            .collect();

        let param_given = bitfield::arr_ty(params.len() as u32, cx);
        let jacobian_ptr = cx.ty_array(cx.ptr_ty(ty_f64), cgunit.matrix_ids.len() as u32);
        let mut num_react = 0;
        let jacobian_ptr_react_off = cgunit
            .matrix_ids
            .raw
            .iter()
            .map(|entry| {
                if cgunit.mir.matrix.reactive.contains_key(&entry.to_middle(&cgunit.node_ids)) {
                    let id = num_react;
                    num_react += 1;
                    Some(id)
                } else {
                    None
                }
            })
            .collect();
        let jacobian_ptr_react = cx.ty_array(cx.ptr_ty(ty_f64), num_react);
        let node_mapping = cx.ty_array(ty_u32, cgunit.node_ids.len() as u32);
        let collapsed = cx.ty_array(cx.ty_c_bool(), cgunit.mir.collapse.len() as u32);
        let temperature = cx.ty_real();
        let connected_ports = cx.ty_int();

        let cache_slots: TiVec<_, _> =
            cgunit.mir.init_inst_cache_slots.raw.values().map(|ty| lltype(ty, cx)).collect();

        let fields: Vec<_> = [
            param_given,
            jacobian_ptr,
            jacobian_ptr_react,
            node_mapping,
            collapsed,
            temperature,
            connected_ports,
        ]
        .into_iter()
        .chain(params.values().copied())
        .chain(cache_slots.iter().copied())
        .chain(eval_outputs.raw.values().copied())
        .collect();

        let name = &cgunit.sym;
        let name = format!("osdi_inst_data_{name}");
        let ty = cx.struct_ty(&name, &fields);

        OsdiInstanceData {
            param_given,
            jacobian_ptr,
            node_mapping,
            collapsed,
            params,
            opvars,
            cache_slots,
            ty,
            eval_outputs,
            matrix_resist,
            matrix_react,
            residual_resist,
            residual_react,
            jacobian_ptr_react,
            jacobian_ptr_react_off,
        }
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
        let elem = NUM_CONST_FIELDS + pos as u32;
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
        let elem = NUM_CONST_FIELDS + pos as u32;
        let indicies = vec![cx.const_int(0), cx.const_unsigned_int(elem)].into_boxed_slice();
        MemLoc { ptr, ptr_ty: self.ty, ty, indicies }
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
        node: OsdiNodeId,
        reactive: bool,
        target_data: &TargetData,
    ) -> Option<u32> {
        let residual = if reactive { &self.residual_react } else { &self.residual_resist };
        let slot = residual[node]?;
        let elem = NUM_CONST_FIELDS
            + self.params.len() as u32
            + self.cache_slots.len() as u32
            + u32::from(slot);

        let off = unsafe { LLVMOffsetOfElement(target_data, self.ty, elem) } as u32;
        Some(off)
    }

    pub unsafe fn store_nth_opvar(
        &self,
        pos: u32,
        inst_ptr: &'ll llvm::Value,
        val: Value,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
    ) {
        if let EvalOutput::Calculated(slot) = self.opvars.get_index(pos as usize).unwrap().1 {
            let val = builder.values[val].get(builder);
            self.store_eval_output(*slot, inst_ptr, val, builder.llbuilder)
        }
    }

    unsafe fn eval_output_slot_ptr(
        &self,
        llbuilder: &llvm::Builder<'ll>,
        ptr: &'ll llvm::Value,
        slot: EvalOutputSlot,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let elem = NUM_CONST_FIELDS
            + self.params.len() as u32
            + self.cache_slots.len() as u32
            + u32::from(slot);
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED);
        let ty = self.eval_outputs.get_index(slot).unwrap().1;
        (ptr, ty)
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

    unsafe fn store_eval_output_slot(
        &self,
        llbuilder: &llvm::Builder<'ll>,
        ptr: &'ll llvm::Value,
        slot: EvalOutputSlot,
        val: &'ll llvm::Value,
    ) {
        let (ptr, _) = self.eval_output_slot_ptr(llbuilder, ptr, slot);
        LLVMBuildStore(llbuilder, val, ptr);
    }

    pub unsafe fn store_eval_output(
        &self,
        slot: EvalOutputSlot,
        inst_ptr: &'ll llvm::Value,
        val: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) {
        let (ptr, _) = self.eval_output_slot_ptr(llbuilder, inst_ptr, slot);
        LLVMBuildStore(llbuilder, val, ptr);
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
        node: OsdiNodeId,
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

    pub unsafe fn read_node_voltage(
        &self,
        cx: &CodegenCx<'_, 'll>,
        node: OsdiNodeId,
        ptr: &'ll llvm::Value,
        prev_result: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let off = self.read_node_off(cx, node, ptr, llbuilder);
        let ptr = LLVMBuildGEP2(llbuilder, cx.ty_real(), prev_result, [off].as_ptr(), 1, UNNAMED);
        LLVMBuildLoad2(llbuilder, cx.ty_real(), ptr, UNNAMED)
    }

    pub unsafe fn read_residual(
        &self,
        node: OsdiNodeId,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
    ) -> Option<&'ll llvm::Value> {
        let residual = if reactive { &self.residual_react } else { &self.residual_resist };
        let val = self.load_eval_output_slot(llbuilder, ptr, residual[node]?);
        Some(val)
    }

    pub unsafe fn store_residual(
        &self,
        node: OsdiNodeId,
        ptr: &'ll llvm::Value,
        val: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
    ) -> bool {
        let residual = if reactive { &self.residual_react } else { &self.residual_resist };
        if let Some(slot) = residual[node] {
            self.store_eval_output_slot(llbuilder, ptr, slot, val);
            true
        } else {
            false
        }
    }

    pub unsafe fn store_contrib(
        &self,
        cx: &CodegenCx<'_, 'll>,
        node: OsdiNodeId,
        ptr: &'ll llvm::Value,
        dst: &'ll llvm::Value,
        contrib: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) {
        let off = self.read_node_off(cx, node, ptr, llbuilder);
        let dst = LLVMBuildGEP2(llbuilder, cx.ty_real(), dst, [off].as_ptr(), 1, UNNAMED);
        let old = LLVMBuildLoad2(llbuilder, cx.ty_real(), dst, UNNAMED);
        let val = LLVMBuildFAdd(llbuilder, contrib, old, UNNAMED);
        LLVMSetFastMath(val);
        LLVMBuildStore(llbuilder, val, dst);
    }

    pub unsafe fn store_jacobian(
        &self,
        entry: OsdiMatrixId,
        inst_ptr: &'ll llvm::Value,
        val: Value,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        reactive: bool,
    ) {
        let matrix = if reactive { &self.matrix_react } else { &self.matrix_resist };
        if let EvalOutput::Calculated(slot) = matrix[entry].unwrap() {
            let val = builder.values[val].get(builder);
            self.store_eval_output(slot, inst_ptr, val, builder.llbuilder)
        }
    }

    pub unsafe fn store_jacobian_contrib(
        &self,
        cx: &CodegenCx<'_, 'll>,
        entry: OsdiMatrixId,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
        val: &'ll llvm::Value,
    ) {
        let field = if reactive { JACOBIAN_PTR_REACT } else { JACOBIAN_PTR_RESIST };
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, field, UNNAMED);
        let zero = cx.const_int(0);
        let entry =
            if reactive { self.jacobian_ptr_react_off[entry].unwrap() } else { entry.into() };
        let entry = cx.const_unsigned_int(entry);
        let ty = if reactive { self.jacobian_ptr_react } else { self.jacobian_ptr };
        let ptr = LLVMBuildGEP2(llbuilder, ty, ptr, [zero, entry].as_ptr(), 2, UNNAMED);
        let dst = LLVMBuildLoad2(llbuilder, cx.ptr_ty(cx.ty_real()), ptr, UNNAMED);
        let old = LLVMBuildLoad2(llbuilder, cx.ty_real(), dst, UNNAMED);
        let val = LLVMBuildFAdd(llbuilder, old, val, UNNAMED);
        LLVMSetFastMath(val);
        LLVMBuildStore(llbuilder, val, dst);
    }

    pub fn cache_slot_ptr(
        &self,
        llbuilder: &llvm::Builder<'ll>,
        slot: CacheSlot,
        ptr: &'ll llvm::Value,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let elem = NUM_CONST_FIELDS + self.params.len() as u32 + u32::from(slot);
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

        if module.mir.init_inst_cache_slots[slot] == Type::Bool {
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
        if module.mir.init_inst_cache_slots[slot] == Type::Bool {
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

    pub unsafe fn load_temperature(
        &self,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        let ptr = builder.typed_struct_gep(self.ty, ptr, TEMPERATURE);
        builder.load(builder.cx.ty_real(), ptr)
    }

    pub unsafe fn store_temperature(
        &self,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm::Value,
        val: &'ll llvm::Value,
    ) {
        let ptr = builder.typed_struct_gep(self.ty, ptr, TEMPERATURE);
        builder.store(ptr, val)
    }

    pub unsafe fn load_connected_ports(
        &self,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        let ptr = builder.typed_struct_gep(self.ty, ptr, CONNECTED);
        builder.load(builder.cx.ty_int(), ptr)
    }

    pub unsafe fn store_connected_ports(
        &self,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm::Value,
        val: &'ll llvm::Value,
    ) {
        let ptr = builder.typed_struct_gep(self.ty, ptr, CONNECTED);
        builder.store(ptr, val)
    }
}

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    unsafe fn load_eval_output(
        &mut self,
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
                let intern = &module.mir.eval_intern;
                if let Some((kind, _)) = intern.params.get_index(param) {
                    match *kind {
                        ParamKind::Param(param) => inst_data
                            .param_ptr(OsdiInstanceParam::User(param), inst_ptr, llbuilder)
                            .unwrap_or_else(|| {
                                model_data.param_ptr(param, model_ptr, llbuilder).unwrap()
                            }),
                        ParamKind::Temperature => (
                            LLVMBuildStructGEP2(
                                llbuilder,
                                cx.ty_real(),
                                inst_ptr,
                                TEMPERATURE,
                                UNNAMED,
                            ),
                            cx.ty_real(),
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
                        | ParamKind::ImplicitUnkown(_) => unreachable!(),
                    }
                } else {
                    let slot = u32::from(param) - module.mir.eval_intern.params.len() as u32;
                    inst_data.cache_slot_ptr(llbuilder, slot.into(), inst_ptr)
                }
            }
        };

        LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED)
    }

    pub unsafe fn load_jacobian_inst(
        &mut self,
        entry: OsdiMatrixId,
        inst_ptr: &'ll llvm::Value,
        model_ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
        reactive: bool,
    ) -> Option<&'ll llvm::Value> {
        let matrix =
            if reactive { &self.inst_data.matrix_react } else { &self.inst_data.matrix_resist };
        let entry = matrix[entry]?;
        let val = self.load_eval_output(entry, inst_ptr, model_ptr, llbuilder);
        Some(val)
    }

    pub unsafe fn nth_opvar_ptr(
        &mut self,
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
                let intern = &module.mir.eval_intern;
                if let Some((kind, _)) = intern.params.get_index(param) {
                    match *kind {
                        ParamKind::Param(param) => inst_data
                            .param_ptr(OsdiInstanceParam::User(param), inst_ptr, llbuilder)
                            .unwrap_or_else(|| {
                                model_data.param_ptr(param, model_ptr, llbuilder).unwrap()
                            }),
                        ParamKind::Temperature => (
                            LLVMBuildStructGEP2(
                                llbuilder,
                                cx.ty_real(),
                                inst_ptr,
                                TEMPERATURE,
                                UNNAMED,
                            ),
                            cx.ty_real(),
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
                        | ParamKind::ImplicitUnkown(_) => unreachable!(),
                    }
                } else {
                    let slot = u32::from(param) - module.mir.eval_intern.params.len() as u32;
                    inst_data.cache_slot_ptr(llbuilder, slot.into(), inst_ptr)
                }
            }
        }
    }
}
