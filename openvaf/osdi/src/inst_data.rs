use core::ffi::c_uint;
use core::ptr::NonNull;
use std::hash::BuildHasherDefault;

use hir::{CompilationDB, ParamSysFun, Parameter, Variable};
use hir_lower::{HirInterner, LimitState, ParamKind, PlaceKind};
use indexmap::IndexMap;
use llvm_sys::core::{
    LLVMBuildFAdd,
    LLVMBuildFSub,
    LLVMBuildGEP2,
    LLVMBuildICmp,
    LLVMBuildIntCast2,
    LLVMBuildLoad2,
    LLVMBuildStore,
    LLVMBuildStructGEP2,
    LLVMConstInt,
    //    LLVMSetFastMath
};
use llvm_sys::target::{LLVMOffsetOfElement, LLVMTargetDataRef};
use llvm_sys::LLVMIntPredicate;
use mir::{strip_optbarrier, Const, Function, Param, ValueDef, F_ZERO};
use mir_llvm::{CodegenCx, MemLoc, UNNAMED};
use rustc_hash::FxHasher;
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
    const NONE: EvalOutput = EvalOutput::Cache(CacheSlot(u32::MAX));

    fn new<'ll>(
        module: &OsdiModule<'_>,
        val: mir::Value,
        eval_outputs: &mut TiMap<EvalOutputSlot, mir::Value, &'ll llvm_sys::LLVMType>,
        requires_slot: bool,
        ty: &'ll llvm_sys::LLVMType,
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
        slots: &mut TiMap<EvalOutputSlot, mir::Value, &'ll llvm_sys::LLVMType>,
        ty_real: &'ll llvm_sys::LLVMType,
        func: &Function,
    ) -> Residual {
        let mut get_slot = |mut val| {
            val = strip_optbarrier(func, val);
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
        slots: &mut TiMap<EvalOutputSlot, mir::Value, &'ll llvm_sys::LLVMType>,
        ty_real: &'ll llvm_sys::LLVMType,
        num_react: &mut u32,
    ) -> MatrixEntry {
        let mut get_output = |mut val| {
            val = strip_optbarrier(module.eval, val);
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

#[derive(Debug)]
pub struct NoiseSource {
    pub factor: EvalOutput,
    /// content of values depend on kind of noise source
    pub args: [EvalOutput; 2],
}

impl NoiseSource {
    pub fn new<'ll>(
        source: &dae::NoiseSource,
        module: &OsdiModule<'_>,
        slots: &mut TiMap<EvalOutputSlot, mir::Value, &'ll llvm_sys::LLVMType>,
        ty_real: &'ll llvm_sys::LLVMType,
    ) -> NoiseSource {
        let mut get_output = |mut val| {
            val = strip_optbarrier(module.eval, val);
            EvalOutput::new(module, val, slots, false, ty_real)
        };
        let args = match source.kind {
            dae::NoiseSourceKind::WhiteNoise { pwr } => [get_output(pwr), EvalOutput::NONE],
            dae::NoiseSourceKind::FlickerNoise { pwr, exp } => [get_output(pwr), get_output(exp)],
            dae::NoiseSourceKind::NoiseTable { .. } => [EvalOutput::NONE; 2],
        };
        NoiseSource { args, factor: get_output(source.factor) }
    }

    pub fn eval_outputs(&self) -> [EvalOutput; 3] {
        [self.factor, self.args[0], self.args[1]]
    }
}

pub struct OsdiInstanceData<'ll> {
    /// llvm type for the instance data struct
    pub ty: &'ll llvm_sys::LLVMType,

    // llvm types for static (always present) instance data struct fields
    pub param_given: &'ll llvm_sys::LLVMType,
    pub jacobian_ptr: &'ll llvm_sys::LLVMType,
    pub jacobian_ptr_react: &'ll llvm_sys::LLVMType,
    pub node_mapping: &'ll llvm_sys::LLVMType,
    pub state_idx: &'ll llvm_sys::LLVMType,
    pub collapsed: &'ll llvm_sys::LLVMType,

    // llvm types for dynamic instance data struct fields
    pub params: IndexMap<OsdiInstanceParam, &'ll llvm_sys::LLVMType, BuildHasherDefault<FxHasher>>,
    pub eval_outputs: TiMap<EvalOutputSlot, mir::Value, &'ll llvm_sys::LLVMType>,
    pub cache_slots: TiVec<CacheSlot, &'ll llvm_sys::LLVMType>,

    pub residual: TiVec<SimUnknown, Residual>,
    pub noise: Vec<NoiseSource>,
    pub opvars: IndexMap<Variable, EvalOutput, BuildHasherDefault<FxHasher>>,
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
        let mut params = IndexMap::with_hasher(BuildHasherDefault::<FxHasher>::default());
        params.extend(builtin_inst_params.chain(alias_inst_params).chain(user_inst_params));

        let mut eval_outputs = TiMap::default();
        let mut opvars = IndexMap::with_hasher(BuildHasherDefault::<FxHasher>::default());
        opvars.extend(module.info.op_vars.keys().map(|var| {
            let val = module.intern.outputs[&PlaceKind::Var(*var)].unwrap_unchecked();
            let ty = lltype(&var.ty(db), cx);
            let pos = EvalOutput::new(module, val, &mut eval_outputs, true, ty);
            (*var, pos)
        }));
        let residual = module
            .dae_system
            .residual
            .iter()
            .map(|residual| Residual::new(residual, &mut eval_outputs, ty_f64, module.eval))
            .collect();
        let mut num_react = 0;
        let jacobian = module
            .dae_system
            .jacobian
            .iter()
            .map(|entry| MatrixEntry::new(entry, module, &mut eval_outputs, ty_f64, &mut num_react))
            .collect();
        let noise = module
            .dae_system
            .noise_sources
            .iter()
            .map(|source| NoiseSource::new(source, module, &mut eval_outputs, ty_f64))
            .collect();
        let bound_step = module.intern.outputs.get(&PlaceKind::BoundStep).and_then(|val| {
            let mut val = val.expand()?;
            val = strip_optbarrier(module.eval, val);
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
            noise,
            opvars,
            jacobian,
            bound_step,
        }
    }

    pub unsafe fn store_bound_step(
        &self,
        ptr: &'ll llvm_sys::LLVMValue,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
    ) {
        if let Some(slot) = self.bound_step {
            self.store_eval_output_slot(slot, ptr, builder);
        }
    }

    pub fn bound_step_elem(&self) -> Option<u32> {
        let elem = self.eval_output_slot_elem(self.bound_step?);
        Some(elem)
    }

    pub unsafe fn param_ptr(
        &self,
        param: OsdiInstanceParam,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> Option<(&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMType)> {
        let (pos, _, ty) = self.params.get_full(&param)?;
        let elem = NUM_CONST_FIELDS + pos as u32;
        let ptr = &*LLVMBuildStructGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(self.ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            elem,
            UNNAMED,
        );
        Some((ptr, ty))
    }

    pub unsafe fn nth_param_ptr(
        &self,
        pos: u32,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> (&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMType) {
        let ty = self.params.get_index(pos as usize).unwrap().1;
        let elem = NUM_CONST_FIELDS + pos;
        let ptr = &*LLVMBuildStructGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(self.ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            elem,
            UNNAMED,
        );
        (ptr, ty)
    }

    pub fn nth_param_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm_sys::LLVMValue,
    ) -> MemLoc<'ll> {
        let ty = self.params.get_index(pos as usize).unwrap().1;
        let elem = NUM_CONST_FIELDS + pos;
        MemLoc::struct_gep(ptr, self.ty, ty, elem, cx)
    }

    pub fn param_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        param: OsdiInstanceParam,
        ptr: &'ll llvm_sys::LLVMValue,
    ) -> Option<MemLoc<'ll>> {
        let pos = self.params.get_index_of(&param)? as u32;
        let res = self.nth_param_loc(cx, pos, ptr);
        Some(res)
    }

    pub unsafe fn read_param(
        &self,
        param: OsdiInstanceParam,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> Option<&'ll llvm_sys::LLVMValue> {
        let (ptr, ty) = self.param_ptr(param, ptr, llbuilder)?;
        let val = &*LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            UNNAMED,
        );
        Some(val)
    }

    pub unsafe fn store_nth_param(
        &self,
        param_id: u32,
        ptr: &'ll llvm_sys::LLVMValue,
        val: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> &'ll llvm_sys::LLVMValue {
        let (ptr, _) = self.nth_param_ptr(param_id, ptr, llbuilder);
        &*LLVMBuildStore(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(val).as_ptr(),
            NonNull::from(ptr).as_ptr(),
        )
    }

    pub unsafe fn read_nth_param(
        &self,
        pos: u32,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> &'ll llvm_sys::LLVMValue {
        let (ptr, ty) = self.nth_param_ptr(pos, ptr, llbuilder);
        &*LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            UNNAMED,
        )
    }

    // pub unsafe fn opvar_ptr(
    //     &self,
    //     var: VarId,
    //     ptr: &'ll llvm_sys::LLVMValue,
    //     llbuilder: &llvm_sys::LLVMBuilder,
    // ) -> Option<(&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMType)> {
    //     let (pos, _, ty) = self.opvars.get_full(&var)?;
    //     let elem = NUM_CONST_FIELDS + self.params.len() as u32 + pos as u32;
    //     let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED);
    //     Some((ptr, ty))
    // }

    pub fn residual_off(
        &self,
        node: SimUnknown,
        reactive: bool,
        target_data: &LLVMTargetDataRef,
    ) -> Option<u32> {
        let residual = &self.residual[node];
        let slot = if reactive { &residual.react } else { &residual.resist };
        let elem = NUM_CONST_FIELDS
            + self.params.len() as u32
            + self.cache_slots.len() as u32
            + u32::from(slot.expand()?);

        let off =
            unsafe { LLVMOffsetOfElement(*target_data, NonNull::from(self.ty).as_ptr(), elem) }
                as u32;
        Some(off)
    }

    pub fn lim_rhs_off(
        &self,
        node: SimUnknown,
        reactive: bool,
        target_data: &LLVMTargetDataRef,
    ) -> Option<u32> {
        let residual = &self.residual[node];
        let residual = if reactive { &residual.react_lim_rhs } else { &residual.resist_lim_rhs };
        let slot = residual.expand()?;
        let elem = self.eval_output_slot_elem(slot);
        let off =
            unsafe { LLVMOffsetOfElement(*target_data, NonNull::from(self.ty).as_ptr(), elem) }
                as u32;
        Some(off)
    }

    unsafe fn eval_output_slot_ptr(
        &self,
        llbuilder: &llvm_sys::LLVMBuilder,
        ptr: &'ll llvm_sys::LLVMValue,
        slot: EvalOutputSlot,
    ) -> (&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMType) {
        let elem = self.eval_output_slot_elem(slot);
        let ptr = &*LLVMBuildStructGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(self.ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            elem,
            UNNAMED,
        );
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
        llbuilder: &llvm_sys::LLVMBuilder,
        ptr: &'ll llvm_sys::LLVMValue,
        slot: EvalOutputSlot,
    ) -> &'ll llvm_sys::LLVMValue {
        let (ptr, ty) = self.eval_output_slot_ptr(llbuilder, ptr, slot);
        &*LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            UNNAMED,
        )
    }

    pub unsafe fn store_eval_output_slot(
        &self,
        slot: EvalOutputSlot,
        inst_ptr: &'ll llvm_sys::LLVMValue,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
    ) {
        let val = *self.eval_outputs.get_index(slot).unwrap().0;
        let val = builder.values[val].get(builder);
        let (ptr, _) = self.eval_output_slot_ptr(builder.llbuilder, inst_ptr, slot);
        builder.store(ptr, val)
    }

    pub unsafe fn store_eval_output(
        &self,
        output: EvalOutput,
        inst_ptr: &'ll llvm_sys::LLVMValue,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
    ) {
        if let EvalOutput::Calculated(slot) = output {
            self.store_eval_output_slot(slot, inst_ptr, builder)
        }
    }

    // pub unsafe fn param_given_pointer_and_mask(
    //     &self,
    //     cx: &CodegenCx<'_, 'll>,
    //     param: OsdiInstanceParam,
    //     ptr: &'ll llvm_sys::LLVMValue,
    //     llbuilder: &llvm_sys::LLVMBuilder,
    // ) -> Option<(&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMValue)> {
    //     let pos = self.params.get_index_of(&param)?;
    //     Some(self.nth_param_given_pointer_and_mask(cx, pos as u32, ptr, llbuilder))
    // }

    // pub unsafe fn nth_param_given_pointer_and_mask(
    //     &self,
    //     cx: &CodegenCx<'_, 'll>,
    //     pos: u32,
    //     ptr: &'ll llvm_sys::LLVMValue,
    //     llbuilder: &llvm_sys::LLVMBuilder,
    // ) -> (&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMValue) {
    //     let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, PARAM_GIVEN, UNNAMED);
    //     bitfield::word_ptr_and_mask(cx, pos, arr_ptr, self.param_given, llbuilder)
    // }

    pub unsafe fn is_nth_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> &'ll llvm_sys::LLVMValue {
        let arr_ptr = &*LLVMBuildStructGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(self.ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            PARAM_GIVEN,
            UNNAMED,
        );
        bitfield::is_set(cx, pos, arr_ptr, self.param_given, llbuilder)
    }

    pub unsafe fn is_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        param: OsdiInstanceParam,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> Option<&'ll llvm_sys::LLVMValue> {
        let pos = self.params.get_index_of(&param)?;
        let res = self.is_nth_param_given(cx, pos as u32, ptr, llbuilder);
        Some(res)
    }

    pub unsafe fn set_nth_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) {
        let arr_ptr = &*LLVMBuildStructGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(self.ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            PARAM_GIVEN,
            UNNAMED,
        );
        bitfield::set_bit(cx, pos, arr_ptr, self.param_given, llbuilder)
    }

    // pub unsafe fn set_param_given(
    //     &self,
    //     cx: &CodegenCx<'_, 'll>,
    //     param: OsdiInstanceParam,
    //     ptr: &'ll llvm_sys::LLVMValue,
    //     llbuilder: &llvm_sys::LLVMBuilder,
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
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> &'ll llvm_sys::LLVMValue {
        let builder_ptr = NonNull::from(llbuilder).as_ptr();
        let ty_ptr = NonNull::from(self.ty).as_ptr();
        let ptr_value = NonNull::from(ptr).as_ptr();

        // First GEP for accessing the NODE_MAPPING field
        let ptr = LLVMBuildStructGEP2(builder_ptr, ty_ptr, ptr_value, NODE_MAPPING, UNNAMED);

        // Preparing indices for the next GEP
        let zero = cx.const_int(0) as *const llvm_sys::LLVMValue as *mut _;
        let node_val = cx.const_unsigned_int(node.into()) as *const llvm_sys::LLVMValue as *mut _;
        let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 2] = [zero, node_val];
        let gep_ptr = gep_indices.as_mut_ptr();

        // Apply GEP2 for node mapping
        let ptr = LLVMBuildGEP2(
            builder_ptr,
            NonNull::from(self.node_mapping).as_ptr(),
            ptr,
            gep_ptr,
            2,
            UNNAMED,
        );

        // Load the integer value from the final pointer
        &*LLVMBuildLoad2(builder_ptr, NonNull::from(cx.ty_int()).as_ptr(), ptr, UNNAMED)
    }

    pub unsafe fn read_state_idx(
        &self,
        cx: &CodegenCx<'_, 'll>,
        idx: LimitState,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> &'ll llvm_sys::LLVMValue {
        let builder_ptr = NonNull::from(llbuilder).as_ptr();
        let ty_ptr = NonNull::from(self.ty).as_ptr();
        let ptr_value = NonNull::from(ptr).as_ptr();

        // First GEP for accessing the STATE_IDX field
        let ptr = LLVMBuildStructGEP2(builder_ptr, ty_ptr, ptr_value, STATE_IDX, UNNAMED);

        // Preparing indices for the next GEP
        let zero = cx.const_int(0) as *const llvm_sys::LLVMValue as *mut _;
        let state = cx.const_unsigned_int(idx.into()) as *const llvm_sys::LLVMValue as *mut _;
        let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 2] = [zero, state];
        let gep_ptr = gep_indices.as_mut_ptr();

        // Apply GEP2 for state index
        let ptr = LLVMBuildGEP2(
            builder_ptr,
            NonNull::from(self.state_idx).as_ptr(),
            ptr,
            gep_ptr,
            2,
            UNNAMED,
        );

        // Load the integer value from the final pointer
        &*LLVMBuildLoad2(builder_ptr, NonNull::from(cx.ty_int()).as_ptr(), ptr, UNNAMED)
    }

    pub unsafe fn read_node_voltage(
        &self,
        cx: &CodegenCx<'_, 'll>,
        node: SimUnknown,
        ptr: &'ll llvm_sys::LLVMValue,
        prev_result: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> &'ll llvm_sys::LLVMValue {
        let off = self.read_node_off(cx, node, ptr, llbuilder);
        let off_val = off as *const llvm_sys::LLVMValue as *mut _;
        let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 1] = [off_val];
        let gep_ptr = gep_indices.as_mut_ptr();

        let ptr = LLVMBuildGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(cx.ty_double()).as_ptr(),
            NonNull::from(prev_result).as_ptr(),
            gep_ptr,
            1,
            UNNAMED,
        );
        &*LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(cx.ty_double()).as_ptr(),
            ptr,
            UNNAMED,
        )
    }

    pub unsafe fn read_residual(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
        reactive: bool,
    ) -> Option<&'ll llvm_sys::LLVMValue> {
        let residual = &self.residual[node];
        let residual = if reactive { &residual.react } else { &residual.resist };
        let val = self.load_eval_output_slot(llbuilder, ptr, residual.expand()?);
        Some(val)
    }

    pub unsafe fn store_lim_rhs(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm_sys::LLVMValue,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        reactive: bool,
    ) -> bool {
        let dst = &self.residual[node];
        let slot = if reactive { dst.react_lim_rhs } else { dst.resist_lim_rhs };
        if let Some(slot) = slot.expand() {
            self.store_eval_output_slot(slot, ptr, builder);
            true
        } else {
            false
        }
    }

    pub unsafe fn read_lim_rhs(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
        reactive: bool,
    ) -> Option<&'ll llvm_sys::LLVMValue> {
        let residual = &self.residual[node];
        let lim_rhs = if reactive { &residual.react_lim_rhs } else { &residual.resist_lim_rhs };
        let val = self.load_eval_output_slot(llbuilder, ptr, lim_rhs.expand()?);
        Some(val)
    }

    pub unsafe fn store_residual(
        &self,
        node: SimUnknown,
        ptr: &'ll llvm_sys::LLVMValue,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        reactive: bool,
    ) -> bool {
        let residual = &self.residual[node];
        let slot = if reactive { residual.react } else { residual.resist };
        if let Some(slot) = slot.expand() {
            self.store_eval_output_slot(slot, ptr, builder);
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
        ptr: &'ll llvm_sys::LLVMValue,
        dst: &'ll llvm_sys::LLVMValue,
        contrib: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
        negate: bool,
    ) {
        let off = self.read_node_off(cx, node, ptr, llbuilder);
        let off_val = off as *const llvm_sys::LLVMValue as *mut _;
        let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 1] = [off_val];
        let gep_ptr = gep_indices.as_mut_ptr();

        let dst = LLVMBuildGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(cx.ty_double()).as_ptr(),
            NonNull::from(dst).as_ptr(),
            gep_ptr,
            1,
            UNNAMED,
        );
        let old = LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(cx.ty_double()).as_ptr(),
            dst,
            UNNAMED,
        );
        let val = if negate {
            LLVMBuildFSub(
                NonNull::from(llbuilder).as_ptr(),
                old,
                NonNull::from(contrib).as_ptr(),
                UNNAMED,
            )
        } else {
            LLVMBuildFAdd(
                NonNull::from(llbuilder).as_ptr(),
                old,
                NonNull::from(contrib).as_ptr(),
                UNNAMED,
            )
        };

        let fast_math_flags: c_uint = 0x1F; // This represents all flags set
        llvm_sys::core::LLVMSetFastMathFlags(val, fast_math_flags);
        //LLVMSetFastMath(val);
        LLVMBuildStore(NonNull::from(llbuilder).as_ptr(), val, dst);
    }

    pub unsafe fn store_jacobian(
        &self,
        entry: MatrixEntryId,
        inst_ptr: &'ll llvm_sys::LLVMValue,
        builder: &mir_llvm::Builder<'_, '_, 'll>,
        reactive: bool,
    ) {
        let entry = &self.jacobian[entry];
        let dst = if reactive { entry.react } else { entry.resist };
        if let Some(EvalOutput::Calculated(slot)) = dst {
            self.store_eval_output_slot(slot, inst_ptr, builder)
        }
    }

    // Adds Jacobian contribution to destination
    pub unsafe fn store_jacobian_contrib(
        &self,
        cx: &CodegenCx<'_, 'll>,
        entry: MatrixEntryId,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
        reactive: bool,
        has_offset: bool,
        offset: &'ll llvm_sys::LLVMValue,
        val: &'ll llvm_sys::LLVMValue,
    ) {
        // Field number within instance structure
        let field = if reactive { JACOBIAN_PTR_REACT } else { JACOBIAN_PTR_RESIST };

        // Get pointer to array of double* pointers
        let ptr = LLVMBuildStructGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(self.ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            field,
            UNNAMED,
        );
        let zero = cx.const_int(0);
        // Get entry index
        let entry = if reactive {
            // For reactive Jacobian get index of reactive entry
            self.jacobian[entry].react_off.unwrap_unchecked().into()
        } else {
            // For resistive Jacobian the u32 value within MatrixEntryId is the index
            entry.into()
        };
        // Convert to LLVM u32
        let entry = cx.const_unsigned_int(entry);
        // Prepare type of Jacobian entry pointers array
        let ty = if reactive { self.jacobian_ptr_react } else { self.jacobian_ptr };
        // Create pointer to array entry with index entry
        let zero_val = zero as *const llvm_sys::LLVMValue as *mut _;
        let entry_val = entry as *const llvm_sys::LLVMValue as *mut _;
        let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 2] = [zero_val, entry_val];
        let gep_ptr = gep_indices.as_mut_ptr();

        let ptr = LLVMBuildGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(ty).as_ptr(),
            ptr,
            gep_ptr,
            2,
            UNNAMED,
        );
        // Load value from destination pointed to by ptr (get pointer to Jacobian entry destination)
        let mut dst = LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(cx.ty_ptr()).as_ptr(),
            ptr,
            UNNAMED,
        );
        // Add offset to destination
        if has_offset {
            let offset_val = offset as *const llvm_sys::LLVMValue as *mut _;
            let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 1] = [offset_val];
            let gep_ptr = gep_indices.as_mut_ptr();

            dst = LLVMBuildGEP2(
                NonNull::from(llbuilder).as_ptr(),
                NonNull::from(cx.ty_double()).as_ptr(),
                dst,
                gep_ptr,
                1,
                UNNAMED,
            );
        }
        // Load value from where the Jacobian entry should be added (pointed to by dst)
        let old = LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(cx.ty_double()).as_ptr(),
            dst,
            UNNAMED,
        );
        // Add value to old
        let val = LLVMBuildFAdd(
            NonNull::from(llbuilder).as_ptr(),
            old,
            NonNull::from(val).as_ptr(),
            UNNAMED,
        );
        // Set fast math flags on result
        let fast_math_flags: c_uint = 0x1F; // This represents all flags set
        llvm_sys::core::LLVMSetFastMathFlags(val, fast_math_flags);
        // Store value where dst pointer points to
        LLVMBuildStore(NonNull::from(llbuilder).as_ptr(), val, dst);
    }

    // Writes Jacobian contribution to corresponding slot in array of doubles
    pub unsafe fn write_jacobian_contrib(
        &self,
        cx: &CodegenCx<'_, 'll>,
        entry: u32,
        ty: &'ll llvm_sys::LLVMType,
        ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
        val: &'ll llvm_sys::LLVMValue,
    ) {
        let zero = cx.const_int(0);

        // Convert to LLVM u32
        let entry = cx.const_unsigned_int(entry);
        // Create pointer to array entry with index entry
        let zero_val = zero as *const llvm_sys::LLVMValue as *mut _;
        let entry_val = entry as *const llvm_sys::LLVMValue as *mut _;
        let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 2] = [zero_val, entry_val];
        let gep_ptr = gep_indices.as_mut_ptr();

        let ptr = LLVMBuildGEP2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            gep_ptr,
            2,
            UNNAMED,
        );

        // Store value where dst pointer points to
        LLVMBuildStore(NonNull::from(llbuilder).as_ptr(), NonNull::from(val).as_ptr(), ptr);
    }

    pub fn cache_slot_elem(&self, slot: CacheSlot) -> u32 {
        NUM_CONST_FIELDS + self.params.len() as u32 + u32::from(slot)
    }

    fn cache_slot_ptr(
        &self,
        llbuilder: &llvm_sys::LLVMBuilder,
        slot: CacheSlot,
        ptr: &'ll llvm_sys::LLVMValue,
    ) -> (&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMType) {
        let elem = self.cache_slot_elem(slot);
        let ptr = unsafe {
            &*LLVMBuildStructGEP2(
                NonNull::from(llbuilder).as_ptr(),
                NonNull::from(self.ty).as_ptr(),
                NonNull::from(ptr).as_ptr(),
                elem,
                UNNAMED,
            )
        };
        let ty = self.cache_slots[slot];
        (ptr, ty)
    }

    pub unsafe fn load_cache_slot(
        &self,
        module: &OsdiModule,
        llbuilder: &llvm_sys::LLVMBuilder,
        slot: CacheSlot,
        ptr: &'ll llvm_sys::LLVMValue,
    ) -> &'ll llvm_sys::LLVMValue {
        let (ptr, ty) = self.cache_slot_ptr(llbuilder, slot, ptr);
        let mut val = &*LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            UNNAMED,
        );

        if module.init.cache_slots[slot] == hir::Type::Bool {
            val = &*LLVMBuildICmp(
                NonNull::from(llbuilder).as_ptr(),
                LLVMIntPredicate::LLVMIntNE,
                NonNull::from(val).as_ptr(),
                LLVMConstInt(NonNull::from(ty).as_ptr(), 0, 0),
                UNNAMED,
            );
        }

        val
    }

    pub unsafe fn store_cache_slot(
        &self,
        module: &OsdiModule,
        llbuilder: &llvm_sys::LLVMBuilder,
        slot: CacheSlot,
        ptr: &'ll llvm_sys::LLVMValue,
        mut val: &'ll llvm_sys::LLVMValue,
    ) {
        let (ptr, ty) = self.cache_slot_ptr(llbuilder, slot, ptr);
        if module.init.cache_slots[slot] == hir::Type::Bool {
            val = &*LLVMBuildIntCast2(
                NonNull::from(llbuilder).as_ptr(),
                NonNull::from(val).as_ptr(),
                NonNull::from(ty).as_ptr(),
                0,
                UNNAMED,
            );
        }
        LLVMBuildStore(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(val).as_ptr(),
            NonNull::from(ptr).as_ptr(),
        );
    }
    pub unsafe fn store_is_collapsible(
        &self,
        cx: &CodegenCx<'_, 'll>,
        llbuilder: &llvm_sys::LLVMBuilder,
        ptr: &'ll llvm_sys::LLVMValue,
        idx: &'ll llvm_sys::LLVMValue,
    ) {
        let builder_ptr = NonNull::from(llbuilder).as_ptr();
        let ty_ptr = NonNull::from(self.ty).as_ptr();

        // Create GEP for the struct field
        let mut ptr = LLVMBuildStructGEP2(
            builder_ptr,
            ty_ptr,
            NonNull::from(ptr).as_ptr(),
            COLLAPSED,
            UNNAMED,
        );

        // Stable storage for GEP indices
        let idx_0: llvm_sys::prelude::LLVMValueRef =
            cx.const_unsigned_int(0) as *const llvm_sys::LLVMValue as *mut _;
        let idx_1: llvm_sys::prelude::LLVMValueRef = idx as *const llvm_sys::LLVMValue as *mut _;

        // Pointers array for GEP indices
        let mut gep_indices: [llvm_sys::prelude::LLVMValueRef; 2] = [idx_0, idx_1];
        let gep_ptr = gep_indices.as_mut_ptr();

        // Apply GEP2 for collapsed field
        ptr = LLVMBuildGEP2(
            builder_ptr,
            NonNull::from(self.collapsed).as_ptr(),
            ptr,
            gep_ptr,
            2,
            UNNAMED,
        );

        // Final store operation
        LLVMBuildStore(builder_ptr, NonNull::from(cx.const_c_bool(true)).as_ptr(), ptr);
    }

    pub unsafe fn temperature_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        ptr: &'ll llvm_sys::LLVMValue,
    ) -> MemLoc<'ll> {
        MemLoc::struct_gep(ptr, self.ty, cx.ty_double(), TEMPERATURE, cx)
    }

    pub unsafe fn store_temperature(
        &self,
        builder: &mut mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm_sys::LLVMValue,
        val: &'ll llvm_sys::LLVMValue,
    ) {
        let ptr = builder.struct_gep(self.ty, ptr, TEMPERATURE);
        builder.store(ptr, val)
    }

    pub unsafe fn load_connected_ports(
        &self,
        builder: &mut mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm_sys::LLVMValue,
    ) -> &'ll llvm_sys::LLVMValue {
        let ptr = builder.struct_gep(self.ty, ptr, CONNECTED);
        builder.load(builder.cx.ty_int(), ptr)
    }

    pub unsafe fn store_connected_ports(
        &self,
        builder: &mut mir_llvm::Builder<'_, '_, 'll>,
        ptr: &'ll llvm_sys::LLVMValue,
        val: &'ll llvm_sys::LLVMValue,
    ) {
        /*let builder_mut: &mut _ = unsafe {
            &mut *(builder as *const _ as *mut mir_llvm::Builder<'_, '_, 'll>)
        };*/
        let ptr = builder.struct_gep(self.ty, ptr, CONNECTED);
        builder.store(ptr, val)
    }
}

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    pub unsafe fn load_eval_output(
        &self,
        output: EvalOutput,
        inst_ptr: &'ll llvm_sys::LLVMValue,
        model_ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> &'ll llvm_sys::LLVMValue {
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
                        &*LLVMBuildStructGEP2(
                            NonNull::from(llbuilder).as_ptr(),
                            NonNull::from(cx.ty_double()).as_ptr(),
                            NonNull::from(inst_ptr).as_ptr(),
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

        &*LLVMBuildLoad2(
            NonNull::from(llbuilder).as_ptr(),
            NonNull::from(ty).as_ptr(),
            NonNull::from(ptr).as_ptr(),
            UNNAMED,
        )
    }

    pub unsafe fn load_jacobian_entry(
        &self,
        entry: MatrixEntryId,
        inst_ptr: &'ll llvm_sys::LLVMValue,
        model_ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
        reactive: bool,
    ) -> Option<&'ll llvm_sys::LLVMValue> {
        let entry = &self.inst_data.jacobian[entry];
        let entry = if reactive { entry.react } else { entry.resist };
        let val = self.load_eval_output(entry?, inst_ptr, model_ptr, llbuilder);
        Some(val)
    }

    pub unsafe fn nth_opvar_ptr(
        &self,
        pos: u32,
        inst_ptr: &'ll llvm_sys::LLVMValue,
        model_ptr: &'ll llvm_sys::LLVMValue,
        llbuilder: &llvm_sys::LLVMBuilder,
    ) -> (&'ll llvm_sys::LLVMValue, &'ll llvm_sys::LLVMType) {
        let OsdiCompilationUnit { inst_data, model_data, cx, module, .. } = self;
        match *inst_data.opvars.get_index(pos as usize).unwrap().1 {
            EvalOutput::Calculated(slot) => {
                inst_data.eval_output_slot_ptr(llbuilder, inst_ptr, slot)
            }
            EvalOutput::Const(val, slot) => {
                let (ptr, ty) = inst_data.eval_output_slot_ptr(llbuilder, inst_ptr, slot.unwrap());
                LLVMBuildStore(
                    NonNull::from(llbuilder).as_ptr(),
                    NonNull::from(cx.const_val(&val)).as_ptr(),
                    NonNull::from(ptr).as_ptr(),
                );
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
                        &*LLVMBuildStructGEP2(
                            NonNull::from(llbuilder).as_ptr(),
                            NonNull::from(cx.ty_double()).as_ptr(),
                            NonNull::from(inst_ptr).as_ptr(),
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
