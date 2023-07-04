use ahash::RandomState;
use hir_def::ParamId;
use indexmap::IndexMap;
use llvm::{LLVMBuildLoad2, LLVMBuildStore, LLVMBuildStructGEP2, Value, UNNAMED};
use mir_llvm::{CodegenCx, MemLoc};

use crate::compilation_unit::OsdiModule;
use crate::inst_data::{OsdiInstanceData, OsdiInstanceParam};
use crate::{bitfield, lltype};

const NUM_CONST_FIELDS: u32 = 1;

pub struct OsdiModelData<'ll> {
    pub param_given: &'ll llvm::Type,
    pub params: IndexMap<ParamId, &'ll llvm::Type, RandomState>,
    pub ty: &'ll llvm::Type,
}

impl<'ll> OsdiModelData<'ll> {
    pub fn new(
        cgunit: &OsdiModule<'_>,
        cx: &CodegenCx<'_, 'll>,
        inst_data: &OsdiInstanceData<'ll>,
    ) -> Self {
        let inst_params = &inst_data.params;
        let params: IndexMap<_, _, _> = cgunit
            .base
            .params
            .iter()
            .filter_map(|(param, info)| {
                if inst_params.contains_key(&OsdiInstanceParam::User(*param)) {
                    None
                } else {
                    Some((*param, lltype(&info.ty, cx)))
                }
            })
            .collect();

        let param_given = bitfield::arr_ty((inst_params.len() + params.len()) as u32, cx);

        let mut fields: Vec<_> = vec![param_given];
        fields.extend(params.values().copied());
        fields.extend(inst_params.values());

        let name = &cgunit.sym;
        let name = format!("osdi_model_data_{name}");
        let ty = cx.ty_struct(&name, &fields);

        OsdiModelData { param_given, params, ty }
    }

    pub fn nth_param_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
    ) -> MemLoc<'ll> {
        let ty = self.params.get_index(pos as usize).unwrap().1;
        let elem = NUM_CONST_FIELDS + pos as u32;
        let indices =
            vec![cx.const_unsigned_int(0), cx.const_unsigned_int(elem)].into_boxed_slice();
        MemLoc { ptr, ptr_ty: self.ty, ty, indices }
    }

    pub fn param_loc(
        &self,
        cx: &CodegenCx<'_, 'll>,
        param: ParamId,
        ptr: &'ll llvm::Value,
    ) -> Option<MemLoc<'ll>> {
        let pos = self.params.get_index_of(&param)? as u32;
        let res = self.nth_param_loc(cx, pos, ptr);
        Some(res)
    }

    pub unsafe fn param_ptr(
        &self,
        param: ParamId,
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

    pub unsafe fn nth_inst_param_ptr(
        &self,
        inst_data: &OsdiInstanceData<'ll>,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> (&'ll llvm::Value, &'ll llvm::Type) {
        let ty = inst_data.params.get_index(pos as usize).unwrap().1;
        let elem = NUM_CONST_FIELDS + self.params.len() as u32 + pos as u32;
        let ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, elem, UNNAMED);
        (ptr, ty)
    }

    // pub unsafe fn read_param(
    //     &self,
    //     param: ParamId,
    //     ptr: &'ll llvm::Value,
    //     llbuilder: &llvm::Builder<'ll>,
    // ) -> Option<&'ll llvm::Value> {
    //     let (ptr, ty) = self.param_ptr(param, ptr, llbuilder)?;
    //     let val = LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED);
    //     Some(val)
    // }

    pub unsafe fn store_nth_param(
        &self,
        param: u32,
        ptr: &'ll Value,
        val: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) {
        let (ptr, _) = self.nth_param_ptr(param, ptr, llbuilder);
        LLVMBuildStore(llbuilder, val, ptr);
    }

    // pub unsafe fn read_nth_param(
    //     &self,
    //     param: u32,
    //     ptr: &'ll llvm::Value,
    //     llbuilder: &llvm::Builder<'ll>,
    // ) -> &'ll llvm::Value {
    //     let (ptr, ty) = self.nth_param_ptr(param, ptr, llbuilder);
    //     LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED)
    // }

    pub unsafe fn read_nth_inst_param(
        &self,
        inst_data: &OsdiInstanceData<'ll>,
        param: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let (ptr, ty) = self.nth_inst_param_ptr(inst_data, param, ptr, llbuilder);
        LLVMBuildLoad2(llbuilder, ty, ptr, UNNAMED)
    }

    pub unsafe fn is_nth_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, 0, UNNAMED);
        bitfield::is_set(cx, pos, arr_ptr, self.param_given, llbuilder)
    }

    pub unsafe fn is_nth_inst_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, 0, UNNAMED);
        bitfield::is_set(cx, pos + self.params.len() as u32, arr_ptr, self.param_given, llbuilder)
    }

    pub unsafe fn is_inst_param_given(
        &self,
        inst_data: &OsdiInstanceData<'ll>,
        cx: &CodegenCx<'_, 'll>,
        param: OsdiInstanceParam,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> &'ll llvm::Value {
        let pos = inst_data.params.get_index_of(&param).unwrap();
        self.is_nth_inst_param_given(cx, pos as u32, ptr, llbuilder)
    }
    pub unsafe fn is_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        param: ParamId,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) -> Option<&'ll llvm::Value> {
        let pos = self.params.get_index_of(&param)?;
        let res = self.is_nth_param_given(cx, pos as u32, ptr, llbuilder);
        Some(res)
    }

    pub unsafe fn set_nth_inst_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) {
        let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, 0, UNNAMED);
        bitfield::set_bit(cx, pos + self.params.len() as u32, arr_ptr, self.param_given, llbuilder)
    }
    pub unsafe fn set_nth_param_given(
        &self,
        cx: &CodegenCx<'_, 'll>,
        pos: u32,
        ptr: &'ll llvm::Value,
        llbuilder: &llvm::Builder<'ll>,
    ) {
        let arr_ptr = LLVMBuildStructGEP2(llbuilder, self.ty, ptr, 0, UNNAMED);
        bitfield::set_bit(cx, pos, arr_ptr, self.param_given, llbuilder)
    }

    // pub unsafe fn set_param_given(
    //     &self,
    //     cx: &CodegenCx<'_, 'll>,
    //     param: ParamId,
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
}
