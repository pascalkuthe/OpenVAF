use hir_def::db::HirDefDB;
use hir_def::NodeId;
use hir_lower::{CallBackKind, HirInterner};
use lasso::Rodeo;
use llvm::Linkage;
use llvm::{LLVMIsDeclaration, LLVMSetLinkage, LLVMSetUnnamedAddress, UnnamedAddr};
use mir::FuncRef;
use mir_llvm::{CallbackFun, CodegenCx, LLVMBackend, ModuleLlvm};
use salsa::InternKey;
use sim_back::matrix::MatrixEntry;
use sim_back::{CompilationDB, EvalMir, ModuleInfo};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::inst_data::OsdiInstanceData;
use crate::metadata::osdi_0_3::{OsdiTys, STDLIB_BITCODE};
use crate::model_data::OsdiModelData;
use crate::{OsdiMatrixId, OsdiNodeId};

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct OsdiMatrixEntry {
    pub row: OsdiNodeId,
    pub col: OsdiNodeId,
}

impl OsdiMatrixEntry {
    pub fn to_middle(self, node_ids: &TiSet<OsdiNodeId, NodeId>) -> MatrixEntry {
        MatrixEntry { row: node_ids[self.row], col: node_ids[self.col] }
    }
}

pub fn new_codegen<'a, 'll>(
    back: &'a LLVMBackend,
    llmod: &'ll ModuleLlvm,
    literals: &'a Rodeo,
) -> CodegenCx<'a, 'll> {
    let mut cx = unsafe { back.new_ctx(literals, llmod) };
    cx.include_bitcode(STDLIB_BITCODE);
    for fun in llvm::function_iter(llmod.llmod()) {
        unsafe {
            if LLVMIsDeclaration(fun) != llvm::False {
                continue;
            }

            LLVMSetLinkage(fun, Linkage::Internal);
            LLVMSetUnnamedAddress(fun, UnnamedAddr::Global);
        }
    }

    cx
}

pub struct OsdiCompilationUnit<'a, 'b, 'll> {
    pub inst_data: OsdiInstanceData<'ll>,
    pub model_data: OsdiModelData<'ll>,
    pub tys: &'a OsdiTys<'ll>,
    pub cx: &'a mut CodegenCx<'b, 'll>,
    pub module: &'a OsdiModule<'b>,
}

impl<'a, 'b, 'll> OsdiCompilationUnit<'a, 'b, 'll> {
    pub fn new(
        module: &'a OsdiModule<'b>,
        cx: &'a mut CodegenCx<'b, 'll>,
        tys: &'a OsdiTys<'ll>,
    ) -> OsdiCompilationUnit<'a, 'b, 'll> {
        let inst_data = OsdiInstanceData::new(module, &cx);
        let model_data = OsdiModelData::new(module, &cx, &inst_data);
        OsdiCompilationUnit { inst_data, model_data, tys, cx, module }
    }
}

pub struct OsdiModule<'a> {
    pub base: &'a ModuleInfo,
    pub mir: &'a EvalMir,
    pub node_ids: TiSet<OsdiNodeId, NodeId>,
    pub matrix_ids: TiSet<OsdiMatrixId, OsdiMatrixEntry>,
    pub num_terminals: u32,
    pub sym: String,
}

impl<'a> OsdiModule<'a> {
    pub fn new(db: &'a CompilationDB, mir: &'a EvalMir, module: &'a ModuleInfo) -> Self {
        let mut terminals: TiSet<_, _> = db.module_data(module.id).ports.iter().copied().collect();
        let num_terminals = terminals.len() as u32;

        let node_ids = {
            // add all used nodes that are not already terminals
            let node_iter =
                mir.residual.resistive.raw.keys().chain(mir.residual.reactive.raw.keys()).copied();
            terminals.raw.extend(node_iter);
            terminals
        };

        let matrix = &mir.matrix;
        let matrix_ids = node_ids
            .iter_enumerated()
            .flat_map(|(i, node1)| {
                node_ids.iter_enumerated().filter_map(move |(j, node2)| {
                    let entry = MatrixEntry { row: *node1, col: *node2 };
                    if matrix.resistive.contains_key(&entry) || matrix.reactive.contains_key(&entry)
                    {
                        Some(OsdiMatrixEntry { row: i, col: j })
                    } else {
                        None
                    }
                })
            })
            .collect();

        let sym =
            base_n::encode(u32::from(module.id.as_intern_id()) as u128, base_n::CASE_INSENSITIVE);

        OsdiModule { mir, node_ids, num_terminals, matrix_ids, base: module, sym }
    }
}

pub fn general_callbacks<'ll>(
    intern: &HirInterner,
    builder: &mut mir_llvm::Builder<'_, '_, 'll>,
    ret_flags: &'ll llvm::Value,
    handle: &'ll llvm::Value,
    simparam: &'ll llvm::Value,
) -> TiVec<FuncRef, Option<CallbackFun<'ll>>> {
    let simparam = unsafe { builder.ptrcast(simparam, builder.cx.ty_void_ptr()) };
    let ty_void_ptr = builder.cx.ty_void_ptr();
    let int_ptr_ty = builder.cx.ptr_ty(builder.cx.ty_int());
    intern
        .callbacks
        .raw
        .iter()
        .map(|call| {
            let cb = match call {
                CallBackKind::SimParam => {
                    let fun = builder
                        .cx
                        .get_func_by_name("simparam")
                        .expect("stdlib function simparam is missing");
                    let fun_ty = builder.cx.ty_func(
                        &[ty_void_ptr, ty_void_ptr, int_ptr_ty, builder.cx.ty_str()],
                        builder.cx.ty_real(),
                    );
                    CallbackFun {
                        fun_ty,
                        fun,
                        state: vec![simparam, handle, ret_flags].into_boxed_slice(),
                    }
                }
                CallBackKind::SimParamOpt => {
                    let fun = builder
                        .cx
                        .get_func_by_name("simparam_opt")
                        .expect("stdlib function simparam_opt is missing");
                    let fun_ty = builder.cx.ty_func(
                        &[ty_void_ptr, builder.cx.ty_str(), builder.cx.ty_real()],
                        builder.cx.ty_real(),
                    );
                    CallbackFun { fun_ty, fun, state: vec![simparam].into_boxed_slice() }
                }
                CallBackKind::SimParamStr => {
                    let fun = builder
                        .cx
                        .get_func_by_name("simparam_str")
                        .expect("stdlib function simparam_str is missing");
                    let fun_ty = builder.cx.ty_func(
                        &[ty_void_ptr, ty_void_ptr, int_ptr_ty, builder.cx.ty_str()],
                        builder.cx.ty_str(),
                    );
                    CallbackFun {
                        fun_ty,
                        fun,
                        state: vec![simparam, handle, ret_flags].into_boxed_slice(),
                    }
                }
                // If these derivative were non zero they would have been removed
                CallBackKind::Derivative(_) | CallBackKind::NodeDerivative(_) => {
                    let zero = builder.cx.const_real(0.0);
                    builder.cx.const_callback(&[builder.cx.ty_real()], zero)
                }
                CallBackKind::ParamInfo(_, _) | CallBackKind::CollapseHint(_, _) => return None,
            };
            Some(cb)
        })
        .collect()
}
