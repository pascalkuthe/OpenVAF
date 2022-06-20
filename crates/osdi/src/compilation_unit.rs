use hir_def::db::HirDefDB;
use hir_def::Type;
use hir_lower::{CallBackKind, DisplayKind, HirInterner};
use lasso::Rodeo;
use llvm::{
    IntPredicate, LLVMAddIncoming, LLVMAppendBasicBlockInContext, LLVMBuildAdd,
    LLVMBuildArrayMalloc, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildICmp,
    LLVMBuildPhi, LLVMGetParam, LLVMIsDeclaration, LLVMPositionBuilderAtEnd, LLVMSetLinkage,
    LLVMSetUnnamedAddress, UnnamedAddr, UNNAMED,
};
use llvm::{LLVMPurgeAttrs, Linkage};
use mir::FuncRef;
use mir_llvm::{CallbackFun, CodegenCx, LLVMBackend, ModuleLlvm};
use salsa::InternKey;
use sim_back::matrix::MatrixEntry;
use sim_back::{CompilationDB, EvalMir, ModuleInfo, SimUnkown};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::inst_data::OsdiInstanceData;
use crate::metadata::osdi_0_3::{
    OsdiTys, LOG_FMT_ERR, LOG_LVL_DEBUG, LOG_LVL_DISPLAY, LOG_LVL_ERR, LOG_LVL_FATAL, LOG_LVL_INFO,
    LOG_LVL_WARN, STDLIB_BITCODE,
};
use crate::model_data::OsdiModelData;
use crate::{lltype, OsdiMatrixId, OsdiNodeId};

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct OsdiMatrixEntry {
    pub row: OsdiNodeId,
    pub col: OsdiNodeId,
}

impl OsdiMatrixEntry {
    pub fn to_middle(self, node_ids: &TiSet<OsdiNodeId, SimUnkown>) -> MatrixEntry {
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
            LLVMPurgeAttrs(fun);
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
        let inst_data = OsdiInstanceData::new(module, cx);
        let model_data = OsdiModelData::new(module, cx, &inst_data);
        OsdiCompilationUnit { inst_data, model_data, tys, cx, module }
    }
}

pub struct OsdiModule<'a> {
    pub base: &'a ModuleInfo,
    pub mir: &'a EvalMir,
    pub node_ids: TiSet<OsdiNodeId, SimUnkown>,
    pub matrix_ids: TiSet<OsdiMatrixId, OsdiMatrixEntry>,
    pub num_terminals: u32,
    pub sym: String,
}

impl<'a> OsdiModule<'a> {
    pub fn new(db: &'a CompilationDB, mir: &'a EvalMir, module: &'a ModuleInfo) -> Self {
        let mut terminals: TiSet<_, _> = db
            .module_data(module.id)
            .ports
            .iter()
            .map(|port| SimUnkown::KirchoffLaw(*port))
            .collect();
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
                        num_state: 0,
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
                    CallbackFun {
                        fun_ty,
                        fun,
                        state: vec![simparam].into_boxed_slice(),
                        num_state: 0,
                    }
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
                        num_state: 0,
                    }
                }
                // If these derivative were non zero they would have been removed
                CallBackKind::Derivative(_) | CallBackKind::NodeDerivative(_) => {
                    let zero = builder.cx.const_real(0.0);
                    builder.cx.const_callback(&[builder.cx.ty_real()], zero)
                }
                CallBackKind::ParamInfo(_, _) | CallBackKind::CollapseHint(_, _) => return None,
                CallBackKind::Print { kind, arg_tys } => {
                    let (fun, fun_ty) = print_callback(builder.cx, *kind, arg_tys);
                    CallbackFun { fun_ty, fun, state: Box::new([handle]), num_state: 0 }
                }
                CallBackKind::BoundStep => builder.cx.trivial_callbacks(&[builder.cx.ty_real()]),
            };
            Some(cb)
        })
        .collect()
}

fn print_callback<'ll>(
    cx: &mut CodegenCx<'_, 'll>,
    kind: DisplayKind,
    arg_tys: &[Type],
) -> (&'ll llvm::Value, &'ll llvm::Type) {
    let mut args = vec![cx.ty_void_ptr(), cx.ty_str()];
    args.extend(arg_tys.iter().map(|ty| lltype(ty, cx)));
    let fun_ty = cx.ty_func(&args, cx.ty_void());
    let name = cx.local_callback_name();
    let fun = cx.declare_int_fn(&name, fun_ty);
    unsafe {
        let entry_bb = LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED);
        let alloc_bb = LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED);
        let write_bb = LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED);
        let err_bb = LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED);
        let exit_bb = LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED);
        let llbuilder = llvm::LLVMCreateBuilderInContext(cx.llcx);

        LLVMPositionBuilderAtEnd(llbuilder, entry_bb);
        let handle = LLVMGetParam(fun, 0);
        let fmt_lit = LLVMGetParam(fun, 1);
        let mut args = vec![cx.const_null_ptr(cx.ty_str()), cx.const_usize(0)];
        args.extend((1..(2 + arg_tys.len())).map(|arg| LLVMGetParam(fun, arg as u32)));
        let (fun_ty, fun) = cx.intrinsic("snprintf").unwrap();
        let len = LLVMBuildCall2(llbuilder, fun_ty, fun, args.as_ptr(), args.len() as u32, UNNAMED);
        let is_err = LLVMBuildICmp(llbuilder, IntPredicate::IntSLT, len, cx.const_int(0), UNNAMED);
        LLVMBuildCondBr(llbuilder, is_err, err_bb, alloc_bb);

        LLVMPositionBuilderAtEnd(llbuilder, alloc_bb);
        let data_len = LLVMBuildAdd(llbuilder, len, cx.const_int(1), UNNAMED);
        let ptr = LLVMBuildArrayMalloc(llbuilder, cx.ty_i8(), data_len, UNNAMED);
        let null_ptr = cx.const_null_ptr(cx.ty_str());
        let is_err = LLVMBuildICmp(llbuilder, llvm::IntPredicate::IntEQ, null_ptr, ptr, UNNAMED);
        LLVMBuildCondBr(llbuilder, is_err, err_bb, write_bb);

        LLVMPositionBuilderAtEnd(llbuilder, write_bb);
        let data_len = LLVMBuildAdd(llbuilder, len, cx.const_int(1), UNNAMED);
        args[0] = ptr;
        args[1] = data_len;
        let len = LLVMBuildCall2(llbuilder, fun_ty, fun, args.as_ptr(), args.len() as u32, UNNAMED);
        let is_err = LLVMBuildICmp(llbuilder, IntPredicate::IntSLT, len, cx.const_int(0), UNNAMED);
        LLVMBuildCondBr(llbuilder, is_err, err_bb, exit_bb);

        LLVMPositionBuilderAtEnd(llbuilder, err_bb);
        LLVMBuildBr(llbuilder, exit_bb);

        LLVMPositionBuilderAtEnd(llbuilder, exit_bb);
        let flags = LLVMBuildPhi(llbuilder, cx.ty_int(), UNNAMED);
        let lvl = match kind {
            DisplayKind::Debug => LOG_LVL_DEBUG,
            DisplayKind::Display | DisplayKind::Monitor => LOG_LVL_DISPLAY,
            DisplayKind::Info => LOG_LVL_INFO,
            DisplayKind::Warn => LOG_LVL_WARN,
            DisplayKind::Error => LOG_LVL_ERR,
            DisplayKind::Fatal => LOG_LVL_FATAL,
        };
        let lvl_and_err = lvl | LOG_FMT_ERR;
        let lvl = cx.const_unsigned_int(lvl);
        let lvl_and_err = cx.const_unsigned_int(lvl_and_err);
        LLVMAddIncoming(flags, [lvl, lvl_and_err].as_ptr(), [write_bb, err_bb].as_ptr(), 2);
        let msg = LLVMBuildPhi(llbuilder, cx.ty_str(), UNNAMED);
        LLVMAddIncoming(msg, [ptr, fmt_lit].as_ptr(), [write_bb, err_bb].as_ptr(), 2);
        let fun = cx.get_func_by_name("osdi_log").expect("callback osdi_log is missing");
        let fun_ty = cx.ty_func(&[cx.ty_void_ptr(), cx.ty_str(), cx.ty_int()], cx.ty_void());
        LLVMBuildCall2(llbuilder, fun_ty, fun, [handle, msg, flags].as_ptr(), 3, UNNAMED);
        llvm::LLVMBuildRetVoid(llbuilder);
        llvm::LLVMDisposeBuilder(llbuilder);
    }

    (fun, fun_ty)
}
