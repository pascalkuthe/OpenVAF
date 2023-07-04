use hir_def::db::HirDefDB;
use hir_lower::{CallBackKind, DisplayKind, FmtArg, FmtArgKind, HirInterner};
use lasso::Rodeo;
use llvm::Linkage;
use llvm::{
    IntPredicate, LLVMAddIncoming, LLVMAppendBasicBlockInContext, LLVMBuildAdd,
    LLVMBuildArrayMalloc, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildFMul,
    LLVMBuildFree, LLVMBuildICmp, LLVMBuildInBoundsGEP2, LLVMBuildLoad2, LLVMBuildPhi,
    LLVMGetParam, LLVMIsDeclaration, LLVMPositionBuilderAtEnd, LLVMSetLinkage,
    LLVMSetUnnamedAddress, UnnamedAddr, UNNAMED,
};
use mir::FuncRef;
use mir_llvm::{CallbackFun, CodegenCx, LLVMBackend, ModuleLlvm};
use salsa::InternKey;
use sim_back::matrix::MatrixEntry;
use sim_back::{CompilationDB, EvalMir, ModuleInfo, SimUnknown};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::inst_data::OsdiInstanceData;
use crate::metadata::osdi_0_3::{
    stdlib_bitcode, OsdiTys, LOG_FMT_ERR, LOG_LVL_DEBUG, LOG_LVL_DISPLAY, LOG_LVL_ERR,
    LOG_LVL_FATAL, LOG_LVL_INFO, LOG_LVL_WARN,
};
use crate::metadata::OsdiLimFunction;
use crate::model_data::OsdiModelData;
use crate::{lltype, OsdiLimId, OsdiMatrixId, OsdiNodeId};

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct OsdiMatrixEntry {
    pub row: OsdiNodeId,
    pub col: OsdiNodeId,
}

impl OsdiMatrixEntry {
    pub fn to_middle(self, node_ids: &TiSet<OsdiNodeId, SimUnknown>) -> MatrixEntry {
        MatrixEntry { row: node_ids[self.row], col: node_ids[self.col] }
    }
}

pub fn new_codegen<'a, 'll>(
    back: &'a LLVMBackend,
    llmod: &'ll ModuleLlvm,
    literals: &'a Rodeo,
) -> CodegenCx<'a, 'll> {
    let cx = unsafe { back.new_ctx(literals, llmod) };
    cx.include_bitcode(stdlib_bitcode(back.target()));

    for fun in llvm::function_iter(llmod.llmod()) {
        unsafe {
            // LLVMPurgeAttrs(fun);
            if LLVMIsDeclaration(fun) != llvm::False {
                continue;
            }

            LLVMSetLinkage(fun, Linkage::Internal);
            LLVMSetUnnamedAddress(fun, UnnamedAddr::Global);
        }
    }

    let exp_table = cx.get_declared_value("EXP").expect("constant EXP missing from stdlib");
    let char_table =
        cx.get_declared_value("FMT_CHARS").expect("constant FMT_CHARS missing from stdlib");

    unsafe {
        LLVMSetLinkage(exp_table, Linkage::Internal);
        LLVMSetLinkage(char_table, Linkage::Internal);
    }

    cx
}

pub struct OsdiCompilationUnit<'a, 'b, 'll> {
    pub inst_data: OsdiInstanceData<'ll>,
    pub model_data: OsdiModelData<'ll>,
    pub tys: &'a OsdiTys<'ll>,
    pub cx: &'a CodegenCx<'b, 'll>,
    pub module: &'a OsdiModule<'b>,
    pub lim_dispatch_table: Option<&'ll llvm::Value>,
}

impl<'a, 'b, 'll> OsdiCompilationUnit<'a, 'b, 'll> {
    pub fn new(
        module: &'a OsdiModule<'b>,
        cx: &'a CodegenCx<'b, 'll>,
        tys: &'a OsdiTys<'ll>,
        eval: bool,
    ) -> OsdiCompilationUnit<'a, 'b, 'll> {
        let inst_data = OsdiInstanceData::new(module, cx);
        let model_data = OsdiModelData::new(module, cx, &inst_data);
        let lim_dispatch_table =
            if eval && !module.lim_table.is_empty() && !module.mir.eval_intern.lim_state.is_empty()
            {
                let ty = cx.ty_array(tys.osdi_lim_function, module.lim_table.len() as u32);
                let ptr = cx
                    .define_global("OSDI_LIM_TABLE", ty)
                    .unwrap_or_else(|| unreachable!("symbol OSDI_LIM_TABLE already defined"));
                unsafe {
                    llvm::LLVMSetLinkage(ptr, llvm::Linkage::ExternalLinkage);
                    llvm::LLVMSetUnnamedAddress(ptr, llvm::UnnamedAddr::No);
                    llvm::LLVMSetDLLStorageClass(ptr, llvm::DLLStorageClass::Export);
                }
                Some(ptr)
            } else {
                None
            };
        OsdiCompilationUnit { inst_data, model_data, tys, cx, module, lim_dispatch_table }
    }

    pub fn lim_dispatch_table(&self) -> &'ll llvm::Value {
        self.lim_dispatch_table.unwrap()
    }
}

pub struct OsdiModule<'a> {
    pub base: &'a ModuleInfo,
    pub mir: &'a EvalMir,
    pub lim_table: &'a TiSet<OsdiLimId, OsdiLimFunction>,
    pub node_ids: TiSet<OsdiNodeId, SimUnknown>,
    pub matrix_ids: TiSet<OsdiMatrixId, OsdiMatrixEntry>,
    pub num_terminals: u32,
    pub sym: String,
}

impl<'a> OsdiModule<'a> {
    pub fn new(
        db: &'a CompilationDB,
        mir: &'a EvalMir,
        module: &'a ModuleInfo,
        lim_table: &'a TiSet<OsdiLimId, OsdiLimFunction>,
    ) -> Self {
        let module_data = db.module_data(module.id);
        let mut terminals: TiSet<_, _> =
            module_data.ports.iter().map(|port| SimUnknown::KirchoffLaw(*port)).collect();
        let num_terminals = terminals.len() as u32;

        let node_ids = {
            // add all internal nodes
            let internal_nodes = module_data.internal_nodes.iter().filter_map(|node| {
                if mir.pruned_nodes.contains(node) {
                    return None;
                }
                Some(SimUnknown::KirchoffLaw(*node))
            });

            terminals.raw.extend(internal_nodes);

            // add all other implicit unknowns
            let other_unknowns =
                mir.residual.resistive.raw.keys().chain(mir.residual.reactive.raw.keys()).copied();

            terminals.raw.extend(other_unknowns);

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

        OsdiModule { mir, node_ids, num_terminals, matrix_ids, base: module, sym, lim_table }
    }
}

pub fn general_callbacks<'ll>(
    intern: &HirInterner,
    builder: &mut mir_llvm::Builder<'_, '_, 'll>,
    ret_flags: &'ll llvm::Value,
    handle: &'ll llvm::Value,
    simparam: &'ll llvm::Value,
) -> TiVec<FuncRef, Option<CallbackFun<'ll>>> {
    let ptr_ty = builder.cx.ty_ptr();
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
                        &[ptr_ty, ptr_ty, ptr_ty, builder.cx.ty_ptr()],
                        builder.cx.ty_double(),
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
                        &[ptr_ty, builder.cx.ty_ptr(), builder.cx.ty_double()],
                        builder.cx.ty_double(),
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
                        &[ptr_ty, ptr_ty, ptr_ty, builder.cx.ty_ptr()],
                        builder.cx.ty_ptr(),
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
                    builder.cx.const_callback(&[builder.cx.ty_double()], zero)
                }
                CallBackKind::ParamInfo(_, _)
                | CallBackKind::CollapseHint(_, _)
                | CallBackKind::BuiltinLimit { .. }
                | CallBackKind::StoreLimit(_)
                | CallBackKind::LimDiscontinuity
                | CallBackKind::Analysis
                | CallBackKind::BoundStep
                | CallBackKind::StoreDelayTime(_) => return None,

                CallBackKind::Print { kind, arg_tys } => {
                    let (fun, fun_ty) = print_callback(builder.cx, *kind, arg_tys);
                    CallbackFun { fun_ty, fun, state: Box::new([handle]), num_state: 0 }
                }
            };
            Some(cb)
        })
        .collect()
}

fn print_callback<'ll>(
    cx: &CodegenCx<'_, 'll>,
    kind: DisplayKind,
    arg_tys: &[FmtArg],
) -> (&'ll llvm::Value, &'ll llvm::Type) {
    let mut args = vec![cx.ty_ptr(), cx.ty_ptr()];
    args.extend(arg_tys.iter().map(|arg| lltype(&arg.ty, cx)));
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
        let mut args = vec![cx.const_null_ptr(), cx.const_usize(0), LLVMGetParam(fun, 1)];

        let exp_table = cx.get_declared_value("EXP").expect("constant EXP missing from stdlib");
        let exp_table_ty = cx.ty_array(cx.ty_double(), 11);
        let char_table =
            cx.get_declared_value("FMT_CHARS").expect("constant FMT_CHARS missing from stdlib");
        let char_table_ty = cx.ty_array(cx.ty_char(), 11);
        let fmt_char_idx =
            cx.get_func_by_name("fmt_char_idx").expect("fmt_char_idx missing from stdlib");
        let fmt_char_idx_ty = cx.ty_func(&[cx.ty_double()], cx.ty_int());
        let fmt_binary = cx.get_func_by_name("fmt_binary").expect("fmt_binary missing from stdlib");
        let fmt_binary_ty = cx.ty_func(&[cx.ty_int()], cx.ty_ptr());
        let mut free = Vec::new();

        for (i, arg) in arg_tys.iter().enumerate() {
            let val = LLVMGetParam(fun, i as u32 + 2);
            match arg.kind {
                FmtArgKind::Binary => {
                    let formatted_str = LLVMBuildCall2(
                        llbuilder,
                        fmt_binary_ty,
                        fmt_binary,
                        [val].as_ptr(),
                        1,
                        UNNAMED,
                    );
                    free.push(formatted_str);
                }
                FmtArgKind::EngineerReal => {
                    let idx = LLVMBuildCall2(
                        llbuilder,
                        fmt_char_idx_ty,
                        fmt_char_idx,
                        [val].as_ptr(),
                        1,
                        UNNAMED,
                    );
                    let exp = LLVMBuildInBoundsGEP2(
                        llbuilder,
                        exp_table_ty,
                        exp_table,
                        [cx.const_int(0), idx].as_ptr(),
                        2,
                        UNNAMED,
                    );
                    let exp = LLVMBuildLoad2(llbuilder, cx.ty_double(), exp, UNNAMED);
                    let num = LLVMBuildFMul(llbuilder, val, exp, UNNAMED);
                    args.push(num);
                    let scale_char = LLVMBuildInBoundsGEP2(
                        llbuilder,
                        char_table_ty,
                        char_table,
                        [cx.const_int(0), idx].as_ptr(),
                        2,
                        UNNAMED,
                    );
                    args.push(scale_char);
                }
                FmtArgKind::Other => args.push(val),
            }
        }
        args.extend((1..(2 + arg_tys.len())).map(|arg| LLVMGetParam(fun, arg as u32)));
        let (fun_ty, fun) = cx.intrinsic("snprintf").unwrap();
        let len = LLVMBuildCall2(llbuilder, fun_ty, fun, args.as_ptr(), args.len() as u32, UNNAMED);
        let is_err = LLVMBuildICmp(llbuilder, IntPredicate::IntSLT, len, cx.const_int(0), UNNAMED);
        LLVMBuildCondBr(llbuilder, is_err, err_bb, alloc_bb);

        LLVMPositionBuilderAtEnd(llbuilder, alloc_bb);
        let data_len = LLVMBuildAdd(llbuilder, len, cx.const_int(1), UNNAMED);
        let ptr = LLVMBuildArrayMalloc(llbuilder, cx.ty_char(), data_len, UNNAMED);
        let null_ptr = cx.const_null_ptr();
        let is_err = LLVMBuildICmp(llbuilder, llvm::IntPredicate::IntEQ, null_ptr, ptr, UNNAMED);
        LLVMBuildCondBr(llbuilder, is_err, err_bb, write_bb);

        LLVMPositionBuilderAtEnd(llbuilder, write_bb);
        let data_len = LLVMBuildAdd(llbuilder, len, cx.const_int(1), UNNAMED);
        args[0] = ptr;
        args[1] = data_len;
        let len = LLVMBuildCall2(llbuilder, fun_ty, fun, args.as_ptr(), args.len() as u32, UNNAMED);
        let is_err = LLVMBuildICmp(llbuilder, IntPredicate::IntSLT, len, cx.const_int(0), UNNAMED);
        for alloc in free.iter() {
            LLVMBuildFree(llbuilder, alloc);
        }
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
        let msg = LLVMBuildPhi(llbuilder, cx.ty_ptr(), UNNAMED);
        LLVMAddIncoming(msg, [ptr, fmt_lit].as_ptr(), [write_bb, err_bb].as_ptr(), 2);
        let fun_ptr = cx.get_declared_value("osdi_log").expect("symbol osdi_log is missing");
        let fun_ty = cx.ty_func(&[cx.ty_ptr(), cx.ty_ptr(), cx.ty_int()], cx.ty_void());
        let fun = LLVMBuildLoad2(llbuilder, cx.ty_ptr(), fun_ptr, UNNAMED);
        LLVMBuildCall2(llbuilder, fun_ty, fun, [handle, msg, flags].as_ptr(), 3, UNNAMED);
        llvm::LLVMBuildRetVoid(llbuilder);
        llvm::LLVMDisposeBuilder(llbuilder);
    }

    (fun, fun_ty)
}
