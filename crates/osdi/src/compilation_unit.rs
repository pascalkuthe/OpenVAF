use hir_def::db::HirDefDB;
use hir_def::NodeId;
use hir_lower::{CallBackKind, HirInterner};
use mir::FuncRef;
use mir_llvm::{CallbackFun, CodegenCx};
use salsa::InternKey;
use sim_back::matrix::MatrixEntry;
use sim_back::{CompilationDB, EvalMir, ModuleInfo};
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::inst_data::OsdiInstanceData;
use crate::metadata::osdi_0_3::OsdiTys;
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

pub struct OsdiCompilationUnit<'a, 'll> {
    pub db: &'a CompilationDB,
    pub module: &'a ModuleInfo,
    pub mir: &'a EvalMir,
    pub node_ids: TiSet<OsdiNodeId, NodeId>,
    pub matrix_ids: TiSet<OsdiMatrixId, OsdiMatrixEntry>,
    pub num_terminals: u32,
    pub inst_data: Option<OsdiInstanceData<'ll>>,
    pub model_data: Option<OsdiModelData<'ll>>,
    pub tys: Option<&'a OsdiTys<'ll>>,
    pub sym: String,
}

impl<'a, 'll> OsdiCompilationUnit<'a, 'll> {
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

        OsdiCompilationUnit {
            mir,
            db,
            node_ids,
            num_terminals,
            matrix_ids,
            module,
            inst_data: None,
            model_data: None,
            tys: None,
            sym,
        }
    }

    pub fn init_llvm_cx(&mut self, cx: &mut CodegenCx<'_, 'll>, tys: &'a OsdiTys<'ll>) {
        let inst_data = OsdiInstanceData::new(self, cx);
        self.inst_data = Some(inst_data);

        let model_data = OsdiModelData::new(self, cx);
        self.model_data = Some(model_data);

        self.tys = Some(tys);
    }

    pub fn inst_data(&self) -> &OsdiInstanceData<'ll> {
        self.inst_data.as_ref().unwrap()
    }

    pub fn model_data(&self) -> &OsdiModelData<'ll> {
        self.model_data.as_ref().unwrap()
    }

    pub fn tys(&self) -> &OsdiTys<'ll> {
        self.tys.as_ref().unwrap()
    }

    pub fn general_callbacks(
        &self,
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
                    CallBackKind::ParamInfo(_, _) | CallBackKind::CollapseHint(_, _) => {
                        return None
                    }
                };
                Some(cb)
            })
            .collect()
    }
}
