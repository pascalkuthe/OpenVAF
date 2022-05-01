use hir_def::{Lookup, Type};
use hir_lower::ParamKind;
use lasso::Rodeo;
use llvm::Linkage::InternalLinkage;
use llvm::{
    function_iter, LLVMDisposeTargetData, LLVMIsDeclaration, LLVMSetLinkage, LLVMSetUnnamedAddress,
    UnnamedAddr,
};
use mir_llvm::{CodegenCx, LLVMBackend};
use sim_back::{CompilationDB, EvalMir};
use stdx::iter::zip;
use stdx::{impl_debug_display, impl_idx_from};
use target::spec::Target;

use std::ffi::CString;
use std::path::Path;

use crate::compilation_unit::OsdiCompilationUnit;
use crate::metadata::osdi_0_3::{OsdiTys, STDLIB_BITCODE};

mod access;
mod bitfield;
mod compilation_unit;
mod inst_data;
mod metadata;
mod model_data;

mod eval;
mod load;
mod setup;
#[cfg(test)]
mod tests;

const OSDI_VERSION: (u32, u32) = (0, 3);

pub fn compile(path: &Path, dst: &Path, target: &Target, back: &LLVMBackend) {
    let db = CompilationDB::new(path).unwrap();
    let modules = db.collect_modules(&path.display()).unwrap();
    let mut literals = Rodeo::new();
    let mir: Vec<_> =
        modules.iter().map(|module| EvalMir::new(&db, module, &mut literals)).collect();
    let name = path.file_stem().unwrap().to_string_lossy();
    let llmod = unsafe { back.new_module(&*name).unwrap() };

    let mut cg_units: Vec<_> = zip(&modules, &mir)
        .map(|(module, mir)| {
            let unit = OsdiCompilationUnit::new(&db, mir, module);
            unit.intern_names(&mut literals);
            unit
        })
        .collect();

    let mut cx = unsafe { back.new_ctx(&literals, &llmod) };
    cx.include_bitcode(STDLIB_BITCODE);
    let target_data = unsafe {
        let src = CString::new(target.data_layout.clone()).unwrap();
        llvm::LLVMCreateTargetData(src.as_ptr())
    };

    for fun in function_iter(llmod.llmod()) {
        unsafe {
            if LLVMIsDeclaration(fun) != llvm::False {
                continue;
            }

            LLVMSetLinkage(fun, InternalLinkage);
            LLVMSetUnnamedAddress(fun, UnnamedAddr::Global);
        }
    }

    let tys = OsdiTys::new(&cx, target_data);

    let descriptors: Vec<_> = cg_units
        .iter_mut()
        .map(|unit| {
            unit.init_llvm_cx(&mut cx, &tys);
            let descriptor = unit.descriptor(&mut cx, target_data);
            descriptor.to_ll_val(&mut cx, &tys)
        })
        .collect();

    cx.export_array("OSDI_DESCRIPTORS", tys.osdi_descriptor, &descriptors, true, false);
    cx.export_val(
        "OSDI_NUM_DESCRIPTORS",
        cx.ty_int(),
        cx.const_unsigned_int(descriptors.len() as u32),
        true,
    );
    cx.export_val("OSDI_VERSION_MAJOR", cx.ty_int(), cx.const_unsigned_int(OSDI_VERSION.0), true);
    cx.export_val("OSDI_VERSION_MINOR", cx.ty_int(), cx.const_unsigned_int(OSDI_VERSION.1), true);

    unsafe { LLVMDisposeTargetData(target_data) };

    debug_assert!(llmod.verify_and_print());

    llmod.optimize(back);

    println!("{}", llmod.to_str());
    assert_eq!(llmod.emit_obect(dst), Ok(()))
}

impl OsdiCompilationUnit<'_, '_> {
    fn intern_names(&self, literals: &mut Rodeo) {
        literals.get_or_intern(&*self.module.id.lookup(self.db).name(self.db));
        self.intern_node_strs(literals);

        for param in self.module.params.values() {
            for alias in &param.alias {
                literals.get_or_intern(&**alias);
            }

            literals.get_or_intern(&*param.name);
            literals.get_or_intern(&param.unit);
            literals.get_or_intern(&param.description);
            literals.get_or_intern(&param.group);
        }

        for opvar in self.module.op_vars.values() {
            literals.get_or_intern(&*opvar.name);
            literals.get_or_intern(&opvar.unit);
            literals.get_or_intern(&opvar.description);
        }

        for param in self.mir.eval_intern.params.raw.keys() {
            if let ParamKind::ParamSysFun(param) = param {
                literals.get_or_intern(format!("${param:?}"));
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct OsdiNodeId(u32);
impl_idx_from!(OsdiNodeId(u32));
impl_debug_display! {match OsdiNodeId{OsdiNodeId(id) => "res{id}";}}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct OsdiMatrixId(u32);
impl_idx_from!(OsdiMatrixId(u32));
impl_debug_display! {match OsdiMatrixId{OsdiMatrixId(id) => "matrix{id}";}}

fn ty_len(ty: &Type) -> Option<u32> {
    match ty {
        Type::Array { ty, len } => Some(len * ty_len(ty).unwrap_or(1)),
        Type::EmptyArray => Some(0),
        _ => None,
    }
}

fn lltype<'ll>(ty: &Type, cx: &CodegenCx<'_, 'll>) -> &'ll llvm::Type {
    let llty = match ty.base_type() {
        Type::Real => cx.ty_real(),
        Type::Integer => cx.ty_int(),
        Type::String => cx.ty_str(),
        Type::EmptyArray => cx.ty_array(cx.ty_int(), 0),
        Type::Bool => cx.ty_c_bool(),
        Type::Void => cx.ty_void(),
        Type::Err | Type::Array { .. } => unreachable!(),
    };

    if let Some(len) = ty_len(ty) {
        cx.ty_array(llty, len)
    } else {
        llty
    }
}
