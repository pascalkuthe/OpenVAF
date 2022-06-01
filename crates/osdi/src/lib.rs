use base_n::CASE_INSENSITIVE;
use hir_def::{Lookup, Type};
use hir_lower::ParamKind;
use lasso::Rodeo;
use llvm::{LLVMDisposeTargetData, OptLevel};
use mir_llvm::{CodegenCx, LLVMBackend};
use salsa::ParallelDatabase;
use sim_back::{CompilationDB, EvalMir, ModuleInfo};
use stdx::iter::zip;
use stdx::{impl_debug_display, impl_idx_from};
use target::spec::Target;

use std::ffi::CString;
use std::path::{Path, PathBuf};

use crate::compilation_unit::{new_codegen, OsdiCompilationUnit, OsdiModule};
use crate::metadata::osdi_0_3::OsdiTys;

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

pub fn compile(
    db: &CompilationDB,
    modules: &[ModuleInfo],
    dst: &Path,
    target: &Target,
    back: &LLVMBackend,
    emit: bool,
    opt_lvl: OptLevel,
) -> Vec<PathBuf> {
    let mut literals = Rodeo::new();
    let mir: Vec<_> =
        modules.iter().map(|module| EvalMir::new(db, module, &mut literals)).collect();
    let name = dst.file_stem().unwrap().to_string_lossy().to_owned();

    let mut paths: Vec<PathBuf> = (0..modules.len() * 4)
        .map(|i| {
            let num = base_n::encode((i + 1) as u128, CASE_INSENSITIVE);
            let extension = format!("o{num}");
            dst.with_extension(extension)
        })
        .collect();

    let target_data = unsafe {
        let src = CString::new(target.data_layout.clone()).unwrap();
        llvm::LLVMCreateTargetData(src.as_ptr())
    };

    let modules: Vec<_> = zip(modules, &mir)
        .map(|(module, mir)| {
            let unit = OsdiModule::new(db, mir, module);
            unit.intern_names(&mut literals, db);
            unit
        })
        .collect();

    let db = db.snapshot();

    let main_file = dst.with_extension("o");

    rayon_core::scope(|scope| {
        let db = db;
        let literals_ = &literals;
        let target_data_ = &target_data;
        let paths = &paths;

        for (i, module) in modules.iter().enumerate() {
            scope.spawn(move |_| {
                let access = format!("access_{}", &module.sym);
                let llmod = unsafe { back.new_module(&access, opt_lvl).unwrap() };
                let mut cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let mut cguint = OsdiCompilationUnit::new(module, &mut cx, &tys);

                cguint.access_function();
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4];
                    llmod.optimize();
                    assert_eq!(llmod.emit_obect(&*path), Ok(()))
                }
            });

            scope.spawn(move |_| {
                let name = format!("setup_model_{}", &module.sym);
                let llmod = unsafe { back.new_module(&name, opt_lvl).unwrap() };
                let mut cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let mut cguint = OsdiCompilationUnit::new(module, &mut cx, &tys);

                cguint.setup_model();
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4 + 1];
                    // llmod.optimize();
                    assert_eq!(llmod.emit_obect(&*path), Ok(()))
                }
            });

            scope.spawn(move |_| {
                let name = format!("setup_instance_{}", &module.sym);
                let llmod = unsafe { back.new_module(&name, opt_lvl).unwrap() };
                let mut cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let mut cguint = OsdiCompilationUnit::new(module, &mut cx, &tys);

                cguint.setup_instance();
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4 + 2];
                    llmod.optimize();
                    assert_eq!(llmod.emit_obect(&*path), Ok(()))
                }
            });

            scope.spawn(move |_| {
                let access = format!("eval_{}", &module.sym);
                let llmod = unsafe { back.new_module(&access, opt_lvl).unwrap() };
                let mut cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let mut cguint = OsdiCompilationUnit::new(module, &mut cx, &tys);

                cguint.eval();
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4 + 3];
                    llmod.optimize();
                    assert_eq!(llmod.emit_obect(&*path), Ok(()))
                }
            });
        }

        let llmod = unsafe { back.new_module(&*name, opt_lvl).unwrap() };
        let mut cx = new_codegen(back, &llmod, &literals);
        let tys = OsdiTys::new(&cx, target_data);

        let descriptors: Vec<_> = modules
            .iter()
            .map(|module| {
                let mut cguint = OsdiCompilationUnit::new(module, &mut cx, &tys);
                let descriptor = cguint.descriptor(target_data, &*db);
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
        cx.export_val(
            "OSDI_VERSION_MAJOR",
            cx.ty_int(),
            cx.const_unsigned_int(OSDI_VERSION.0),
            true,
        );
        cx.export_val(
            "OSDI_VERSION_MINOR",
            cx.ty_int(),
            cx.const_unsigned_int(OSDI_VERSION.1),
            true,
        );

        // debug_assert!(llmod.verify_and_print());

        if emit {
            // println!("{}", llmod.to_str());
            llmod.optimize();
            // println!("{}", llmod.to_str());
            assert_eq!(llmod.emit_obect(&*main_file), Ok(()))
        }
    });

    paths.push(main_file);
    unsafe { LLVMDisposeTargetData(target_data) };
    paths
}

impl OsdiModule<'_> {
    fn intern_names(&self, literals: &mut Rodeo, db: &CompilationDB) {
        literals.get_or_intern(&*self.base.id.lookup(db).name(db));
        self.intern_node_strs(literals, db);

        for param in self.base.params.values() {
            for alias in &param.alias {
                literals.get_or_intern(&**alias);
            }

            literals.get_or_intern(&*param.name);
            literals.get_or_intern(&param.unit);
            literals.get_or_intern(&param.description);
            literals.get_or_intern(&param.group);
        }

        for opvar in self.base.op_vars.values() {
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
