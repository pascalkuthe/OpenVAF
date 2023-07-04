use base_n::CASE_INSENSITIVE;
use camino::{Utf8Path, Utf8PathBuf};
use hir_def::{Lookup, Type};
use hir_lower::{CallBackKind, ParamKind};
use lasso::Rodeo;
use llvm::{LLVMDisposeTargetData, OptLevel};
use mir_llvm::{CodegenCx, LLVMBackend};
use salsa::ParallelDatabase;
use sim_back::{CompilationDB, EvalMir, ModuleInfo};
use stdx::iter::zip;
use stdx::{impl_debug_display, impl_idx_from};
use target::spec::Target;
use typed_indexmap::TiSet;

use std::ffi::CString;

use crate::compilation_unit::{new_codegen, OsdiCompilationUnit, OsdiModule};
use crate::metadata::osdi_0_3::OsdiTys;
use crate::metadata::OsdiLimFunction;

mod access;
mod bitfield;
mod compilation_unit;
mod inst_data;
mod metadata;
mod model_data;

mod eval;
mod load;
mod setup;

const OSDI_VERSION: (u32, u32) = (0, 3);

pub fn compile(
    db: &CompilationDB,
    modules: &[ModuleInfo],
    dst: &Utf8Path,
    target: &Target,
    back: &LLVMBackend,
    emit: bool,
    opt_lvl: OptLevel,
) -> Vec<Utf8PathBuf> {
    let mut literals = Rodeo::new();
    let mut lim_table = TiSet::default();
    let mir: Vec<_> = modules
        .iter()
        .map(|module| {
            let mir = EvalMir::new(db, module, &mut literals);
            for cb in mir.eval_intern.callbacks.iter() {
                if let CallBackKind::BuiltinLimit { name, num_args } = *cb {
                    lim_table.ensure(OsdiLimFunction { name, num_args: num_args - 2 });
                }
            }
            mir
        })
        .collect();
    let name = dst.file_stem().expect("destition is a file").to_owned();

    let mut paths: Vec<Utf8PathBuf> = (0..modules.len() * 4)
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
            let unit = OsdiModule::new(db, mir, module, &lim_table);
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
                let cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let cguint = OsdiCompilationUnit::new(module, &cx, &tys, false);

                cguint.access_function();
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4];
                    llmod.optimize();
                    assert_eq!(llmod.emit_object(path.as_ref()), Ok(()))
                }
            });

            scope.spawn(move |_| {
                let name = format!("setup_model_{}", &module.sym);
                let llmod = unsafe { back.new_module(&name, opt_lvl).unwrap() };
                let cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let cguint = OsdiCompilationUnit::new(module, &cx, &tys, false);

                cguint.setup_model();
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4 + 1];
                    // llmod.optimize();
                    assert_eq!(llmod.emit_object(path.as_ref()), Ok(()))
                }
            });

            scope.spawn(move |_| {
                let name = format!("setup_instance_{}", &module.sym);
                let llmod = unsafe { back.new_module(&name, opt_lvl).unwrap() };
                let cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let mut cguint = OsdiCompilationUnit::new(module, &cx, &tys, false);

                cguint.setup_instance();
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4 + 2];
                    llmod.optimize();
                    assert_eq!(llmod.emit_object(path.as_ref()), Ok(()))
                }
            });

            scope.spawn(move |_| {
                let access = format!("eval_{}", &module.sym);
                let llmod = unsafe { back.new_module(&access, opt_lvl).unwrap() };
                let cx = new_codegen(back, &llmod, literals_);
                let tys = OsdiTys::new(&cx, target_data_);
                let cguint = OsdiCompilationUnit::new(module, &cx, &tys, true);

                // println!("{:?}", module.mir.eval_func);
                cguint.eval();
                // println!("{}", llmod.to_str());
                debug_assert!(llmod.verify_and_print());

                if emit {
                    let path = &paths[i * 4 + 3];
                    llmod.optimize();
                    assert_eq!(llmod.emit_object(path.as_ref()), Ok(()))
                }
            });
        }

        let llmod = unsafe { back.new_module(&name, opt_lvl).unwrap() };
        let cx = new_codegen(back, &llmod, &literals);
        let tys = OsdiTys::new(&cx, target_data);

        let descriptors: Vec<_> = modules
            .iter()
            .map(|module| {
                let cguint = OsdiCompilationUnit::new(module, &cx, &tys, false);
                let descriptor = cguint.descriptor(target_data, &db);
                descriptor.to_ll_val(&cx, &tys)
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

        if !lim_table.is_empty() {
            let lim: Vec<_> = lim_table.iter().map(|entry| entry.to_ll_val(&cx, &tys)).collect();
            cx.export_array("OSDI_LIM_TABLE", tys.osdi_lim_function, &lim, false, false);
            cx.export_val(
                "OSDI_LIM_TABLE_LEN",
                cx.ty_int(),
                cx.const_unsigned_int(lim.len() as u32),
                true,
            );
        }

        let osdi_log =
            cx.get_declared_value("osdi_log").expect("symbol osdi_log missing from std lib");
        let val = cx.const_null_ptr();
        unsafe {
            llvm::LLVMSetInitializer(osdi_log, val);
            llvm::LLVMSetLinkage(osdi_log, llvm::Linkage::ExternalLinkage);
            llvm::LLVMSetUnnamedAddress(osdi_log, llvm::UnnamedAddr::No);
            llvm::LLVMSetDLLStorageClass(osdi_log, llvm::DLLStorageClass::Export);
        }

        debug_assert!(llmod.verify_and_print());

        if emit {
            // println!("{}", llmod.to_str());
            llmod.optimize();
            // println!("{}", llmod.to_str());
            assert_eq!(llmod.emit_object(main_file.as_ref()), Ok(()))
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
        literals.get_or_intern_static("Multiplier (Verilog-A $mfactor)");
        literals.get_or_intern_static("deg");
        literals.get_or_intern_static("m");

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

        for alias_list in self.base.sys_fun_alias.values() {
            for alias in alias_list {
                literals.get_or_intern(&**alias);
            }
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct OsdiLimId(u32);
impl_idx_from!(OsdiLimId(u32));
impl_debug_display! {match OsdiLimId{OsdiLimId(id) => "lim{id}";}}

fn ty_len(ty: &Type) -> Option<u32> {
    match ty {
        Type::Array { ty, len } => Some(len * ty_len(ty).unwrap_or(1)),
        Type::EmptyArray => Some(0),
        _ => None,
    }
}

fn lltype<'ll>(ty: &Type, cx: &CodegenCx<'_, 'll>) -> &'ll llvm::Type {
    let llty = match ty.base_type() {
        Type::Real => cx.ty_double(),
        Type::Integer => cx.ty_int(),
        Type::String => cx.ty_ptr(),
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
