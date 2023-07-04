use std::iter::once;

use hir_def::db::HirDefDB;
use hir_def::{Lookup, ParamSysFun, Type};
use hir_lower::CurrentKind;
use hir_ty::db::HirTyDB;
use lasso::{Rodeo, Spur};
use llvm::{LLVMABISizeOfType, LLVMOffsetOfElement, TargetData};
use mir_llvm::CodegenCx;
use sim_back::matrix::MatrixEntry;
use sim_back::{CompilationDB, SimUnknown};
use smol_str::SmolStr;

use crate::compilation_unit::{OsdiCompilationUnit, OsdiModule};
use crate::inst_data::{
    OsdiInstanceParam, COLLAPSED, JACOBIAN_PTR_REACT, JACOBIAN_PTR_RESIST, NODE_MAPPING, STATE_IDX,
};
use crate::load::JacobianLoadType;
use crate::metadata::osdi_0_3::{
    OsdiDescriptor, OsdiJacobianEntry, OsdiNode, OsdiNodePair, OsdiParamOpvar, OsdiTys,
    JACOBIAN_ENTRY_REACT, JACOBIAN_ENTRY_REACT_CONST, JACOBIAN_ENTRY_RESIST,
    JACOBIAN_ENTRY_RESIST_CONST, PARA_KIND_INST, PARA_KIND_MODEL, PARA_KIND_OPVAR, PARA_TY_INT,
    PARA_TY_REAL, PARA_TY_STR,
};
use crate::ty_len;

#[allow(unused_parens, dead_code)]
pub mod osdi_0_3;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct OsdiLimFunction {
    pub name: Spur,
    pub num_args: u32,
}

impl OsdiLimFunction {
    pub fn to_ll_val<'ll>(self, ctx: &CodegenCx<'_, 'll>, tys: &'ll OsdiTys) -> &'ll llvm::Value {
        osdi_0_3::OsdiLimFunction {
            name: ctx.literals.resolve(&self.name).to_owned(),
            num_args: self.num_args,
            func_ptr: ctx.const_null_ptr(),
        }
        .to_ll_val(ctx, tys)
    }
}

impl<'ll> OsdiCompilationUnit<'_, '_, 'll> {
    pub fn param_opvar(&self) -> Vec<OsdiParamOpvar> {
        let OsdiCompilationUnit { inst_data, model_data, module, .. } = self;
        fn para_ty_flags(ty: &Type) -> u32 {
            match ty.base_type() {
                Type::Real => PARA_TY_REAL,
                Type::Integer => PARA_TY_INT,
                Type::String => PARA_TY_STR,
                _ => unreachable!(),
            }
        }

        let inst_params = inst_data.params.keys().map(|param| match param {
            OsdiInstanceParam::Builtin(builtin) => {
                let mut name = vec![format!("${builtin:?}")];
                if let Some(alias) = self.module.base.sys_fun_alias.get(builtin) {
                    name.extend(alias.iter().map(SmolStr::to_string))
                }
                OsdiParamOpvar {
                    num_alias: name.len() as u32 - 1,
                    name,
                    description: match builtin {
                        ParamSysFun::mfactor => "Multiplier (Verilog-A $mfactor)".to_owned(),
                        _ => "".to_owned(),
                    },
                    units: match builtin {
                        ParamSysFun::yposition | ParamSysFun::xposition => "m".to_owned(),
                        ParamSysFun::angle => "deg".to_owned(),
                        _ => "".to_owned(),
                    },
                    flags: PARA_TY_REAL | PARA_KIND_INST,
                    len: 0,
                }
            }
            OsdiInstanceParam::User(param) => {
                let param = &module.base.params[param];

                let flags = para_ty_flags(&param.ty) | PARA_KIND_INST;
                OsdiParamOpvar {
                    name: once(&param.name).chain(&*param.alias).map(SmolStr::to_string).collect(),
                    num_alias: param.alias.len() as u32,
                    description: param.description.clone(),
                    units: param.unit.clone(),
                    flags,
                    len: ty_len(&param.ty).unwrap_or(0),
                }
            }
        });

        let model_params = model_data.params.keys().filter_map(|param| {
            let param = &module.base.params[param];
            if param.is_instance {
                return None;
            }
            let flags = para_ty_flags(&param.ty) | PARA_KIND_MODEL;
            let param_opvar = OsdiParamOpvar {
                name: once(&param.name).chain(&*param.alias).map(SmolStr::to_string).collect(),
                num_alias: param.alias.len() as u32,
                description: param.description.clone(),
                units: param.unit.clone(),
                flags,
                len: ty_len(&param.ty).unwrap_or(0),
            };
            Some(param_opvar)
        });

        let opvars = inst_data.opvars.keys().map(|opvar| {
            let opvar = &module.base.op_vars[opvar];
            // TODO inst params
            let flags = para_ty_flags(&opvar.ty) | PARA_KIND_OPVAR;
            OsdiParamOpvar {
                name: vec![opvar.name.to_string()],
                num_alias: 0,
                description: opvar.description.clone(),
                units: opvar.unit.clone(),
                flags,
                len: ty_len(&opvar.ty).unwrap_or(0),
            }
        });

        inst_params.chain(model_params).chain(opvars).collect()
    }

    pub fn nodes(&self, target_data: &TargetData, db: &CompilationDB) -> Vec<OsdiNode> {
        let OsdiCompilationUnit { inst_data, module, .. } = self;
        module
            .node_ids
            .iter_enumerated()
            .map(|(id, unknown)| {
                let (name, units, is_flow) = sim_unknown_info(*unknown, db);
                let resist_residual_off =
                    inst_data.residual_off(id, false, target_data).unwrap_or(u32::MAX);
                let react_residual_off =
                    inst_data.residual_off(id, true, target_data).unwrap_or(u32::MAX);

                let resist_limit_rhs_off =
                    inst_data.lim_rhs_off(id, false, target_data).unwrap_or(u32::MAX);
                let react_limit_rhs_off =
                    inst_data.lim_rhs_off(id, true, target_data).unwrap_or(u32::MAX);
                OsdiNode {
                    name,
                    units,
                    residual_units: String::new(),
                    resist_residual_off,
                    react_residual_off,
                    is_flow,
                    resist_limit_rhs_off,
                    react_limit_rhs_off,
                }
            })
            .collect()
    }

    fn is_const(&self, entry: MatrixEntry, reactive: bool) -> bool {
        let matrix = &self.module.mir.matrix;
        let func = &self.module.mir.eval_func;
        let matrix = if reactive { &matrix.reactive } else { &matrix.resistive };
        if let Some(val) = matrix.raw.get(&entry) {
            func.dfg.value_def(*val).inst().is_none()
        } else {
            false
        }
    }

    pub fn jacobian_entries(&self, target_data: &TargetData) -> Vec<OsdiJacobianEntry> {
        let OsdiCompilationUnit { inst_data, module, .. } = self;
        let mut jacobian_ptr_react_offset =
            unsafe { LLVMOffsetOfElement(target_data, inst_data.ty, JACOBIAN_PTR_REACT) } as u32;

        module
            .matrix_ids
            .raw
            .iter()
            .map(|entry| {
                let mut flags = 0;
                let mut react_ptr_off = u32::MAX;

                let entry_ = entry.to_middle(&module.node_ids);
                if self.is_const(entry_, false) {
                    flags |= JACOBIAN_ENTRY_RESIST_CONST
                }

                if self.is_const(entry_, true) {
                    flags |= JACOBIAN_ENTRY_REACT_CONST
                }

                if module.mir.matrix.resistive.contains_key(&entry_) {
                    flags |= JACOBIAN_ENTRY_RESIST;
                }

                if module.mir.matrix.reactive.contains_key(&entry_) {
                    flags |= JACOBIAN_ENTRY_REACT;
                    react_ptr_off = jacobian_ptr_react_offset;
                    jacobian_ptr_react_offset += 8;
                }
                OsdiJacobianEntry {
                    nodes: OsdiNodePair { node_1: entry.row.0, node_2: entry.col.0 },
                    react_ptr_off,
                    flags,
                }
            })
            .collect()
    }

    pub fn collapsible(&self) -> Vec<OsdiNodePair> {
        self.module
            .mir
            .collapse
            .raw
            .keys()
            .map(|(hi, lo)| {
                let node_1 = self.module.node_ids.unwrap_index(hi).0;
                let node_2 = lo.map_or(u32::MAX, |lo| self.module.node_ids.unwrap_index(&lo).0);
                OsdiNodePair { node_1, node_2 }
            })
            .collect()
    }

    pub fn descriptor(
        &self,
        target_data: &llvm::TargetData,
        db: &CompilationDB,
    ) -> OsdiDescriptor<'ll> {
        let collapsible = self.collapsible();
        let OsdiCompilationUnit { ref inst_data, ref model_data, module, .. } = *self;

        unsafe {
            let node_mapping_offset =
                LLVMOffsetOfElement(target_data, inst_data.ty, NODE_MAPPING) as u32;
            let jacobian_ptr_resist_offset =
                LLVMOffsetOfElement(target_data, inst_data.ty, JACOBIAN_PTR_RESIST) as u32;

            let collapsed_offset = LLVMOffsetOfElement(target_data, inst_data.ty, COLLAPSED) as u32;
            let bound_step_offset = inst_data.bound_step.map_or(u32::MAX, |slot| {
                let elem = inst_data.cache_slot_elem(slot);
                LLVMOffsetOfElement(target_data, inst_data.ty, elem) as u32
            });

            let state_idx_off = LLVMOffsetOfElement(target_data, inst_data.ty, STATE_IDX) as u32;

            let instance_size = LLVMABISizeOfType(target_data, inst_data.ty) as u32;
            let model_size = LLVMABISizeOfType(target_data, model_data.ty) as u32;

            OsdiDescriptor {
                name: module.base.id.lookup(db).name(db).to_string(),
                num_nodes: module.node_ids.len() as u32,
                num_terminals: module.num_terminals,
                nodes: self.nodes(target_data, db),
                num_jacobian_entries: module.matrix_ids.len() as u32,
                jacobian_entries: self.jacobian_entries(target_data),
                num_collapsible: collapsible.len() as u32,
                collapsible,
                collapsed_offset,
                bound_step_offset,

                // TODO noise
                noise_sources: Vec::new(),
                num_noise_src: 0,

                num_params: model_data.params.len() as u32 + inst_data.params.len() as u32,
                num_instance_params: inst_data.params.len() as u32,
                num_opvars: inst_data.opvars.len() as u32,
                param_opvar: self.param_opvar(),

                node_mapping_offset,
                jacobian_ptr_resist_offset,
                state_idx_off,
                instance_size,
                model_size,
                access: self.access_function_prototype(),
                setup_model: self.setup_model_prototype(),
                setup_instance: self.setup_instance_prototype(),
                eval: self.eval_prototype(),
                load_noise: self.load_noise(),
                load_residual_resist: self.load_residual(false),
                load_residual_react: self.load_residual(true),
                load_spice_rhs_dc: self.load_spice_rhs(false),
                load_spice_rhs_tran: self.load_spice_rhs(true),
                load_jacobian_resist: self.load_jacobian(JacobianLoadType::Resist),
                load_jacobian_react: self.load_jacobian(JacobianLoadType::React),
                load_jacobian_tran: self.load_jacobian(JacobianLoadType::Tran),
                num_states: self.module.mir.eval_intern.lim_state.len() as u32,
                load_limit_rhs_resist: self.load_lim_rhs(false),
                load_limit_rhs_react: self.load_lim_rhs(true),
            }
        }
    }
}

impl OsdiModule<'_> {
    pub fn intern_node_strs(&self, intern: &mut Rodeo, db: &CompilationDB) {
        for unknown in self.node_ids.iter() {
            let (name, units, _) = sim_unknown_info(*unknown, db);
            intern.get_or_intern(&name);
            intern.get_or_intern(&units);
        }
    }
}

fn sim_unknown_info(unknown: SimUnknown, db: &CompilationDB) -> (String, String, bool) {
    let name;
    let discipline;
    let is_flow;

    match unknown {
        SimUnknown::KirchoffLaw(node) => {
            name = db.node_data(node).name.to_string();
            discipline = db.node_discipline(node);
            is_flow = false;
        }

        SimUnknown::Current(CurrentKind::Unnamed { hi, lo }) => {
            let hi_ = db.node_data(hi);
            name = if let Some(lo) = lo {
                let lo = db.node_data(lo);
                format!("flow({},{})", &hi_.name, &lo.name)
            } else {
                format!("flow({})", &hi_.name)
            };
            discipline = Some(db.node_discipline(hi).unwrap());
            is_flow = true;
        }
        SimUnknown::Current(CurrentKind::Branch(br)) => {
            let br_ = db.branch_data(br);
            name = format!("flow({})", &br_.name);
            discipline = Some(db.branch_info(br).unwrap().discipline);
            is_flow = true;
        }
        SimUnknown::Current(CurrentKind::Port(node)) => {
            let node_ = db.node_data(node);
            name = format!("flow(<{}>)", &node_.name);
            discipline = db.node_discipline(node);
            is_flow = true;
        }
        SimUnknown::Implicit(equ) => {
            name = format!("implicit_equation_{}", u32::from(equ));
            discipline = None;
            is_flow = false;
        }
    };

    let units = if let Some(discipline) = discipline {
        let discipline = db.discipline_info(discipline);
        let nature = if is_flow { discipline.flow } else { discipline.potential };

        db.nature_info(nature.unwrap()).units.as_ref().map_or_else(String::new, String::clone)
    } else {
        String::new()
    };

    (name, units, is_flow)
}
