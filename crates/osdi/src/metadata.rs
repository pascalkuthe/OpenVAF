use std::iter::once;

use hir_def::db::HirDefDB;
use hir_def::{Lookup, Type};
use hir_ty::db::HirTyDB;
use lasso::Rodeo;
use llvm::{LLVMABISizeOfType, LLVMOffsetOfElement, TargetData};
use mir_llvm::CodegenCx;
use sim_back::matrix::MatrixEntry;
use smol_str::SmolStr;

use crate::compilation_unit::OsdiCompilationUnit;
use crate::inst_data::{
    OsdiInstanceParam, COLLAPSED, JACOBIAN_PTR_REACT, JACOBIAN_PTR_RESIST, NODE_MAPPING,
};
use crate::load::JacobianLoadType;
use crate::metadata::osdi_0_3::{
    OsdiDescriptor, OsdiJacobianEntry, OsdiNode, OsdiNodePair, OsdiParamOpvar,
    JACOBIAN_ENTRY_REACT, JACOBIAN_ENTRY_REACT_CONST, JACOBIAN_ENTRY_RESIST,
    JACOBIAN_ENTRY_RESIST_CONST, PARA_KIND_INST, PARA_KIND_MODEL, PARA_KIND_OPVAR, PARA_TY_INT,
    PARA_TY_REAL, PARA_TY_STR,
};
use crate::ty_len;

#[allow(unused_parens, dead_code)]
pub mod osdi_0_3;

impl<'a, 'll> OsdiCompilationUnit<'a, 'll> {
    pub fn param_opvar(&self) -> Vec<OsdiParamOpvar> {
        fn para_ty_flags(ty: &Type) -> u32 {
            match ty.base_type() {
                Type::Real => PARA_TY_REAL,
                Type::Integer => PARA_TY_INT,
                Type::String => PARA_TY_STR,
                _ => unreachable!(),
            }
        }

        let inst_params = self.inst_data().params.keys().map(|param| {
            match param {
                OsdiInstanceParam::Builtin(builtin) => OsdiParamOpvar {
                    // TODO alias
                    name: vec![format!("${builtin:?}")],
                    num_alias: 0,
                    description: "".to_owned(),
                    units: "".to_owned(),
                    flags: PARA_TY_REAL | PARA_KIND_INST,
                    len: 0,
                },
                OsdiInstanceParam::User(param) => {
                    let param = &self.module.params[param];

                    let flags = para_ty_flags(&param.ty) | PARA_KIND_INST;
                    OsdiParamOpvar {
                        name: once(&param.name)
                            .chain(&*param.alias)
                            .map(SmolStr::to_string)
                            .collect(),
                        num_alias: param.alias.len() as u32,
                        description: param.description.clone(),
                        units: param.unit.clone(),
                        flags,
                        len: ty_len(&param.ty).unwrap_or(0),
                    }
                }
            }
        });

        let model_params = self.model_data().params.keys().filter_map(|param| {
            let param = &self.module.params[param];
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

        let opvars = self.inst_data().opvars.keys().map(|opvar| {
            let opvar = &self.module.op_vars[opvar];
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

    pub fn nodes(&self, target_data: &TargetData) -> Vec<OsdiNode> {
        self.node_ids
            .iter_enumerated()
            .map(|(id, node)| {
                // TODO flows
                let discipline = self.db.node_discipline(*node).unwrap();
                let discipline = self.db.discipline_info(discipline);

                let inst_data = self.inst_data();
                let resist_residual_off =
                    inst_data.residual_off(id, false, target_data).unwrap_or(u32::MAX);
                let react_residual_off =
                    inst_data.residual_off(id, true, target_data).unwrap_or(u32::MAX);
                OsdiNode {
                    name: self.db.node_data(*node).name.to_string(),
                    units: self
                        .db
                        .nature_info(discipline.potential.unwrap())
                        .units
                        .as_ref()
                        .map_or_else(String::new, String::clone),
                    residual_units: self
                        .db
                        .nature_info(discipline.flow.unwrap())
                        .units
                        .as_ref()
                        .map_or_else(String::new, String::clone),
                    resist_residual_off,
                    react_residual_off,
                    is_flow: false, // TODO
                }
            })
            .collect()
    }

    pub fn intern_node_strs(&self, intern: &mut Rodeo) {
        for node in self.node_ids.raw.iter() {
            intern.get_or_intern(&*self.db.node_data(*node).name);

            let discipline = self.db.node_discipline(*node).unwrap();
            let discipline = self.db.discipline_info(discipline);
            if let Some(units) = self.db.nature_info(discipline.potential.unwrap()).units.as_ref() {
                intern.get_or_intern(units);
            }
            if let Some(units) = self.db.nature_info(discipline.flow.unwrap()).units.as_ref() {
                intern.get_or_intern(units);
            }
        }
    }

    fn is_const(&self, entry: MatrixEntry, reactive: bool) -> bool {
        let matrix = &self.mir.matrix;
        let func = &self.mir.eval_func;
        let matrix = if reactive { &matrix.reactive } else { &matrix.resistive };
        if let Some(val) = matrix.raw.get(&entry) {
            func.dfg.value_def(*val).inst().is_none()
        } else {
            false
        }
    }

    pub fn jacobian_entries(&self, target_data: &TargetData) -> Vec<OsdiJacobianEntry> {
        let mut jacobian_ptr_react_offset =
            unsafe { LLVMOffsetOfElement(target_data, self.inst_data().ty, JACOBIAN_PTR_REACT) }
                as u32;

        self.matrix_ids
            .raw
            .iter()
            .map(|entry| {
                let mut flags = 0;
                let mut react_ptr_off = u32::MAX;

                let entry_ = entry.to_middle(&self.node_ids);
                if self.is_const(entry_, false) {
                    flags |= JACOBIAN_ENTRY_RESIST_CONST
                }

                if self.is_const(entry_, true) {
                    flags |= JACOBIAN_ENTRY_REACT_CONST
                }

                if self.mir.matrix.resistive.contains_key(&entry_) {
                    flags |= JACOBIAN_ENTRY_RESIST;
                }

                if self.mir.matrix.reactive.contains_key(&entry_) {
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
        self.mir
            .collapse
            .raw
            .iter()
            .map(|(hi, lo)| {
                let node_1 = self.node_ids.unwrap_index(hi).0;
                let node_2 = lo.map_or(u32::MAX, |lo| self.node_ids.unwrap_index(&lo).0);
                OsdiNodePair { node_1, node_2 }
            })
            .collect()
    }

    pub fn descriptor(
        &self,
        cx: &mut CodegenCx<'_, 'll>,
        target_data: &llvm::TargetData,
    ) -> OsdiDescriptor<'ll> {
        let collapsible = self.collapsible();
        let inst_data = self.inst_data();
        let model_data = self.model_data();
        unsafe {
            let node_mapping_offset =
                LLVMOffsetOfElement(target_data, inst_data.ty, NODE_MAPPING) as u32;
            let jacobian_ptr_resist_offset =
                LLVMOffsetOfElement(target_data, inst_data.ty, JACOBIAN_PTR_RESIST) as u32;

            let collapsed_offset = LLVMOffsetOfElement(target_data, inst_data.ty, COLLAPSED) as u32;

            let instance_size = LLVMABISizeOfType(target_data, inst_data.ty) as u32;
            let model_size = LLVMABISizeOfType(target_data, self.model_data().ty) as u32;

            OsdiDescriptor {
                name: self.module.id.lookup(self.db).name(self.db).to_string(),
                num_nodes: self.node_ids.len() as u32,
                num_terminals: self.num_terminals,
                nodes: self.nodes(target_data),
                num_jacobian_entries: self.matrix_ids.len() as u32,
                jacobian_entries: self.jacobian_entries(target_data),
                num_collapsible: collapsible.len() as u32,
                collapsible,
                collapsed_offset,

                // TODO noise
                noise_sources: Vec::new(),
                num_noise_src: 0,

                num_params: model_data.params.len() as u32 + inst_data.params.len() as u32,
                num_instance_params: inst_data.params.len() as u32,
                num_opvars: inst_data.opvars.len() as u32,
                param_opvar: self.param_opvar(),

                node_mapping_offset,
                jacobian_ptr_resist_offset,
                instance_size,
                model_size,
                access: self.access_function(cx),
                setup_model: self.setup_model(cx),
                setup_instance: self.setup_instance(cx),
                eval: self.eval(cx),
                load_noise: self.load_noise(cx),
                load_residual_resist: self.load_residual(cx, false),
                load_residual_react: self.load_residual(cx, true),
                load_spice_rhs_dc: self.load_spice_rhs(cx, false),
                load_spice_rhs_tran: self.load_spice_rhs(cx, true),
                load_jacobian_resist: self.load_jacobian(cx, JacobianLoadType::Resist),
                load_jacobian_react: self.load_jacobian(cx, JacobianLoadType::React),
                load_jacobian_tran: self.load_jacobian(cx, JacobianLoadType::Tran),
            }
        }
    }
}
