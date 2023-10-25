use std::iter::once;

use hir::{CompilationDB, ParamSysFun, Type};
use hir_lower::CurrentKind;
use lasso::{Rodeo, Spur};
use llvm::{LLVMABISizeOfType, LLVMOffsetOfElement, TargetData};
use mir::{ValueDef, F_ZERO};
use mir_llvm::CodegenCx;
use sim_back::dae::MatrixEntry;
use sim_back::SimUnknownKind;
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
                if let Some(alias) = self.module.info.sys_fun_alias.get(builtin) {
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
                let param_info = &module.info.params[param];
                let ty = param.ty(self.db);

                let flags = para_ty_flags(&ty) | PARA_KIND_INST;
                OsdiParamOpvar {
                    name: once(&param_info.name)
                        .chain(&*param_info.alias)
                        .map(SmolStr::to_string)
                        .collect(),
                    num_alias: param_info.alias.len() as u32,
                    description: param_info.description.clone(),
                    units: param_info.unit.clone(),
                    flags,
                    len: ty_len(&ty).unwrap_or(0),
                }
            }
        });

        let model_params = model_data.params.keys().filter_map(|param| {
            let param_info = &module.info.params[param];
            if param_info.is_instance {
                return None;
            }
            let ty = param.ty(self.db);
            let flags = para_ty_flags(&ty) | PARA_KIND_MODEL;
            let param_opvar = OsdiParamOpvar {
                name: once(&param_info.name)
                    .chain(&*param_info.alias)
                    .map(SmolStr::to_string)
                    .collect(),
                num_alias: param_info.alias.len() as u32,
                description: param_info.description.clone(),
                units: param_info.unit.clone(),
                flags,
                len: ty_len(&ty).unwrap_or(0),
            };
            Some(param_opvar)
        });

        let opvars = inst_data.opvars.keys().map(|opvar| {
            let opvar_info = &module.info.op_vars[opvar];
            // TODO inst params
            let ty = opvar.ty(self.db);
            let flags = para_ty_flags(&ty) | PARA_KIND_OPVAR;
            OsdiParamOpvar {
                name: vec![opvar.name(self.db).to_string()],
                num_alias: 0,
                description: opvar_info.description.clone(),
                units: opvar_info.unit.clone(),
                flags,
                len: ty_len(&ty).unwrap_or(0),
            }
        });

        inst_params.chain(model_params).chain(opvars).collect()
    }

    pub fn nodes(&self, target_data: &TargetData, db: &CompilationDB) -> Vec<OsdiNode> {
        let OsdiCompilationUnit { inst_data, module, .. } = self;
        module
            .dae_system
            .unknowns
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

    fn is_const(&self, entry: &MatrixEntry, reactive: bool) -> bool {
        let entry = if reactive { entry.react } else { entry.resist };
        match self.module.eval.dfg.value_def(entry) {
            ValueDef::Result(_, _) => false,
            ValueDef::Param(param) => self
                .module
                .intern
                .params
                .get_index(param)
                .map_or(true, |(kind, _)| !kind.op_dependent()),
            ValueDef::Const(_) => true,
            ValueDef::Invalid => unreachable!(),
        }
    }

    pub fn jacobian_entries(&self, target_data: &TargetData) -> Vec<OsdiJacobianEntry> {
        let OsdiCompilationUnit { inst_data, module, .. } = self;
        let mut jacobian_ptr_react_offset =
            unsafe { LLVMOffsetOfElement(target_data, inst_data.ty, JACOBIAN_PTR_REACT) } as u32;

        module
            .dae_system
            .jacobian
            .iter()
            .map(|entry| {
                let mut flags = 0;
                let mut react_ptr_off = u32::MAX;

                if self.is_const(entry, false) {
                    flags |= JACOBIAN_ENTRY_RESIST_CONST
                }

                if self.is_const(entry, true) {
                    flags |= JACOBIAN_ENTRY_REACT_CONST
                }

                if entry.resist != F_ZERO {
                    flags |= JACOBIAN_ENTRY_RESIST;
                }

                if entry.react != F_ZERO {
                    flags |= JACOBIAN_ENTRY_REACT;
                    react_ptr_off = jacobian_ptr_react_offset;
                    jacobian_ptr_react_offset += 8;
                }
                OsdiJacobianEntry {
                    nodes: OsdiNodePair { node_1: entry.row.into(), node_2: entry.col.into() },
                    react_ptr_off,
                    flags,
                }
            })
            .collect()
    }

    pub fn collapsible(&self) -> Vec<OsdiNodePair> {
        self.module
            .node_collapse
            .pairs()
            .map(|(_, node1, node2)| OsdiNodePair {
                node_1: node1.into(),
                node_2: node2.map_or(u32::MAX, u32::from),
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
            let bound_step_offset = inst_data.bound_step_elem().map_or(u32::MAX, |elem| {
                LLVMOffsetOfElement(target_data, inst_data.ty, elem) as u32
            });

            let state_idx_off = LLVMOffsetOfElement(target_data, inst_data.ty, STATE_IDX) as u32;

            let instance_size = LLVMABISizeOfType(target_data, inst_data.ty) as u32;
            let model_size = LLVMABISizeOfType(target_data, model_data.ty) as u32;

            OsdiDescriptor {
                name: module.info.module.name(db),
                num_nodes: module.dae_system.unknowns.len() as u32,
                num_terminals: module.info.module.ports(db).len() as u32,
                nodes: self.nodes(target_data, db),
                num_jacobian_entries: module.dae_system.jacobian.len() as u32,
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
                num_states: self.module.intern.lim_state.len() as u32,
                load_limit_rhs_resist: self.load_lim_rhs(false),
                load_limit_rhs_react: self.load_lim_rhs(true),
            }
        }
    }
}

impl OsdiModule<'_> {
    pub fn intern_node_strs(&self, intern: &mut Rodeo, db: &CompilationDB) {
        for &unknown in self.dae_system.unknowns.iter() {
            let (name, units, _) = sim_unknown_info(unknown, db);
            intern.get_or_intern(&name);
            intern.get_or_intern(&units);
        }
    }
}

fn sim_unknown_info(unknown: SimUnknownKind, db: &CompilationDB) -> (String, String, bool) {
    let name;
    let discipline;
    let is_flow;

    match unknown {
        SimUnknownKind::KirchoffLaw(node) => {
            name = node.name(db).to_string();
            discipline = Some(node.discipline(db));
            is_flow = false;
        }

        SimUnknownKind::Current(CurrentKind::Unnamed { hi, lo }) => {
            name = if let Some(lo) = lo {
                format!("flow({},{})", &hi.name(db), &lo.name(db))
            } else {
                format!("flow({})", &hi.name(db))
            };
            discipline = Some(hi.discipline(db));
            is_flow = true;
        }
        SimUnknownKind::Current(CurrentKind::Branch(br)) => {
            name = format!("flow({})", &br.name(db));
            discipline = Some(br.discipline(db));
            is_flow = true;
        }
        SimUnknownKind::Current(CurrentKind::Port(node)) => {
            name = format!("flow(<{}>)", &node.name(db));
            discipline = Some(node.discipline(db));
            is_flow = true;
        }
        SimUnknownKind::Implicit(equ) => {
            name = format!("implicit_equation_{}", u32::from(equ));
            discipline = None;
            is_flow = false;
        }
    };

    // its valid to have disciplines without pot/flow nature but then we can't
    // have branches for those so its ok to unwrap here
    let units = discipline
        .map(|discipline| {
            let nature = if is_flow {
                discipline.flow(db).unwrap()
            } else {
                discipline.potential(db).unwrap()
            };
            nature.units(db)
        })
        .unwrap_or_default();

    (name, units, is_flow)
}
