use std::sync::Arc;

use basedb::Upcast;
use hir_def::db::HirDefDB;
use hir_def::{
    BranchId, DefWithBehaviourId, DefWithBodyId, DefWithExprId, DisciplineId, NatureAttrId,
    NatureId, ParamId,
};

use crate::inference::InferenceResult;
use crate::lower::{BranchTy, DisciplineTy, NatureTy};
use crate::types::Ty;

#[salsa::query_group(HirTyDatabase)]
pub trait HirTyDB: HirDefDB + Upcast<dyn HirDefDB> {
    #[salsa::invoke(NatureTy::nature_info_query)]
    #[salsa::cycle(NatureTy::nature_info_recover)]
    fn nature_info(&self, nature: NatureId) -> Arc<NatureTy>;
    #[salsa::invoke(DisciplineTy::discipline_info_query)]
    fn discipline_info(&self, nature: DisciplineId) -> Arc<DisciplineTy>;
    #[salsa::invoke(BranchTy::branch_info_query)]
    fn branch_info(&self, branch: BranchId) -> Option<Arc<BranchTy>>;

    #[salsa::invoke(InferenceResult::infere_expr_body_query)]
    fn infere_expr_body(&self, id: DefWithExprId) -> Arc<InferenceResult>;

    #[salsa::invoke(InferenceResult::infere_param_body_query)]
    fn infere_param_body(&self, id: ParamId) -> Arc<InferenceResult>;

    #[salsa::invoke(InferenceResult::infere_analog_behaviour_query)]
    fn infere_behaviour(&self, id: DefWithBehaviourId) -> Arc<InferenceResult>;

    #[salsa::transparent]
    fn inference_result(&self, id: DefWithBodyId) -> Arc<InferenceResult>;

    #[salsa::cycle(nature_attr_ty_recover)]
    fn nature_attr_ty(&self, id: NatureAttrId) -> Option<Ty>;
}

fn inference_result(db: &dyn HirTyDB, id: DefWithBodyId) -> Arc<InferenceResult> {
    match id {
        DefWithBodyId::ParamId(param) => db.infere_param_body(param),
        DefWithBodyId::ModuleId(module) => db.infere_behaviour(module.into()),
        DefWithBodyId::FunctionId(function) => db.infere_behaviour(function.into()),
        DefWithBodyId::VarId(var) => db.infere_expr_body(var.into()),
        DefWithBodyId::NatureAttrId(attr) => db.infere_expr_body(attr.into()),
        DefWithBodyId::DisciplineAttrId(attr) => db.infere_expr_body(attr.into()),
    }
}

fn nature_attr_ty(db: &dyn HirTyDB, id: NatureAttrId) -> Option<Ty> {
    let expr = db.expr_body(id.into()).val;
    db.infere_expr_body(id.into()).expr_types.get(expr).cloned()
}

fn nature_attr_ty_recover(_db: &dyn HirTyDB, _cycel: &[String], _id: &NatureAttrId) -> Option<Ty> {
    None
}
