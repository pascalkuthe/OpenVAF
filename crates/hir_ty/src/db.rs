use std::sync::Arc;

use basedb::Upcast;
use hir_def::db::HirDefDB;
use hir_def::{BranchId, DefWithBodyId, DisciplineId, NatureAttrId, NatureId};

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

    #[salsa::invoke(InferenceResult::infere_body_query)]
    fn inference_result(&self, id: DefWithBodyId) -> Arc<InferenceResult>;

    #[salsa::cycle(nature_attr_ty_recover)]
    fn nature_attr_ty(&self, id: NatureAttrId) -> Option<Ty>;
}

fn nature_attr_ty(db: &dyn HirTyDB, id: NatureAttrId) -> Option<Ty> {
    let body = db.body(id.into());
    let expr = body.stmts[body.entry_stmts[0]].unwrap_expr();
    db.inference_result(id.into()).expr_types.get(expr).cloned()
}

fn nature_attr_ty_recover(_db: &dyn HirTyDB, _cycel: &[String], _id: &NatureAttrId) -> Option<Ty> {
    None
}
