use std::sync::Arc;

use basedb::Upcast;
use hir_def::db::HirDefDB;
use hir_def::nameres::{ResolvedPath, ScopeDefItem};
use hir_def::{
    AliasParamId, BranchId, DefWithBodyId, DisciplineId, Lookup, NatureAttrId, NatureId, ParamId,
    Type,
};

use crate::inference::InferenceResult;
use crate::lower::{BranchTy, DisciplineTy, NatureTy};

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
    fn nature_attr_ty(&self, id: NatureAttrId) -> Option<Type>;

    #[salsa::cycle(resolve_alias_recover)]
    fn resolve_alias(&self, id: AliasParamId) -> Option<ParamId>;
}

fn nature_attr_ty(db: &dyn HirTyDB, id: NatureAttrId) -> Option<Type> {
    let body = db.body(id.into());
    let expr = body.stmts[body.entry_stmts[0]].unwrap_expr();
    db.inference_result(id.into()).expr_types.get(expr).and_then(|ty| ty.to_value())
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn nature_attr_ty_recover(
    _db: &dyn HirTyDB,
    _cycel: &[String],
    _id: &NatureAttrId,
) -> Option<Type> {
    None
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn resolve_alias_recover(
    _db: &dyn HirTyDB,
    _cycel: &[String],
    _id: &AliasParamId,
) -> Option<ParamId> {
    None
}

// TODO validate
// TODO allow $mfactor etc
fn resolve_alias(db: &dyn HirTyDB, id: AliasParamId) -> Option<ParamId> {
    let loc = id.lookup(db.upcast());
    let data = db.alias_data(id);
    match loc.scope.resolve_path(db.upcast(), data.src.as_ref()?).ok()? {
        ResolvedPath::ScopeDefItem(ScopeDefItem::ParamId(param)) => Some(param),
        ResolvedPath::ScopeDefItem(ScopeDefItem::AliasParamId(alias)) => db.resolve_alias(alias),
        _ => None,
    }
}
