use std::sync::Arc;

use basedb::Upcast;
use hir_def::{db::HirDefDB, NatureId};

use crate::lower::NatureTy;

#[salsa::query_group(HirTyDatabase)]
pub trait HirTyDB: HirDefDB + Upcast<dyn HirDefDB> {
    #[salsa::invoke(NatureTy::nature_info_query)]
    #[salsa::cycle(NatureTy::nature_info_recover)]
    fn nature_info(&self, nature: NatureId) -> Arc<NatureTy>;
}
