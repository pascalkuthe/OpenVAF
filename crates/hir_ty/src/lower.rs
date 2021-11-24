use std::sync::Arc;

use crate::db::HirTyDB;
use hir_def::{name::kw, DisciplineId, Lookup, Name, NatureAttrId, NatureId};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NatureTy {
    pub ddt_nature: NatureId,
    pub idt_nature: NatureId,
    pub base_nature: NatureId,
    pub abstol: Option<NatureAttrId>,
    pub units: Option<String>,
}

impl NatureTy {
    pub fn nature_info_query(db: &dyn HirTyDB, nature: NatureId) -> Arc<NatureTy> {
        NatureTy::obtain(db, nature, true)
    }
    pub fn obtain(db: &dyn HirTyDB, nature: NatureId, resolve_base_nature: bool) -> Arc<NatureTy> {
        let data = db.nature_data(nature);
        let loc = nature.lookup(db.upcast());
        let def_map = loc.scope.def_map(db.upcast());

        let lookup = |name: &Name, scope| def_map.resolve_local_item_in_scope(scope, name).ok();

        let parent = if resolve_base_nature {
            data.parent.as_ref().and_then(|parent| lookup(parent, def_map.entry()))
        } else {
            None
        };
        let base_nature = parent.map(|parent| db.nature_info(parent).base_nature);

        let ddt_nature = lookup(&kw::ddt_nature, loc.scope.local_scope).unwrap_or(nature);
        let idt_nature = lookup(&kw::idt_nature, loc.scope.local_scope).unwrap_or(nature);

        // TODO check idt_nature.base_nature == base_nature.idt_nature.base_nature
        // TODO check ddt_nature.base_nature == base_nature.ddt_nature.base_nature

        let lookup = |name: &Name, scope| def_map.resolve_local_item_in_scope(scope, name).ok();
        let abstol = lookup(&kw::abstol, loc.scope.local_scope);

        let units = base_nature.and_then(|nature| db.nature_info(nature).units).or(data.units);

        Arc::new(NatureTy {
            ddt_nature,
            idt_nature,
            base_nature: base_nature.unwrap_or(nature),
            abstol,
            units,
        })
    }

    pub(crate) fn nature_info_recover(
        db: &dyn HirTyDB,
        _cycle: &[String],
        nature: &NatureId,
    ) -> Arc<NatureTy> {
        NatureTy::obtain(db, *nature, false)
    }

    pub fn compatible(db: &dyn HirTyDB, nature1: NatureId, nature2: NatureId) -> bool {
        let nature1_info = db.nature_info(nature1);
        let nature2_info = db.nature_info(nature1);

        nature1_info.units == nature2_info.units
    }
}

pub struct DisciplineTy {
    pub flow: Option<NatureId>,
    pub potential: Option<NatureId>,
}

pub enum DisciplineAccess {
    Potential,
    Flow,
}

impl DisciplineTy {
    pub fn access(&self, nature: NatureId) -> Option<DisciplineAccess> {
        if self.flow.map_or(false, |flow| flow == nature) {
            Some(DisciplineAccess::Flow)
        } else if self.potential.map_or(false, |potential| potential == nature) {
            Some(DisciplineAccess::Potential)
        } else {
            None
        }
    }
}

pub struct BranchTy {
    pub discipline: DisciplineId,
}
impl BranchTy {}
