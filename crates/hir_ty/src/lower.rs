use std::sync::Arc;

use crate::db::HirTyDB;
use hir_def::{name::kw, Lookup, Name, NatureAttrId, NatureId};

#[derive(Debug,PartialEq, Eq,Clone, Copy)]
pub struct NatureTy {
    pub ddt_nature: NatureId,
    pub idt_nature: NatureId,
    pub base_nature: NatureId,
    pub abstol: Option<NatureAttrId>,
    pub units: Option<NatureAttrId>,
}

impl NatureTy {

    pub fn nature_info_query(
        db: &dyn HirTyDB,
        nature: NatureId,
    ) -> Arc<NatureTy> {
        NatureTy::obtain(db, nature, true)
    }
    pub fn obtain(
        db: &dyn HirTyDB,
        nature: NatureId,
        resolve_base_nature: bool,
    ) -> Arc<NatureTy> {
        let data = db.nature_data(nature);
        let loc = nature.lookup(db.upcast());
        let def_map = loc.scope.def_map(db.upcast());

        let lookup = |name: &Name, scope| def_map.resolve_local_item_in_scope(scope, name).ok();

        let parent = if resolve_base_nature {
            data.parent.as_ref().and_then(|parent| lookup(parent, def_map.root()))
        } else {
            None
        };
        let base_nature = parent.map(|parent| db.nature_info(parent).base_nature).unwrap_or(nature);

        let ddt_nature = lookup(&kw::ddt_nature, loc.scope.local_scope).unwrap_or(nature);
        let idt_nature = lookup(&kw::idt_nature, loc.scope.local_scope).unwrap_or(nature);

        // TODO check idt_nature.base_nature == base_nature.idt_nature.base_nature
        // TODO check ddt_nature.base_nature == base_nature.ddt_nature.base_nature

        let lookup = |name: &Name, scope| def_map.resolve_local_item_in_scope(scope, name).ok();
        let abstol = lookup(&kw::abstol, loc.scope.local_scope);
        let units = lookup(&kw::units, loc.scope.local_scope);

        Arc::new(NatureTy { ddt_nature, idt_nature, base_nature, abstol, units })
    }

    pub(crate) fn nature_info_recover(
        db: &dyn HirTyDB,
        _cycle: &[String],
        nature: &NatureId,
    ) -> Arc<NatureTy> {
        NatureTy::obtain(db, *nature, false)
    }

    // pub fn compatible(
    //     db: &dyn HirTyDB,
    //     nature1: NatureId,
    //     nature2: NatureId,
    // )->bool{
    //     let nature1_info = db.nature_info(nature1);
    //     let nature2_info = db.nature_info(nature1);



    // }
}
