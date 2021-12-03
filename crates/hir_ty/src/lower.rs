use std::sync::Arc;

use crate::db::HirTyDB;
use hir_def::{
    nameres::{diagnostics::PathResolveError, DefMap},
    BranchId, DisciplineId, Intern, Lookup, NatureAttrId, NatureAttrLoc, NatureId, NatureRef,
    NatureRefKind, NodeId, Path, ScopeId,
};
use syntax::name::{kw, Name};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NatureTy {
    pub ddt_nature: NatureId,
    pub idt_nature: NatureId,
    pub parent: Option<NatureId>,
    pub base_nature: NatureId,
    pub units: Option<String>,
}

impl NatureTy {
    pub fn nature_info_query(db: &dyn HirTyDB, nature: NatureId) -> Arc<NatureTy> {
        NatureTy::obtain(db, nature, true)
    }
    pub fn obtain(db: &dyn HirTyDB, nature: NatureId, resolve_parent: bool) -> Arc<NatureTy> {
        let data = db.nature_data(nature);
        let loc = nature.lookup(db.upcast());
        let def_map = db.def_map(loc.root_file);

        let parent =
            data.parent.as_ref().and_then(|parent| lookup_nature(&def_map, parent, db).ok());

        let parent_info = parent.and_then(|parent| resolve_parent.then(|| db.nature_info(parent)));
        let base_nature = parent_info.as_ref().map(|parent| parent.base_nature);

        let ddt_nature = data
            .ddt_nature
            .as_ref()
            .and_then(|ddt_nature| lookup_nature(&def_map, ddt_nature, db).ok())
            .or_else(|| parent_info.as_ref().map(|parent| parent.ddt_nature))
            .unwrap_or(nature);

        let idt_nature = data
            .idt_nature
            .as_ref()
            .and_then(|idt_nature| lookup_nature(&def_map, idt_nature, db).ok())
            .or_else(|| parent_info.as_ref().map(|parent| parent.idt_nature))
            .unwrap_or(nature);

        // TODO check idt_nature.base_nature == base_nature.idt_nature.base_nature
        // TODO check ddt_nature.base_nature == base_nature.ddt_nature.base_nature

        let units = base_nature
            .and_then(|nature| db.nature_info(nature).units.clone())
            .or_else(|| data.units.clone());

        Arc::new(NatureTy {
            ddt_nature,
            idt_nature,
            parent,
            base_nature: base_nature.unwrap_or(nature),
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
        let nature2_info = db.nature_info(nature2);
        nature1_info.units == nature2_info.units
    }

    pub fn lookup_attr(
        db: &dyn HirTyDB,
        nature: NatureId,
        name: &Name,
    ) -> Result<NatureAttrId, PathResolveError> {
        fn lookup_attr_inner(
            db: &dyn HirTyDB,
            mut nature: NatureId,
            name: &Name,
        ) -> Option<NatureAttrId> {
            loop {
                if let Some((attr, _)) = db
                    .nature_data(nature)
                    .attrs
                    .iter_enumerated()
                    .find(|(_, attr)| &attr.name == name)
                {
                    return Some(NatureAttrLoc { nature, id: attr }.intern(db.upcast()));
                }
                let info = db.nature_info(nature);
                if info.base_nature == nature {
                    return None;
                }
                nature = info.parent?;
            }
        }
        lookup_attr_inner(db, nature, name).ok_or_else(|| PathResolveError::NotFoundIn {
            name: name.clone(),
            scope: db.nature_data(nature).name.clone(),
        })
    }
}

pub fn lookup_nature(
    def_map: &DefMap,
    nature_ref: &NatureRef,
    db: &dyn HirTyDB,
) -> Result<NatureId, PathResolveError> {
    let (nature, attr) = match nature_ref.kind {
        NatureRefKind::Nature => {
            return def_map.resolve_local_item_in_scope(def_map.root(), &nature_ref.name)
        }
        NatureRefKind::DisciplinePotential => {
            let discipline =
                def_map.resolve_local_item_in_scope(def_map.root(), &nature_ref.name)?;
            (db.discipline_info(discipline).potential, kw::potential)
        }
        NatureRefKind::DisciplineFlow => {
            let discipline =
                def_map.resolve_local_item_in_scope(def_map.root(), &nature_ref.name)?;
            (db.discipline_info(discipline).flow, kw::flow)
        }
    };

    nature
        .ok_or_else(|| PathResolveError::NotFoundIn { name: attr, scope: nature_ref.name.clone() })
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DisciplineTy {
    pub flow: Option<NatureId>,
    pub potential: Option<NatureId>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DisciplineAccess {
    Potential,
    Flow,
}

impl DisciplineTy {
    pub fn discipline_info_query(db: &dyn HirTyDB, discipline: DisciplineId) -> Arc<DisciplineTy> {
        let data = db.disipline_data(discipline);
        let def_map = db.def_map(discipline.lookup(db.upcast()).root_file);
        Arc::new(DisciplineTy {
            flow: data.flow.as_ref().and_then(|flow| lookup_nature(&def_map, flow, db).ok()),
            potential: data
                .potential
                .as_ref()
                .and_then(|potential| lookup_nature(&def_map, potential, db).ok()),
        })
    }

    pub fn access(&self, nature: NatureId, db: &dyn HirTyDB) -> Option<DisciplineAccess> {
        if self.flow.map_or(false, |flow| NatureTy::compatible(db, flow, nature)) {
            Some(DisciplineAccess::Flow)
        } else if self
            .potential
            .map_or(false, |potential| NatureTy::compatible(db, potential, nature))
        {
            Some(DisciplineAccess::Potential)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BranchKind {
    PortFlow(NodeId),
    NodeGnd(NodeId),
    Nodes(NodeId, NodeId),
}

impl BranchKind {
    pub fn discipline(&self, db: &dyn HirTyDB, scope: ScopeId) -> Option<DisciplineId> {
        match self {
            // standard dictates that the dicisplines of the two nodes need to be compatible
            // compatible disciplines behave idential during type checking
            // so we just use the discipline of the first node here
            // TODO check that discipline are compatible
            BranchKind::PortFlow(node) | BranchKind::NodeGnd(node) | BranchKind::Nodes(node, _) => {
                let node = db.node_data(*node);
                scope
                    .resolve_item_path(db.upcast(), &Path::new_ident(node.discipline.clone()?))
                    .ok()
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BranchTy {
    pub discipline: DisciplineId,
    pub kind: BranchKind,
}
impl BranchTy {
    pub fn branch_info_query(db: &dyn HirTyDB, branch: BranchId) -> Option<Arc<BranchTy>> {
        let kind = &db.branch_data(branch).kind;
        let scope = branch.lookup(db.upcast()).scope;
        let kind = match kind {
            hir_def::BranchKind::PortFlow(port) => {
                // TODO check that this is actually a port in verification
                BranchKind::PortFlow(scope.resolve_item_path(db.upcast(), port).ok()?)
            }
            hir_def::BranchKind::NodeGnd(node) => {
                BranchKind::NodeGnd(scope.resolve_item_path(db.upcast(), node).ok()?)
            }
            hir_def::BranchKind::Nodes(node1, node2) => {
                let node1 = scope.resolve_item_path(db.upcast(), node1).ok()?;
                let node2 = scope.resolve_item_path(db.upcast(), node2).ok()?;
                BranchKind::Nodes(node1, node2)
            }
            hir_def::BranchKind::Missing => return None,
        };

        Some(Arc::new(BranchTy { discipline: kind.discipline(db, scope)?, kind }))
    }

    pub fn access(&self, nature: NatureId, db: &dyn HirTyDB) -> Option<DisciplineAccess> {
        db.discipline_info(self.discipline).access(nature, db)
    }

    pub fn flow_attr(
        db: &dyn HirTyDB,
        branch: BranchId,
        name: &Name,
    ) -> Option<Result<NatureAttrId, PathResolveError>> {
        let discipline = db.branch_info(branch)?.discipline;
        match db.discipline_info(discipline).flow {
            Some(nature) => Some(NatureTy::lookup_attr(db, nature, name)),
            None => Some(Err(PathResolveError::NotFoundIn {
                name: kw::flow,
                scope: db.disipline_data(discipline).name.clone(),
            })),
        }
    }

    pub fn potential_attr(
        db: &dyn HirTyDB,
        branch: BranchId,
        name: &Name,
    ) -> Option<Result<NatureAttrId, PathResolveError>> {
        let discipline = db.branch_info(branch)?.discipline;
        match db.discipline_info(discipline).potential {
            Some(nature) => Some(NatureTy::lookup_attr(db, nature, name)),
            None => Some(Err(PathResolveError::NotFoundIn {
                name: kw::potential,
                scope: db.disipline_data(discipline).name.clone(),
            })),
        }
    }
}
