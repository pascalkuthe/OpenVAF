use std::sync::Arc;

use crate::{
    db::HirDefDB, item_tree::Domain, DisciplineId, Lookup, Name, NatureId, NodeId, ParamId, Type,
    VarId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisciplineData {
    pub name: Name,

    pub potential: Option<Name>,
    pub flow: Option<Name>,
    pub domain: Option<Domain>,
}

impl DisciplineData {
    pub fn discipline_data_query(db: &dyn HirDefDB, id: DisciplineId) -> Arc<DisciplineData> {
        let loc = id.lookup(db);
        let discipline = &loc.item_tree(db)[loc.id];

        Arc::new(DisciplineData {
            name: discipline.name.clone(),
            potential: discipline.potential.clone(),
            flow: discipline.flow.clone(),
            domain: discipline.domain,
        })
    }

    pub fn compatible(&self, other: &DisciplineData) -> bool {
        if self.domain.is_none() || other.domain.is_none() {
            return true;
        }

        if self.potential.is_none() && self.flow.is_some()
            || other.potential.is_none() && other.flow.is_none()
        {
            return self.domain == other.domain;
        }

        self.potential == other.potential && self.flow == other.flow && self.domain == other.domain
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatureData {
    pub name: Name,
    pub parent: Option<Name>,
    pub units: Option<String>,
}

impl NatureData {
    pub fn nature_data_query(db: &dyn HirDefDB, id: NatureId) -> Arc<NatureData> {
        let loc = id.lookup(db);
        let nature = &loc.item_tree(db)[loc.id];

        Arc::new(NatureData {
            name: nature.name.clone(),
            parent: nature.parent.clone(),
            units: nature.units.clone(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarData {
    pub name: Name,
    pub ty: Type,
}

impl VarData {
    pub fn var_data_query(db: &dyn HirDefDB, id: VarId) -> Arc<VarData> {
        let loc = id.lookup(db);
        let var = &loc.item_tree(db)[loc.id];
        Arc::new(VarData { name: var.name.clone(), ty: var.ty.clone() })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamData {
    pub name: Name,
    pub ty: Type,
}

impl ParamData {
    pub fn param_data_query(db: &dyn HirDefDB, id: ParamId) -> Arc<ParamData> {
        let loc = id.lookup(db);
        let var = &loc.item_tree(db)[loc.id];
        Arc::new(ParamData { name: var.name.clone(), ty: var.ty.clone() })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeData {
    discipline: Option<Name>,
    is_input: bool,
    is_output: bool,
    is_gnd: bool,
}

impl NodeData {
    pub fn node_data_query(db: &dyn HirDefDB, id: NodeId) -> Arc<NodeData> {
        let loc = id.lookup(db);
        let def_map = loc.scope.def_map(db);
        let node = def_map.node(loc.scope.local_scope, loc.id);
        let tree = db.item_tree(loc.scope.root_file);
        Arc::new(NodeData {
            discipline: node.discipline(&tree).cloned(),
            is_input: node.is_input(&tree),
            is_output: node.is_output(&tree),
            is_gnd: node.is_gnd(),
        })
    }
}

/*impl Node {
    pub fn discipline<'a>(&self, tree: &'a ItemTree) -> Option<&'a Name> {
        self.discipline.map(|d| d.name(tree))
    }

    pub fn is_input(&self, tree: &ItemTree) -> bool {
        self.port.map_or(false, |port| tree[port].is_input)
    }

    pub fn is_output(&self, tree: &ItemTree) -> bool {
        self.port.map_or(false, |port| tree[port].is_output)
    }

    pub fn is_port(&self) -> bool {
        self.port.is_some()
    }

    pub fn is_gnd(&self) -> bool {
        self.gnd_declaration.is_some()
    }
}
*/
