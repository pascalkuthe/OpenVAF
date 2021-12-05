use std::sync::Arc;

use arena::Arena;
use syntax::name::Name;
use typed_index_collections::TiSlice;

use crate::db::HirDefDB;
use crate::item_tree::{self, BranchKind, DisciplineAttrKind, Domain, NatureRef};
use crate::{
    BranchId, DisciplineId, FunctionId, ItemTree, LocalFunctionArgId, LocalNatureAttrId, Lookup,
    NatureId, NodeId, ParamId, Type, VarId,
};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct DisciplineAttrData {
    pub name: Name,
    pub kind: DisciplineAttrKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisciplineData {
    pub name: Name,
    pub potential: Option<NatureRef>,
    pub flow: Option<NatureRef>,
    pub domain: Option<Domain>,
    pub attrs: Arena<DisciplineAttrData>,
}

impl DisciplineData {
    pub fn discipline_data_query(db: &dyn HirDefDB, id: DisciplineId) -> Arc<DisciplineData> {
        let loc = id.lookup(db);
        let tree = &loc.item_tree(db);
        let discipline = &tree[loc.id];
        let attrs: Vec<_> = discipline
            .extra_attrs
            .clone()
            .map(|attr| {
                let attr = &tree[attr];
                DisciplineAttrData { name: attr.name.clone(), kind: attr.kind }
            })
            .collect();

        Arc::new(DisciplineData {
            name: discipline.name.clone(),
            potential: discipline.potential.clone().map(|(pot, _)| pot),
            flow: discipline.flow.clone().map(|(flow, _)| flow),
            domain: discipline.domain.map(|(domain, _)| domain),
            attrs: Arena::from(attrs),
        })
    }

    pub fn compatible(&self, other: &DisciplineData) -> bool {
        if self.domain.is_none() || other.domain.is_none() {
            return true;
        }

        if self.potential.is_none() && self.flow.is_none()
            || other.potential.is_none() && other.flow.is_none()
        {
            return self.domain == other.domain;
        }

        self.potential == other.potential && self.flow == other.flow && self.domain == other.domain
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct NatureAttrData {
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatureData {
    pub name: Name,
    pub parent: Option<NatureRef>,
    pub idt_nature: Option<NatureRef>,
    pub ddt_nature: Option<NatureRef>,
    pub units: Option<String>,
    pub abstol: Option<LocalNatureAttrId>,
    pub attrs: Arena<NatureAttrData>,
}

impl NatureData {
    pub fn nature_data_query(db: &dyn HirDefDB, id: NatureId) -> Arc<NatureData> {
        let loc = id.lookup(db);
        let tree = db.item_tree(loc.root_file);
        let nature = &tree[loc.id];
        let attrs: Vec<_> = nature
            .attrs
            .clone()
            .map(|attr| NatureAttrData { name: tree[attr].name.clone() })
            .collect();

        Arc::new(NatureData {
            name: nature.name.clone(),
            parent: nature.parent.clone(),
            units: nature.units.clone().map(|(it, _)| it),
            idt_nature: nature.idt_nature.clone().map(|(it, _)| it),
            ddt_nature: nature.ddt_nature.clone().map(|(it, _)| it),
            abstol: nature.abstol,
            attrs: Arena::from(attrs),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarData {
    // pub name: Name,
    pub ty: Type,
}

impl VarData {
    pub fn var_data_query(db: &dyn HirDefDB, id: VarId) -> Arc<VarData> {
        let loc = id.lookup(db);
        let var = &loc.item_tree(db)[loc.id];
        Arc::new(VarData { ty: var.ty.clone() })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamData {
    // pub name: Name,
    pub ty: Type,
}

impl ParamData {
    pub fn param_data_query(db: &dyn HirDefDB, id: ParamId) -> Arc<ParamData> {
        let loc = id.lookup(db);
        let param = &loc.item_tree(db)[loc.id];
        Arc::new(ParamData { ty: param.ty.clone() })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeData {
    pub discipline: Option<Name>,
    pub is_input: bool,
    pub is_output: bool,
    pub is_gnd: bool,
}

impl NodeData {
    pub fn node_data_query(db: &dyn HirDefDB, id: NodeId) -> Arc<NodeData> {
        let loc = id.lookup(db);
        let module = loc.module.lookup(db);
        let tree = module.item_tree(db);
        let node = &tree[module.id].nodes[loc.id];
        let (is_input, is_output) = node.direction(&tree);

        Arc::new(NodeData {
            discipline: node.discipline(&tree),
            is_input,
            is_output,
            is_gnd: node.is_gnd(&tree),
        })
    }

    #[inline]
    pub fn is_port(&self) -> bool {
        self.is_input | self.is_output
    }

    #[inline]
    pub fn read_only(&self) -> bool {
        self.is_input & !self.is_output
    }

    #[inline]
    pub fn write_only(&self) -> bool {
        self.is_output & !self.is_input
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BranchData {
    // pub name: Name,
    pub kind: BranchKind,
}

impl BranchData {
    pub fn branch_data_query(db: &dyn HirDefDB, id: BranchId) -> Arc<BranchData> {
        let loc = id.lookup(db);
        let branch = &loc.item_tree(db)[loc.id];
        Arc::new(BranchData { kind: branch.kind.clone() })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionArg {
    pub ty: Type,
    pub is_input: bool,
    pub is_output: bool,
}

impl FunctionArg {
    fn new(arg: &item_tree::FunctionArg, tree: &ItemTree) -> FunctionArg {
        FunctionArg { ty: arg.ty(tree), is_input: arg.is_input, is_output: arg.is_output }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionData {
    pub return_ty: Type,
    pub args: Box<TiSlice<LocalFunctionArgId, FunctionArg>>,
}

impl FunctionData {
    pub fn function_data_query(db: &dyn HirDefDB, id: FunctionId) -> Arc<FunctionData> {
        let loc = id.lookup(db);
        let item_tree = loc.item_tree(db);
        let args =
            item_tree[loc.id].args.iter().map(|arg| FunctionArg::new(arg, &item_tree)).collect();
        Arc::new(FunctionData { return_ty: item_tree[loc.id].ty.clone(), args })
    }
}
