mod allowed_options;
mod ast_id_map;
mod body;
mod builtin;
pub mod db;
mod expr;
mod item_tree;
mod name;
mod nameres;
mod path;
mod types;

mod attrs;
#[cfg(test)]
mod tests;

use arena::Idx;
use basedb::{lints::ErasedItemTreeId, FileId};
pub use path::Path;
use stdx::{impl_from, impl_from_typed};

use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use db::HirDefDB;
pub use expr::{Case, Expr, ExprId, Literal, Stmt, StmtId};
use item_tree::{
    BlockScope, Branch, Discipline, DisciplineAttr, Function, ItemTree, ItemTreeId, ItemTreeNode,
    Module, Nature, NatureAttr, Net, Param, Port, Var,
};
pub use name::Name;
use nameres::ScopeDefItem;
pub use types::Type;

pub use ast_id_map::{AstIdMap, FileAstId};
pub use basedb::impl_intern_key;

trait Intern {
    type ID;
    fn intern(self, db: &dyn db::HirDefDB) -> Self::ID;
}

pub trait Lookup {
    type Data;
    fn lookup(&self, db: &dyn db::HirDefDB) -> Self::Data;
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Hash)]
pub struct ScopeId {
    pub root_file: FileId,
    pub local_scope: nameres::LocalScopeId,
}

#[derive(Debug)]
pub struct ItemLoc<N: ItemTreeNode> {
    pub scope: ScopeId,
    pub id: ItemTreeId<N>,
}

impl<N: ItemTreeNode> ItemLoc<N> {
    pub fn item_tree(&self, db: &dyn HirDefDB) -> Arc<ItemTree> {
        db.item_tree(self.scope.root_file)
    }

    pub fn erased_id(&self, db: &dyn HirDefDB) -> ErasedItemTreeId {
        N::lookup(&self.item_tree(db), self.id).erased_id()
    }
}

impl<N: ItemTreeNode> Clone for ItemLoc<N> {
    fn clone(&self) -> Self {
        Self { scope: self.scope, id: self.id }
    }
}

impl<N: ItemTreeNode> Copy for ItemLoc<N> {}

impl<N: ItemTreeNode> PartialEq for ItemLoc<N> {
    fn eq(&self, other: &Self) -> bool {
        self.scope == other.scope && self.id == other.id
    }
}

impl<N: ItemTreeNode> Eq for ItemLoc<N> {}

impl<N: ItemTreeNode> Hash for ItemLoc<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.scope.hash(state);
        self.id.hash(state);
    }
}

macro_rules! impl_intern {
    ($ty: ident, $id:ident, $loc:ident, $intern:ident, $lookup:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $id(salsa::InternId);
        pub type $loc = ItemLoc<$ty>;
        impl_intern_key!($id);

        impl Intern for $loc {
            type ID = $id;
            fn intern(self, db: &dyn db::HirDefDB) -> $id {
                db.$intern(self)
            }
        }

        impl Lookup for $id {
            type Data = $loc;
            fn lookup(&self, db: &dyn db::HirDefDB) -> $loc {
                db.$lookup(*self)
            }
        }
    };
}

impl_intern!(Module, ModuleId, ModuleLoc, intern_module, lookup_intern_module);
impl_intern!(BlockScope, BlockId, BlockLoc, intern_block, lookup_intern_block);
impl_intern!(Nature, NatureId, NatureLoc, intern_nature, lookup_intern_nature);
impl_intern!(Discipline, DisciplineId, DisciplineLoc, intern_discipline, lookup_intern_discipline);
// impl_intern!(Port, PortId, PortLoc, intern_port, lookup_intern_port);
// impl_intern!(Net, NetId, NetLoc, intern_net, lookup_intern_net);
impl_intern!(Branch, BranchId, BranchLoc, intern_branch, lookup_intern_branch);
impl_intern!(Var, VarId, VarLoc, intern_var, lookup_intern_var);
impl_intern!(Param, ParamId, ParamLoc, intern_param, lookup_intern_param);
impl_intern!(Function, FunctionId, FunctionLoc, intern_function, lookup_intern_function);
impl_intern!(
    NatureAttr,
    NatureAttrId,
    NatureAttrLoc,
    intern_nature_attr,
    lookup_intern_nature_attr
);
impl_intern!(
    DisciplineAttr,
    DisciplineAttrId,
    DisciplineAttrLoc,
    intern_discipline_attr,
    lookup_intern_discipline_attr
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefWithExprId {
    VarId(VarId),
    NatureAttrId(NatureAttrId),
    DisciplineAttrId(DisciplineAttrId),
}

impl_from!(VarId, NatureAttrId,DisciplineAttrId for DefWithExprId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefWithBodyId {
    ParamId(ParamId),
    ModuleId(ModuleId),
    FunctionId(FunctionId),
    VarId(VarId),
    NatureAttrId(NatureAttrId),
    DisciplineAttrId(DisciplineAttrId),
}

impl_from!(ParamId, ModuleId,FunctionId,VarId,NatureAttrId,DisciplineAttrId for DefWithBodyId);
impl TryFrom<ScopeDefItem> for DefWithBodyId {
    type Error = ();
    fn try_from(src: ScopeDefItem) -> Result<DefWithBodyId, ()> {
        let res = match src {
            ScopeDefItem::ModuleId(module) => module.into(),
            ScopeDefItem::VarId(var) => var.into(),
            ScopeDefItem::ParamId(param) => param.into(),
            ScopeDefItem::FunctionId(fun) => fun.into(),
            ScopeDefItem::NatureAttrId(attr) => attr.into(),
            ScopeDefItem::DisciplineAttrId(attr) => attr.into(),
            _ => return Err(())
        };
        Ok(res)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefWithBehaviourId {
    ModuleId(ModuleId),
    FunctionId(FunctionId),
}

impl_from!( ModuleId,FunctionId for DefWithBehaviourId);

impl TryFrom<ScopeDefItem> for DefWithExprId {
    type Error = ();

    fn try_from(item: ScopeDefItem) -> Result<Self, Self::Error> {
        match item {
            ScopeDefItem::VarId(var) => Ok(var.into()),
            ScopeDefItem::NatureAttrId(attr) => Ok(attr.into()),
            ScopeDefItem::DisciplineAttrId(attr) => Ok(attr.into()),
            _ => Err(()),
        }
    }
}

impl TryFrom<ScopeDefItem> for DefWithBehaviourId {
    type Error = ();

    fn try_from(item: ScopeDefItem) -> Result<Self, Self::Error> {
        match item {
            ScopeDefItem::ModuleId(module) => Ok(module.into()),
            ScopeDefItem::FunctionId(fun) => Ok(fun.into()),
            _ => Err(()),
        }
    }
}

pub type NodeId = Idx<Node>;

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum NodeTypeDecl {
    Net(ItemTreeId<Net>),
    Port(ItemTreeId<Port>),
}

impl_from_typed!(
    Net(ItemTreeId<Net>),
    Port(ItemTreeId<Port>) for NodeTypeDecl
);

impl NodeTypeDecl {
    pub fn name<'a>(&self, tree: &'a ItemTree) -> &'a Name {
        match *self {
            NodeTypeDecl::Net(net) => &tree[net].name,
            NodeTypeDecl::Port(port) => &tree[port].name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    port: Option<ItemTreeId<Port>>,
    discipline: Option<NodeTypeDecl>,
    gnd_declaration: Option<NodeTypeDecl>,
}

impl Node {
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
