// mod allowed_options;
mod ast_id_map;
mod attrs;
pub mod body;
pub mod db;
pub mod expr;
mod item_tree;
pub mod name;
pub mod nameres;
mod path;
mod types;

mod builtin;

mod data;
#[cfg(test)]
mod tests;

use basedb::FileId;
use nameres::{
    DefMap, DefMapSource, FunctionArgPos, LocalNodeId, PathResolveError, ScopeDefItemKind,
};
use stdx::impl_from;
use syntax::{ast::BlockStmt, AstNode};

use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{
    db::HirDefDB,
    item_tree::{
        Branch, Discipline, DisciplineAttr, Function, ItemTreeId, ItemTreeNode, Module, Nature,
        NatureAttr, Param, Var,
    },
    nameres::ScopeDefItem,
};

pub use crate::{
    ast_id_map::{AstIdMap, FileAstId},
    builtin::BuiltIn,
    expr::{Case, Expr, ExprId, Literal, Stmt, StmtId},
    item_tree::ItemTree,
    name::Name,
    path::Path,
    types::{FunctionSignature, Type},
};

pub use basedb::impl_intern_key;

trait Intern {
    type ID;
    fn intern(self, db: &dyn db::HirDefDB) -> Self::ID;
}

pub trait Lookup {
    type Data;
    fn lookup(&self, db: &dyn db::HirDefDB) -> Self::Data;
}

pub enum DefMapKind {
    Block(BlockId),
    Function(FunctionId),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ScopeId {
    pub root_file: FileId,
    pub local_scope: nameres::LocalScopeId,
    pub src: DefMapSource,
}
impl ScopeId {
    pub fn def_map(&self, db: &dyn HirDefDB) -> Arc<DefMap> {
        match self.src {
            DefMapSource::Block(block) => db.block_def_map(block).unwrap(),
            DefMapSource::Function(fun) => db.function_def_map(fun),
            DefMapSource::Root => db.def_map(self.root_file),
        }
    }

    pub fn resolve_path(
        &self,
        db: &dyn HirDefDB,
        path: &Path,
    ) -> Result<ScopeDefItem, PathResolveError> {
        match self.src {
            DefMapSource::Block(_) if path.is_root_path => {
                db.def_map(self.root_file).resolve_root_path(&path.segments, db)
            }
            DefMapSource::Function(_) | DefMapSource::Root if path.is_root_path => {
                self.def_map(db).resolve_root_path(&path.segments, db)
            }

            _ => {
                self.def_map(db).resolve_normal_path_in_scope(self.local_scope, &path.segments, db)
            }
        }
    }

    pub fn resolve_item_path<T: ScopeDefItemKind>(
        &self,
        db: &dyn HirDefDB,
        path: &Path,
    ) -> Result<T, PathResolveError> {
        match self.src {
            DefMapSource::Block(_) if path.is_root_path => {
                db.def_map(self.root_file).resolve_root_item_path(&path.segments, db)
            }
            DefMapSource::Function(_) | DefMapSource::Root if path.is_root_path => {
                self.def_map(db).resolve_root_item_path(&path.segments, db)
            }

            _ => self.def_map(db).resolve_normal_item_path_in_scope(
                self.local_scope,
                &path.segments,
                db,
            ),
        }
    }
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
    pub fn source(&self, db: &dyn HirDefDB) -> N::Source {
        let ast_id = N::lookup(&self.item_tree(db), self.id).ast_id();
        db.ast_id_map(self.scope.root_file)
            .get(ast_id)
            .to_node(db.parse(self.scope.root_file).tree().syntax())
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(salsa::InternId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockLoc {
    ast: FileAstId<BlockStmt>,
    parent: ScopeId,
}

impl_intern_key!(BlockId);

impl Intern for BlockLoc {
    type ID = BlockId;
    fn intern(self, db: &dyn db::HirDefDB) -> BlockId {
        db.intern_block(self)
    }
}

impl Lookup for BlockId {
    type Data = BlockLoc;
    fn lookup(&self, db: &dyn db::HirDefDB) -> BlockLoc {
        db.lookup_intern_block(*self)
    }
}

impl_intern!(Module, ModuleId, ModuleLoc, intern_module, lookup_intern_module);
// impl_intern!(BlockScope, BlockId, BlockLoc, intern_block, lookup_intern_block);
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
            _ => return Err(()),
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct NodeLoc {
    pub scope: ScopeId,
    pub id: LocalNodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(salsa::InternId);

impl_intern_key!(NodeId);
impl Intern for NodeLoc {
    type ID = NodeId;

    fn intern(self, db: &dyn db::HirDefDB) -> Self::ID {
        db.intern_node(self)
    }
}
impl Lookup for NodeId {
    type Data = NodeLoc;

    fn lookup(&self, db: &dyn db::HirDefDB) -> Self::Data {
        db.lookup_intern_node(*self)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct FunctionArgLoc {
    fun: FunctionId,
    arg_pos: FunctionArgPos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionArgId(salsa::InternId);

impl_intern_key!(FunctionArgId);
impl Intern for FunctionArgLoc {
    type ID = FunctionArgId;

    fn intern(self, db: &dyn db::HirDefDB) -> Self::ID {
        db.intern_function_arg(self)
    }
}
impl Lookup for FunctionArgId {
    type Data = FunctionArgLoc;

    fn lookup(&self, db: &dyn db::HirDefDB) -> Self::Data {
        db.lookup_intern_function_arg(*self)
    }
}
