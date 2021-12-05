// mod allowed_options;
pub mod body;
pub mod db;
pub mod expr;
mod item_tree;
pub mod nameres;
mod path;
mod types;

mod builtin;

mod data;
#[cfg(test)]
mod tests;

use std::hash::{Hash, Hasher};
use std::sync::Arc;

use arena::Idx;
pub use basedb::impl_intern_key;
use basedb::{AstId, ErasedAstId, FileId};
use item_tree::Node;
use nameres::diagnostics::PathResolveError;
use nameres::{DefMap, DefMapSource, ResolvedPath, ScopeDefItemKind};
use stdx::impl_from;
use syntax::ast::{self, BlockStmt};
use syntax::AstNode;

pub use crate::builtin::BuiltIn;
use crate::db::HirDefDB;
pub use crate::expr::{Case, Expr, ExprId, Literal, Stmt, StmtId};
use crate::item_tree::{
    Branch, Discipline, Function, ItemTreeId, ItemTreeNode, Module, Nature, Param, Var,
};
pub use crate::item_tree::{BranchKind, ItemTree, NatureRef, NatureRefKind};
use crate::nameres::ScopeDefItem;
pub use crate::path::Path;
pub use crate::types::Type;

pub trait Intern {
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
    ) -> Result<ResolvedPath, PathResolveError> {
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

    pub fn root(root_file: FileId) -> ScopeId {
        ScopeId { root_file, local_scope: 0usize.into(), src: DefMapSource::Root }
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
    pub fn ast_id(&self, db: &dyn HirDefDB) -> AstId<N::Source> {
        N::lookup(&self.item_tree(db), self.id).ast_id()
    }

    pub fn source(&self, db: &dyn HirDefDB) -> N::Source {
        let ast_id = self.ast_id(db);
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
    ($id:ident, $loc:ident, $intern:ident, $lookup:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $id(salsa::InternId);

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
pub struct BlockLoc {
    ast: AstId<BlockStmt>,
    parent: ScopeId,
}

impl_intern!(BlockId, BlockLoc, intern_block, lookup_intern_block);

pub type ModuleLoc = ItemLoc<Module>;
impl_intern!(ModuleId, ModuleLoc, intern_module, lookup_intern_module);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DisciplineLoc {
    pub root_file: FileId,
    pub id: ItemTreeId<Discipline>,
}

impl DisciplineLoc {
    pub fn item_tree(&self, db: &dyn HirDefDB) -> Arc<ItemTree> {
        db.item_tree(self.root_file)
    }

    pub fn ast_id(&self, db: &dyn HirDefDB) -> AstId<ast::DisciplineDecl> {
        self.item_tree(db)[self.id].ast_id
    }

    pub fn source(&self, db: &dyn HirDefDB) -> ast::DisciplineDecl {
        let ast_id = self.ast_id(db);
        db.ast_id_map(self.root_file).get(ast_id).to_node(db.parse(self.root_file).tree().syntax())
    }
}

impl_intern!(DisciplineId, DisciplineLoc, intern_discipline, lookup_intern_discipline);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NatureLoc {
    pub root_file: FileId,
    pub id: ItemTreeId<Nature>,
}

impl NatureLoc {
    pub fn item_tree(&self, db: &dyn HirDefDB) -> Arc<ItemTree> {
        db.item_tree(self.root_file)
    }

    pub fn ast_id(&self, db: &dyn HirDefDB) -> AstId<ast::NatureDecl> {
        db.item_tree(self.root_file)[self.id].ast_id
    }

    pub fn source(&self, db: &dyn HirDefDB) -> ast::NatureDecl {
        let ast_id = self.ast_id(db);
        db.ast_id_map(self.root_file).get(ast_id).to_node(db.parse(self.root_file).tree().syntax())
    }
}

impl_intern!(NatureId, NatureLoc, intern_nature, lookup_intern_nature);

pub type BranchLoc = ItemLoc<Branch>;
impl_intern!(BranchId, BranchLoc, intern_branch, lookup_intern_branch);

pub type VarLoc = ItemLoc<Var>;
impl_intern!(VarId, VarLoc, intern_var, lookup_intern_var);

pub type ParamLoc = ItemLoc<Param>;
impl_intern!(ParamId, ParamLoc, intern_param, lookup_intern_param);

pub type FunctionLoc = ItemLoc<Function>;
impl_intern!(FunctionId, FunctionLoc, intern_function, lookup_intern_function);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct NodeLoc {
    pub module: ModuleId,
    pub id: LocalNodeId,
}

pub type LocalNodeId = Idx<Node>;

impl_intern!(NodeId, NodeLoc, intern_node, lookup_intern_node);

impl NodeLoc {
    pub fn ast_id(&self, db: &dyn HirDefDB) -> ErasedAstId {
        let loc = self.module.lookup(db);
        loc.item_tree(db)[loc.id].nodes[self.id].ast_id
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct FunctionArgLoc {
    pub fun: FunctionId,
    pub id: LocalFunctionArgId,
}

impl FunctionArgLoc {
    pub fn ast_id(&self, db: &dyn HirDefDB) -> AstId<ast::FunctionArg> {
        let fun = self.fun.lookup(db);
        fun.item_tree(db)[fun.id].args[self.id].ast_ids[0]
    }
}

pub type LocalFunctionArgId = Idx<item_tree::FunctionArg>;

impl_intern!(FunctionArgId, FunctionArgLoc, intern_function_arg, lookup_intern_function_arg);

pub type LocalDisciplineAttrId = Idx<data::DisciplineAttrData>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct DisciplineAttrLoc {
    pub discipline: DisciplineId,
    pub id: LocalDisciplineAttrId,
}

impl_intern!(
    DisciplineAttrId,
    DisciplineAttrLoc,
    intern_discipline_attr,
    lookup_intern_discipline_attr
);

pub type LocalNatureAttrId = Idx<data::NatureAttrData>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct NatureAttrLoc {
    pub nature: NatureId,
    pub id: LocalNatureAttrId,
}

impl NatureAttrLoc {
    pub fn ast_id(&self, db: &dyn HirDefDB) -> AstId<ast::NatureAttr> {
        let nature = self.nature.lookup(db);
        let item_tree = &nature.item_tree(db);
        let nature = &item_tree[nature.id];
        let id = u32::from(nature.attrs.start()) + u32::from(self.id);
        let id: ItemTreeId<item_tree::NatureAttr> = ItemTreeId::from(id);
        item_tree[id].ast_id()
    }
}

impl_intern!(NatureAttrId, NatureAttrLoc, intern_nature_attr, lookup_intern_nature_attr);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefWithExprId {
    VarId(VarId),
    NatureAttrId(NatureAttrId),
    DisciplineAttrId(DisciplineAttrId),
}

impl DefWithExprId {
    pub fn ty(&self, db: &dyn HirDefDB) -> Option<Type> {
        match self {
            DefWithExprId::VarId(var) => Some(db.var_data(*var).ty.clone()),
            // TODO different types?
            DefWithExprId::NatureAttrId(_) | DefWithExprId::DisciplineAttrId(_) => None,
        }
    }
}

impl DefWithExprId {
    pub fn file(&self, db: &dyn HirDefDB) -> FileId {
        match self {
            DefWithExprId::VarId(var) => var.lookup(db).scope.root_file,
            DefWithExprId::NatureAttrId(attr) => attr.lookup(db).nature.lookup(db).root_file,
            DefWithExprId::DisciplineAttrId(attr) => {
                attr.lookup(db).discipline.lookup(db).root_file
            }
        }
    }
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
            // ScopeDefItem::DisciplineAttrId(attr) => attr.into(),
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

impl DefWithBehaviourId {
    pub fn file(&self, db: &dyn HirDefDB) -> FileId {
        match self {
            DefWithBehaviourId::ModuleId(module) => module.lookup(db).scope.root_file,
            DefWithBehaviourId::FunctionId(function) => function.lookup(db).scope.root_file,
        }
    }
}

impl_from!( ModuleId,FunctionId for DefWithBehaviourId);

impl TryFrom<ScopeDefItem> for DefWithExprId {
    type Error = ();

    fn try_from(item: ScopeDefItem) -> Result<Self, Self::Error> {
        match item {
            ScopeDefItem::VarId(var) => Ok(var.into()),
            // ScopeDefItem::NatureAttrId(attr) => Ok(attr.into()),
            // ScopeDefItem::DisciplineAttrId(attr) => Ok(attr.into()),
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
