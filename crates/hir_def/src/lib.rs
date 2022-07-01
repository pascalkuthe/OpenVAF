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
use stdx::{impl_debug_display, impl_from};
use syntax::ast::{self, BlockStmt};
use syntax::name::Name;
use syntax::{AstNode, AstPtr};

pub use crate::builtin::{BuiltIn, ParamSysFun};
pub use crate::data::FunctionArg;
use crate::db::HirDefDB;
pub use crate::expr::{Case, Expr, ExprId, Literal, Stmt, StmtId};
use crate::item_tree::{
    AliasParam, Branch, Discipline, Function, ItemTreeId, Module, Nature, Param, Var,
};
pub use crate::item_tree::{
    BranchKind, DisciplineAttr, ItemTree, ItemTreeNode, NatureAttr, NatureRef, NatureRefKind,
    NodeTypeDecl,
};
use crate::nameres::ScopeDefItem;
pub use crate::path::Path;
pub use crate::types::Type;

impl ParamSysFun {
    pub fn default_value(self) -> f64 {
        match self {
            ParamSysFun::vflip | ParamSysFun::hflip | ParamSysFun::mfactor => 1f64,
            ParamSysFun::xposition | ParamSysFun::yposition | ParamSysFun::angle => 0f64,
        }
    }
}

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

    pub fn ast_ptr(&self, db: &dyn HirDefDB) -> AstPtr<N::Source> {
        let ast_id = self.ast_id(db);
        db.ast_id_map(self.scope.root_file).get(ast_id)
    }

    pub fn name(&self, db: &dyn HirDefDB) -> Name {
        N::lookup(&self.item_tree(db), self.id).name().clone()
    }

    pub fn source(&self, db: &dyn HirDefDB) -> N::Source {
        self.ast_ptr(db).to_node(db.parse(self.scope.root_file).tree().syntax())
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
    pub fn item_tree(self, db: &dyn HirDefDB) -> Arc<ItemTree> {
        db.item_tree(self.root_file)
    }

    pub fn ast_id(self, db: &dyn HirDefDB) -> AstId<ast::DisciplineDecl> {
        self.item_tree(db)[self.id].ast_id
    }

    pub fn source(self, db: &dyn HirDefDB) -> ast::DisciplineDecl {
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
    pub fn item_tree(self, db: &dyn HirDefDB) -> Arc<ItemTree> {
        db.item_tree(self.root_file)
    }

    pub fn ast_id(self, db: &dyn HirDefDB) -> AstId<ast::NatureDecl> {
        db.item_tree(self.root_file)[self.id].ast_id
    }

    pub fn source(self, db: &dyn HirDefDB) -> ast::NatureDecl {
        let ast_id = self.ast_id(db);
        db.ast_id_map(self.root_file).get(ast_id).to_node(db.parse(self.root_file).tree().syntax())
    }
}

impl_intern!(NatureId, NatureLoc, intern_nature, lookup_intern_nature);

pub type BranchLoc = ItemLoc<Branch>;
impl_intern!(BranchId, BranchLoc, intern_branch, lookup_intern_branch);

pub type VarLoc = ItemLoc<Var>;
impl_intern!(VarId, VarLoc, intern_var, lookup_intern_var);

impl From<u32> for VarId {
    fn from(val: u32) -> Self {
        VarId(val.into())
    }
}

impl From<VarId> for u32 {
    fn from(var: VarId) -> Self {
        var.0.into()
    }
}

pub type ParamLoc = ItemLoc<Param>;
impl_intern!(ParamId, ParamLoc, intern_param, lookup_intern_param);

pub type AliasParamLoc = ItemLoc<AliasParam>;
impl_intern!(AliasParamId, AliasParamLoc, intern_alias_param, lookup_intern_alias_param);

pub type FunctionLoc = ItemLoc<Function>;
impl_intern!(FunctionId, FunctionLoc, intern_function, lookup_intern_function);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct NodeLoc {
    pub module: ModuleId,
    pub id: LocalNodeId,
}

pub type LocalNodeId = Idx<Node>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(salsa::InternId);

impl_debug_display!(match NodeId{ NodeId(id) => "node{:?}", id;});

impl_intern_key!(NodeId);

impl Intern for NodeLoc {
    type ID = NodeId;
    fn intern(self, db: &dyn db::HirDefDB) -> NodeId {
        db.intern_node(self)
    }
}

impl Lookup for NodeId {
    type Data = NodeLoc;
    fn lookup(&self, db: &dyn db::HirDefDB) -> NodeLoc {
        db.lookup_intern_node(*self)
    }
}

impl NodeLoc {
    pub fn ast_id(self, db: &dyn HirDefDB) -> ErasedAstId {
        let loc = self.module.lookup(db);
        loc.item_tree(db)[loc.id].nodes[self.id].ast_id
    }

    pub fn ast_ptr(self, db: &dyn HirDefDB) -> ErasedAstId {
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
    pub fn ast_id(self, db: &dyn HirDefDB) -> AstId<ast::FunctionArg> {
        let fun = self.fun.lookup(db);
        fun.item_tree(db)[fun.id].args[self.id].ast_ids[0]
    }

    pub fn syntax(self, db: &dyn HirDefDB) -> ast::FunctionArg {
        let file = self.fun.lookup(db).scope.root_file;
        let ptr = db.ast_id_map(file).get(self.ast_id(db));
        ptr.to_node(db.parse(file).tree().syntax())
    }

    pub fn ast_ptr(self, db: &dyn HirDefDB) -> AstPtr<ast::FunctionArg> {
        db.ast_id_map(self.fun.lookup(db).scope.root_file).get(self.ast_id(db))
    }

    pub fn name(self, db: &dyn HirDefDB) -> Name {
        let fun = self.fun.lookup(db);
        fun.item_tree(db)[fun.id].args[self.id].name.clone()
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
    pub fn ast_id(self, db: &dyn HirDefDB) -> AstId<ast::NatureAttr> {
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
pub enum DefWithBodyId {
    ParamId(ParamId),
    ModuleId(ModuleId),
    FunctionId(FunctionId),
    VarId(VarId),
    NatureAttrId(NatureAttrId),
    DisciplineAttrId(DisciplineAttrId),
}

impl DefWithBodyId {
    pub fn file(self, db: &dyn HirDefDB) -> FileId {
        match self {
            DefWithBodyId::ParamId(id) => id.lookup(db).scope.root_file,
            DefWithBodyId::ModuleId(id) => id.lookup(db).scope.root_file,
            DefWithBodyId::FunctionId(id) => id.lookup(db).scope.root_file,
            DefWithBodyId::VarId(id) => id.lookup(db).scope.root_file,
            DefWithBodyId::NatureAttrId(id) => id.lookup(db).nature.lookup(db).root_file,
            DefWithBodyId::DisciplineAttrId(id) => id.lookup(db).discipline.lookup(db).root_file,
        }
    }
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
