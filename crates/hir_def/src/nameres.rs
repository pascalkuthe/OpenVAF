use std::ops::{Index, IndexMut};
use std::sync::Arc;

use crate::item_tree::ItemTree;
use crate::name::AsName;
use crate::{
    builtin::BuiltinFunction, db::HirDefDB, BlockId, BranchId, DisciplineAttrId, DisciplineId,
    FunctionId, Lookup, ModuleId, NatureAttrId, NatureId, NodeId, ParamId, VarId,
};
use crate::{AstIdMap, Name, Node};
use basedb::FileId;
use data_structures::{
    arena::{Arena, Idx},
    HashMap,
};
use derive_more::{From, Into, TryInto, Unwrap};
use syntax::{ast, AstPtr};

mod collect;
mod diagnostics;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefMap {
    scopes: Arena<Scope>,
    nodes: Arena<Node>,
}

impl Index<ScopeId> for DefMap {
    type Output = Scope;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

impl IndexMut<ScopeId> for DefMap {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index]
    }
}

impl DefMap {
    pub fn root(&self) -> ScopeId {
        ScopeId::from_raw(0u32)
    }
}

#[derive(Debug, Hash, Clone, Copy, From, PartialEq, Eq)]
pub enum AccessFunction {
    Flow,
    Potential,
}

#[derive(Debug, Hash, Clone, Copy, From, Into, PartialEq, Eq)]
pub struct NatureAccess(pub NatureId);

#[derive(Debug, Hash, Clone, Copy, From, TryInto, PartialEq, Eq)]
pub enum ScopeDefItem {
    Module(ModuleId),
    Block(BlockId),
    Nature(NatureId),
    NatureAccess(NatureAccess),
    Discipline(DisciplineId),
    Node(NodeId),
    VarId(VarId),
    ParamId(ParamId),
    Branch(BranchId),
    Function(FunctionId),
    NatureAttr(NatureAttrId),
    DisciplineAttr(DisciplineAttrId),
    BuiltInFunction(BuiltinFunction),
}

impl ScopeDefItem {
    pub const fn item_kind(&self) -> &'static str {
        match self {
            ScopeDefItem::Module(_) => ModuleId::NAME,
            ScopeDefItem::Block(_) => BlockId::NAME,
            ScopeDefItem::Nature(_) => NatureId::NAME,
            ScopeDefItem::NatureAccess(_) => NatureAccess::NAME,
            ScopeDefItem::Discipline(_) => DisciplineId::NAME,
            ScopeDefItem::Node(_) => NodeId::NAME,
            ScopeDefItem::VarId(_) => VarId::NAME,
            ScopeDefItem::ParamId(_) => ParamId::NAME,
            ScopeDefItem::Branch(_) => BranchId::NAME,
            ScopeDefItem::NatureAttr(_) => NatureAttrId::NAME,
            ScopeDefItem::Function(_) | ScopeDefItem::BuiltInFunction(_) => FunctionId::NAME,
            ScopeDefItem::DisciplineAttr(_) => DisciplineAttrId::NAME,
        }
    }
}

pub trait ScopeDefItemKind: TryFrom<ScopeDefItem> {
    const NAME: &'static str;
}

macro_rules! scope_item_kinds {
    ($($ty: ident => $name:literal),*) => {
        $(impl ScopeDefItemKind for $ty{const NAME: &'static str = $name;})*

    };
}

scope_item_kinds! {
    ModuleId => "module",
    BlockId => "block scope",
    NatureId => "nature",
    DisciplineId => "discipline",
    NatureAccess => "nature access function",
    NodeId => "node",
    VarId => "variable",
    ParamId => "parameter",
    BranchId => "branch",
    FunctionId => "function",
    BuiltinFunction => "function",
    NatureAttrId => "nature attribute",
    DisciplineAttrId => "discipline attribute"
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, Unwrap)]
pub enum ScopeOrigin {
    Root,
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
    BlockScope(BlockId),
}
pub type ScopeId = Idx<Scope>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    origin: ScopeOrigin,
    parent: Option<ScopeId>,
    pub children: HashMap<Name, ScopeId>,
    duplicate_children: Vec<ScopeId>,
    /// All Items visible in this scope (superset of declarations)
    pub visible_items: HashMap<Name, ScopeDefItem>,
    /// Items declared in this scopes
    pub declarations: HashMap<Name, ScopeDefItem>,
}

impl DefMap {
    pub fn def_map_query(db: &dyn HirDefDB, root_file: FileId) -> Arc<DefMap> {
        collect::collect_defs(db, root_file)
    }

    pub fn resolve_name_in_scope(
        &self,
        scope: ScopeId,
        name: &Name,
    ) -> Result<ScopeDefItem, PathResolveError> {
        self.scopes[scope]
            .visible_items
            .get(name)
            .copied()
            .ok_or_else(|| PathResolveError::NotFound { name: name.clone() })
    }

    pub fn resolve_item_in_scope<T: ScopeDefItemKind>(
        &self,
        scope: ScopeId,
        name: &Name,
    ) -> Result<T, PathResolveError> {
        let res = self.resolve_name_in_scope(scope, name)?;
        res.try_into().map_err(|_| PathResolveError::ExpectedItemKind {
            name: name.clone(),
            expected: T::NAME,
            found: res.item_kind(),
        })
    }


    pub fn resolve_path_in_scope(
        &self,
        scope: ScopeId,
        path: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<ScopeDefItem, PathResolveError> {
        let (root, segments, name) = match path {
            [] => unreachable!(),
            [name] => return self.resolve_name_in_scope(scope, name),
            [root, segments @ .., name] => (root, segments, name),
        };

        let root_scope = self.resolve_name_in_scope(scope, root)?;
        let root_scope = match root_scope {
            ScopeDefItem::Module(module) => module.lookup(db).scope,
            ScopeDefItem::Block(block) => block.lookup(db).scope,
            // TODO more items with scope
            _ => {
                return Err(PathResolveError::ExpectedScope {
                    name: root.clone(),
                    found: root_scope,
                })
            }
        };

        let scope = segments.iter().try_fold(root_scope, |scope, segment| {
            match self.scopes[scope].children.get(segment) {
                Some(scope) => Ok(*scope),
                None => {
                    if let Some(found) = self.scopes[scope].declarations.get(segment) {
                        Err(PathResolveError::ExpectedScope {
                            name: segment.clone(),
                            found: *found,
                        })
                    } else {
                        Err(PathResolveError::NotFoundIn { name: segment.clone(), scope })
                    }
                }
            }
        })?;

        self.scopes[scope]
            .declarations
            .get(name)
            .copied()
            .ok_or_else(|| PathResolveError::NotFoundIn { name: name.clone(), scope })
    }

    pub(crate) fn block_scope(
        &self,
        parent: ScopeId,
        block: &ast::BlockStmt,
        ast_ptr: &AstPtr<ast::BlockStmt>,
        ast_id_map: &AstIdMap,
        tree: &ItemTree,
        db: &dyn HirDefDB,
    ) -> Option<ScopeId> {
        let name = block.block_scope()?.name()?.as_name();
        let scope = *self.scopes[parent].children.get(&name)?;
        // At this point we already have the scope
        // However if another blocks with the same name was declared then this scope is incorrect
        // So the ASTs need to be compared
        let item_tree = self.scopes[scope].origin.unwrap_block_scope().lookup(db).item_tree;
        if &ast_id_map.get(tree[item_tree].ast_id) == ast_ptr {
            Some(scope)
        } else {
            self.scopes[parent]
                .duplicate_children
                .iter()
                .find(|scope| {
                    if let ScopeOrigin::BlockScope(block) = self.scopes[**scope].origin {
                        let item_tree = block.lookup(db).item_tree;
                        &ast_id_map.get(tree[item_tree].ast_id) == ast_ptr
                    } else {
                        false
                    }
                })
                .copied()
        }
    }
}

// TODO better errors / hints?
#[derive(Debug, Clone)]
pub enum PathResolveError {
    NotFound { name: Name },
    NotFoundIn { name: Name, scope: ScopeId },
    ExpectedScope { name: Name, found: ScopeDefItem },
    ExpectedItemKind { name: Name, expected: &'static str, found: &'static str },
}
