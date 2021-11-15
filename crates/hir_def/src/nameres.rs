use std::ops::{Index, IndexMut};
use std::sync::Arc;

use crate::item_tree::ItemTree;
use crate::name::AsName;
use crate::{
    builtin::BuiltInFunction, db::HirDefDB, BlockId, BranchId, DisciplineAttrId, DisciplineId,
    FunctionId, Lookup, ModuleId, NatureAttrId, NatureId, NodeId, ParamId, VarId,
};
use crate::{AstIdMap, Name, Node};
use ahash::AHashMap as HashMap;
use arena::{Arena, Idx};
use basedb::lints::ErasedItemTreeId;
use basedb::FileId;
use stdx::{impl_from, impl_from_typed};
use syntax::{ast, AstPtr};

mod collect;
mod diagnostics;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefMap {
    scopes: Arena<Scope>,
    nodes: Arena<Node>,
}

impl Index<LocalScopeId> for DefMap {
    type Output = Scope;

    fn index(&self, index: LocalScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

impl IndexMut<LocalScopeId> for DefMap {
    fn index_mut(&mut self, index: LocalScopeId) -> &mut Self::Output {
        &mut self.scopes[index]
    }
}

impl DefMap {
    pub fn root(&self) -> LocalScopeId {
        LocalScopeId::from(0u32)
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum AccessFunction {
    Flow,
    Potential,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct NatureAccess(pub NatureId);

impl From<NatureAccess> for NatureId {
    fn from(access: NatureAccess) -> Self {
        access.0
    }
}

impl From<NatureId> for NatureAccess {
    fn from(id: NatureId) -> Self {
        NatureAccess(id)
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum ScopeDefItem {
    ModuleId(ModuleId),
    BlockId(BlockId),
    NatureId(NatureId),
    NatureAccess(NatureAccess),
    DisciplineId(DisciplineId),
    NodeId(NodeId),
    VarId(VarId),
    ParamId(ParamId),
    BranchId(BranchId),
    FunctionId(FunctionId),
    NatureAttrId(NatureAttrId),
    DisciplineAttrId(DisciplineAttrId),
    BuiltInFunction(BuiltInFunction),
}

impl_from! {
    ModuleId,
    BlockId,
    NatureId,
    NatureAccess,
    DisciplineId,
    NodeId,
    VarId,
    ParamId,
    BranchId,
    FunctionId,
    NatureAttrId,
    DisciplineAttrId,
    BuiltInFunction

    for ScopeDefItem
}

pub trait ScopeDefItemKind: TryFrom<ScopeDefItem> {
    const NAME: &'static str;
}

macro_rules! scope_item_kinds {
    ($($ty: ident => $name:literal),*) => {
        $(impl ScopeDefItemKind for $ty{const NAME: &'static str = $name;})*
        impl ScopeDefItem {
            pub const fn item_kind(&self) -> &'static str {
                match self {
                    $(ScopeDefItem::$ty(_) => $ty::NAME,)*
                }
            }
        }

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
    BuiltInFunction => "function",
    NatureAttrId => "nature attribute",
    DisciplineAttrId => "discipline attribute"
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum ScopeOrigin {
    Root,
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
    BlockScope(BlockId),
}

impl ScopeOrigin {
    pub fn erased_item_tree_id(&self, db: &dyn HirDefDB) -> Option<ErasedItemTreeId> {
        match self {
            ScopeOrigin::Root => None,
            ScopeOrigin::Module(id) => {
                let loc = id.lookup(db);
                Some(loc.erased_id(db))
            }
            ScopeOrigin::Nature(id) => {
                let loc = id.lookup(db);
                Some(loc.erased_id(db))
            }
            ScopeOrigin::Discipline(id) => {
                let loc = id.lookup(db);
                Some(loc.erased_id(db))
            }
            ScopeOrigin::BlockScope(id) => {
                let loc = id.lookup(db);
                Some(loc.erased_id(db))
            }
        }
    }
}

pub type LocalScopeId = Idx<Scope>;

impl_from_typed! {
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
    BlockScope(BlockId) for ScopeOrigin
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    pub origin: ScopeOrigin,
    parent: Option<LocalScopeId>,
    pub children: HashMap<Name, LocalScopeId>,
    duplicate_children: Vec<LocalScopeId>,
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
        scope: LocalScopeId,
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
        scope: LocalScopeId,
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
        scope: LocalScopeId,
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
            ScopeDefItem::ModuleId(module) => module.lookup(db).scope.local_scope,
            ScopeDefItem::BlockId(block) => block.lookup(db).scope.local_scope,
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
        parent: LocalScopeId,
        block: &ast::BlockStmt,
        ast_ptr: &AstPtr<ast::BlockStmt>,
        ast_id_map: &AstIdMap,
        tree: &ItemTree,
        db: &dyn HirDefDB,
    ) -> Option<LocalScopeId> {
        let name = block.block_scope()?.name()?.as_name();
        let scope = *self.scopes[parent].children.get(&name)?;
        // At this point we already have the scope
        // However if another blocks with the same name was declared then this scope is incorrect
        // So the ASTs need to be compared
        let block_scope: BlockId = self.scopes[scope].origin.try_into().unwrap();
        let item_tree = block_scope.lookup(db).id;
        if &ast_id_map.get(tree[item_tree].ast_id) == ast_ptr {
            Some(scope)
        } else {
            self.scopes[parent]
                .duplicate_children
                .iter()
                .find(|scope| {
                    if let ScopeOrigin::BlockScope(block) = self.scopes[**scope].origin {
                        let item_tree = block.lookup(db).id;
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
    NotFoundIn { name: Name, scope: LocalScopeId },
    ExpectedScope { name: Name, found: ScopeDefItem },
    ExpectedItemKind { name: Name, expected: &'static str, found: &'static str },
}
