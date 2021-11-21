use std::ops::{Index, IndexMut};
use std::sync::Arc;

use crate::builtin::{insert_builtin_scope, BuiltIn};
use crate::name::kw;
use crate::{
    db::HirDefDB, BlockId, BranchId, DisciplineAttrId, DisciplineId, FunctionId, Lookup, ModuleId,
    NatureAttrId, NatureId, NodeId, ParamId, VarId,
};
use crate::{Name, Node};
use ahash::AHashMap as HashMap;
use arena::{Arena, Idx};
use basedb::lints::ErasedItemTreeId;
use basedb::FileId;
use once_cell::sync::Lazy;
use stdx::{impl_from, impl_from_typed};

mod collect;
mod diagnostics;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefMap {
    block: Option<BlockId>,
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
    #[inline(always)]
    pub fn root(&self) -> LocalScopeId {
        LocalScopeId::from(0u32)
    }
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
    BuiltIn(BuiltIn),
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
    BuiltIn

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
    BuiltIn => "function",
    NatureAttrId => "nature attribute",
    DisciplineAttrId => "discipline attribute"
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum ScopeOrigin {
    Root,
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
    Block(BlockId),
}

pub type LocalScopeId = Idx<Scope>;

impl_from_typed! {
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
    Block(BlockId)
    for ScopeOrigin
}

static BUILTIN_SCOPE: Lazy<HashMap<Name, ScopeDefItem>> = Lazy::new(|| {
    let mut scope = HashMap::new();
    insert_builtin_scope(&mut scope);
    scope
});

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    pub origin: ScopeOrigin,
    parent: Option<LocalScopeId>,
    pub children: HashMap<Name, LocalScopeId>,
    duplicate_children: Vec<LocalScopeId>,
    /// Items declared in this scopes
    pub declarations: HashMap<Name, ScopeDefItem>,
}

impl DefMap {
    pub fn def_map_query(db: &dyn HirDefDB, root_file: FileId) -> Arc<DefMap> {
        collect::collect_root_def_map(db, root_file)
    }

    pub fn block_def_map_query(db: &dyn HirDefDB, block: BlockId) -> Option<Arc<DefMap>> {
        collect::collect_block_map(db, block)
    }

    pub fn resolve_local_name_in_scope(
        &self,
        mut scope: LocalScopeId,
        name: &Name,
    ) -> Result<ScopeDefItem, PathResolveError> {
        loop {
            if let Some(decl) = self.scopes[scope].declarations.get(name) {
                return Ok(*decl);
            }

            match self[scope].parent {
                Some(parent) => scope = parent,
                None => return Err(PathResolveError::NotFound { name: name.clone() }),
            }
        }
    }

    pub fn resolve_local_item_in_scope<T: ScopeDefItemKind>(
        &self,
        scope: LocalScopeId,
        name: &Name,
    ) -> Result<T, PathResolveError> {
        let res = self.resolve_local_name_in_scope(scope, name)?;
        res.try_into().map_err(|_| PathResolveError::ExpectedItemKind {
            name: name.clone(),
            expected: T::NAME,
            found: res,
        })
    }

    pub fn resolve_normal_item_path_in_scope<T: ScopeDefItemKind>(
        &self,
        scope: LocalScopeId,
        path: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<T, PathResolveError> {
        let res = self.resolve_normal_path_in_scope(scope, path, db)?;
        res.try_into().map_err(|_| PathResolveError::ExpectedItemKind {
            name: path.last().unwrap().clone(),
            expected: T::NAME,
            found: res,
        })
    }

    pub fn resolve_normal_path_in_scope(
        &self,
        mut scope: LocalScopeId,
        path: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<ScopeDefItem, PathResolveError> {
        let mut arc;
        let mut current_map = self;

        let name = &path[0];
        let decl = loop {
            if let Some(decl) = current_map.scopes[scope].declarations.get(name) {
                break *decl;
            }

            match current_map[scope].parent {
                Some(parent) => scope = parent,
                None => match self.block {
                    Some(block) => {
                        let block = block.lookup(db);
                        arc = block.parent.def_map(db);
                        current_map = &*arc;
                    }
                    None => {
                        if let Some(builtin) = BUILTIN_SCOPE.get(name) {
                            break *builtin;
                        }

                        return Err(PathResolveError::NotFound { name: name.clone() });
                    }
                },
            }
        };

        if path.len() == 1 {
            return Ok(decl);
        }

        scope = match decl {
            ScopeDefItem::ModuleId(module) => module.lookup(db).scope.local_scope,
            ScopeDefItem::NatureId(nature) => nature.lookup(db).scope.local_scope,
            ScopeDefItem::DisciplineId(discipline) => discipline.lookup(db).scope.local_scope,

            ScopeDefItem::BlockId(block) => match db.block_def_map(block) {
                Some(block_map) => {
                    arc = block_map;
                    current_map = &*arc;
                    current_map.root()
                }
                None => {
                    return Err(PathResolveError::NotFoundIn {
                        name: path[1].clone(),
                        scope: path[0].clone(),
                    })
                }
            },

            _ => return Err(PathResolveError::ExpectedScope { name: name.clone(), found: decl }),
        };

        current_map.resolve_names_in(scope, name, &path[1..], db)
    }

    pub fn resolve_root_path(
        db: &dyn HirDefDB,
        root_file: FileId,
        segments: &[Name],
    ) -> Result<ScopeDefItem, PathResolveError> {
        let def_map = db.def_map(root_file);
        def_map.resolve_names_in(def_map.root(), &kw::root, segments, db)
    }

    pub fn resolve_root_item_path_in_scope<T: ScopeDefItemKind>(
        db: &dyn HirDefDB,
        root_file: FileId,
        segments: &[Name],
    ) -> Result<T, PathResolveError> {
        let res = DefMap::resolve_root_path(db, root_file, segments)?;
        res.try_into().map_err(|_| PathResolveError::ExpectedItemKind {
            name: segments.last().unwrap().clone(),
            expected: T::NAME,
            found: res,
        })
    }

    fn resolve_names_in<'a>(
        &self,
        mut scope: LocalScopeId,
        mut scope_name: &'a Name,
        path: &'a [Name],
        db: &dyn HirDefDB,
    ) -> Result<ScopeDefItem, PathResolveError> {
        let (segments, name) =
            if let [segments @ .., name] = path { (segments, name) } else { unreachable!() };

        let mut arc;
        let mut current_map = self;

        for (i, segment) in segments.iter().enumerate() {
            match current_map.scopes[scope].children.get(segment) {
                Some(child) => scope = *child,
                None => {
                    if let Some(found) = current_map.scopes[scope].declarations.get(segment) {
                        if let ScopeDefItem::BlockId(block) = *found {
                            match db.block_def_map(block) {
                                Some(block_map) => {
                                    arc = block_map;
                                    current_map = &*arc;
                                    scope = current_map.root();
                                }
                                None => {
                                    return Err(PathResolveError::NotFoundIn {
                                        name: path[i + 1].clone(),
                                        scope: segment.clone(),
                                    })
                                }
                            }
                        } else {
                            return Err(PathResolveError::ExpectedScope {
                                name: segment.clone(),
                                found: *found,
                            });
                        }
                    } else {
                        return Err(PathResolveError::NotFoundIn {
                            name: segment.clone(),
                            scope: scope_name.clone(),
                        });
                    }
                }
            }

            scope_name = segment;
        }

        self.scopes[scope].declarations.get(name).copied().ok_or_else(|| {
            PathResolveError::NotFoundIn { name: name.clone(), scope: scope_name.clone() }
        })
    }
}

// TODO better errors / hints?
#[derive(Debug, Clone)]
pub enum PathResolveError {
    NotFound { name: Name },
    NotFoundIn { name: Name, scope: Name },
    ExpectedScope { name: Name, found: ScopeDefItem },
    ExpectedItemKind { name: Name, expected: &'static str, found: ScopeDefItem },
}
