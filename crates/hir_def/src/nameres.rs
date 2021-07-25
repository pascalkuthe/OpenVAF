use crate::{
    builtin::BuiltinFunction, db::HirDefDB, BlockId, BranchId, DisciplineId, FunctionId, Lookup,
    ModuleId, NatureId, NetId, ParamId, PortId, VarId,
};
use data_structures::HashMap;
use derive_more::{From, Index, IndexMut, Into, TryInto};
use la_arena::{Arena, Idx};

use crate::Name;

mod collect;
mod diagnostics;

#[derive(PartialEq, Eq, Clone, Index, IndexMut, Debug)]
pub struct DefMap {
    #[index]
    scopes: Arena<Scope>,
}

impl DefMap {
    fn new() -> DefMap {
        DefMap { scopes: Arena::new() }
    }
}

impl DefMap {
    pub fn root(&self) -> ScopeId {
        ScopeId::from_raw(0u32.into())
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
    Port(PortId),
    Net(NetId),
    VarId(VarId),
    ParamId(ParamId),
    Branch(BranchId),
    Function(FunctionId),
    BuiltInFunction(BuiltinFunction),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum ScopeOrigin {
    Root,
    Module(ModuleId),
    BlockScope(BlockId),
}
pub type ScopeId = Idx<Scope>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    origin: ScopeOrigin,
    parent: Option<ScopeId>,
    children: HashMap<Name, ScopeId>,
    /// All Items visible in this scope (superset of declarations)
    visible_items: HashMap<Name, ScopeDefItem>,
    /// Items declared in this scopes
    declarations: HashMap<Name, ScopeDefItem>,
}

impl DefMap {
    pub fn resolve_name_in_scope(
        &self,
        scope: ScopeId,
        name: &Name,
    ) -> Result<&ScopeDefItem, PathResolveError> {
        self.scopes[scope]
            .visible_items
            .get(name)
            .ok_or_else(|| PathResolveError::NotFound { name: name.clone() })
    }

    pub fn resolve_path_in_scope(
        &self,
        scope: ScopeId,
        path: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<&ScopeDefItem, PathResolveError> {
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
                    found: *root_scope,
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
            .ok_or_else(|| PathResolveError::NotFoundIn { name: name.clone(), scope })
    }
}

// TODO better errors / hints?
pub enum PathResolveError {
    NotFound { name: Name },
    NotFoundIn { name: Name, scope: ScopeId },
    ExpectedScope { name: Name, found: ScopeDefItem },
}
