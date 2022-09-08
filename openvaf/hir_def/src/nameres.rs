use std::ops::{Index, IndexMut};
use std::sync::Arc;

use arena::{Arena, Idx};
use basedb::{AstIdMap, ErasedAstId, FileId};
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use stdx::{impl_display, impl_from, impl_from_typed};
use syntax::name::{kw, Name};
use syntax::{AstNode, Parse, SourceFile, TextRange};

use self::diagnostics::DefDiagnostic;
use crate::builtin::{insert_builtin_scope, BuiltIn, ParamSysFun};
use crate::db::HirDefDB;
use crate::nameres::diagnostics::PathResolveError;
use crate::{
    AliasParamId, BlockId, BranchId, DisciplineId, FunctionArgId, FunctionId, Lookup, ModuleId,
    NatureAttrId, NatureId, NodeId, ParamId, VarId,
};

mod collect;
pub mod diagnostics;
mod pretty;

#[cfg(test)]
mod tests;

#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
pub enum DefMapSource {
    Block(BlockId),
    Function(FunctionId),
    Root,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefMap {
    src: DefMapSource,
    scopes: Arena<Scope>,
    root_scope: LocalScopeId,
    pub diagnostics: Vec<DefDiagnostic>,
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
    pub fn entry(&self) -> LocalScopeId {
        LocalScopeId::from(0u32)
    }
    #[inline(always)]
    pub fn root(&self) -> LocalScopeId {
        self.root_scope
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct NatureAccess(pub NatureAttrId);

impl From<NatureAttrId> for NatureAccess {
    fn from(id: NatureAttrId) -> NatureAccess {
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
    ParamSysFun(ParamSysFun),
    AliasParamId(AliasParamId),
    BranchId(BranchId),
    FunctionId(FunctionId),
    BuiltIn(BuiltIn),
    FunctionReturn(FunctionId),
    FunctionArgId(FunctionArgId),
    NatureAttrId(NatureAttrId),
}

impl ScopeDefItem {
    pub fn ast_id(&self, db: &dyn HirDefDB) -> Option<ErasedAstId> {
        let id: ErasedAstId = match self {
            ScopeDefItem::ModuleId(module) => module.lookup(db).ast_id(db).into(),
            ScopeDefItem::BlockId(block) => block.lookup(db).ast.into(),
            ScopeDefItem::NatureId(nature) => nature.lookup(db).ast_id(db).into(),
            ScopeDefItem::NatureAccess(access) => access.0.lookup(db).ast_id(db).into(),
            ScopeDefItem::DisciplineId(discipline) => discipline.lookup(db).ast_id(db).into(),
            ScopeDefItem::VarId(var) => var.lookup(db).ast_id(db).into(),
            ScopeDefItem::ParamId(param) => param.lookup(db).ast_id(db).into(),
            ScopeDefItem::BranchId(branch) => branch.lookup(db).ast_id(db).into(),
            ScopeDefItem::FunctionReturn(fun) | ScopeDefItem::FunctionId(fun) => {
                fun.lookup(db).ast_id(db).into()
            }
            ScopeDefItem::FunctionArgId(arg) => arg.lookup(db).ast_id(db).into(),
            ScopeDefItem::NodeId(node) => node.lookup(db).ast_id(db),
            ScopeDefItem::BuiltIn(_) | ScopeDefItem::ParamSysFun(_) => return None,
            ScopeDefItem::AliasParamId(id) => id.lookup(db).ast_id(db).into(),
            ScopeDefItem::NatureAttrId(id) => id.lookup(db).ast_id(db).into(),
        };
        Some(id)
    }

    pub fn text_range(
        &self,
        db: &dyn HirDefDB,
        ast_id_map: &AstIdMap,
        parse: &Parse<SourceFile>,
    ) -> Option<TextRange> {
        let res = match self {
            ScopeDefItem::ModuleId(module) => ast_id_map
                .get(module.lookup(db).ast_id(db))
                .to_node(parse.tree().syntax())
                .name()?
                .syntax()
                .text_range(),
            ScopeDefItem::BlockId(block) => ast_id_map
                .get(block.lookup(db).ast)
                .to_node(parse.tree().syntax())
                .block_scope()?
                .name()?
                .syntax()
                .text_range(),
            ScopeDefItem::NatureId(nature) => ast_id_map
                .get(nature.lookup(db).ast_id(db))
                .to_node(parse.tree().syntax())
                .name()?
                .syntax()
                .text_range(),
            ScopeDefItem::NatureAccess(access) => {
                ast_id_map.get(access.0.lookup(db).ast_id(db)).range()
            }

            ScopeDefItem::DisciplineId(discipline) => ast_id_map
                .get(discipline.lookup(db).ast_id(db))
                .to_node(parse.tree().syntax())
                .name()?
                .syntax()
                .text_range(),
            ScopeDefItem::VarId(var) => ast_id_map.get(var.lookup(db).ast_id(db)).range(),

            ScopeDefItem::ParamId(param) => ast_id_map.get(param.lookup(db).ast_id(db)).range(),
            ScopeDefItem::BranchId(branch) => {
                let branch = branch.lookup(db);
                let pos = branch.item_tree(db)[branch.id].name_idx;
                ast_id_map
                    .get(branch.ast_id(db))
                    .to_node(parse.tree().syntax())
                    .names()
                    .nth(pos)?
                    .syntax()
                    .text_range()
            }
            ScopeDefItem::FunctionReturn(fun) | ScopeDefItem::FunctionId(fun) => ast_id_map
                .get(fun.lookup(db).ast_id(db))
                .to_node(parse.tree().syntax())
                .name()?
                .syntax()
                .text_range(),
            ScopeDefItem::FunctionArgId(arg) => {
                let arg = arg.lookup(db);
                let fun = arg.fun.lookup(db);
                let pos = fun.item_tree(db)[fun.id].args[arg.id].name_idx;
                ast_id_map
                    .get(arg.ast_id(db))
                    .to_node(parse.tree().syntax())
                    .names()
                    .nth(pos)?
                    .syntax()
                    .text_range()
            }
            ScopeDefItem::NodeId(node) => ast_id_map.get_syntax(node.lookup(db).ast_id(db)).range(),
            ScopeDefItem::BuiltIn(_) | ScopeDefItem::ParamSysFun(_) => return None,
            ScopeDefItem::AliasParamId(id) => ast_id_map.get(id.lookup(db).ast_id(db)).range(),
            ScopeDefItem::NatureAttrId(id) => ast_id_map.get(id.lookup(db).ast_id(db)).range(),
        };

        Some(res)
    }
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
    AliasParamId,
    ParamSysFun,
    // DisciplineAttrId,
    FunctionArgId,
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
                    ScopeDefItem::FunctionReturn(_) => VarId::NAME
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
    NatureAttrId => "nature attribute",
    NodeId => "node",
    VarId => "variable",
    ParamId => "parameter",
    ParamSysFun => "hierarchical parameter system function",
    AliasParamId => "parameter",
    BranchId => "branch",
    FunctionId => "function",
    BuiltIn => "function",
    FunctionArgId => "function argument"
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum ScopeOrigin {
    Root,
    Module(ModuleId),
    // Nature(NatureId),
    // Discipline(DisciplineId),
    Block(BlockId),
    Function(FunctionId),
}

pub type LocalScopeId = Idx<Scope>;

impl_from_typed! {
    Module(ModuleId),
    // Nature(NatureId),
    // Discipline(DisciplineId),
    Block(BlockId),
    Function(FunctionId)
    for ScopeOrigin
}

static BUILTIN_SCOPE: Lazy<IndexMap<Name, ScopeDefItem, ahash::RandomState>> = Lazy::new(|| {
    let mut scope = IndexMap::default();
    insert_builtin_scope(&mut scope);
    scope
});

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    pub origin: ScopeOrigin,
    parent: Option<LocalScopeId>,
    pub children: IndexMap<Name, LocalScopeId, ahash::RandomState>,
    pub declarations: IndexMap<Name, ScopeDefItem, ahash::RandomState>,
}

impl DefMap {
    pub fn def_map_query(db: &dyn HirDefDB, root_file: FileId) -> Arc<DefMap> {
        collect::collect_root_def_map(db, root_file)
    }

    pub fn block_def_map_query(db: &dyn HirDefDB, block: BlockId) -> Option<Arc<DefMap>> {
        collect::collect_block_map(db, block)
    }

    pub fn function_def_map_query(db: &dyn HirDefDB, fun: FunctionId) -> Arc<DefMap> {
        collect::collect_function_map(db, fun)
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
            found: res.into(),
        })
    }

    pub fn resolve_normal_item_path_in_scope<T: ScopeDefItemKind>(
        &self,
        scope: LocalScopeId,
        path: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<T, PathResolveError> {
        let resolved_path = self.resolve_normal_path_in_scope(scope, path, db)?;
        let res: Result<ScopeDefItem, _> = resolved_path.clone().try_into();
        if let Ok(res) = res {
            if let Ok(res) = res.try_into() {
                return Ok(res);
            }
        }
        Err(PathResolveError::ExpectedItemKind {
            name: path.last().unwrap().clone(),
            expected: T::NAME,
            found: resolved_path,
        })
    }

    pub fn resolve_normal_path_in_scope(
        &self,
        mut scope: LocalScopeId,
        path: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<ResolvedPath, PathResolveError> {
        let mut arc;
        let mut current_map = self;

        let name = &path[0];

        let decl = loop {
            if let Some(decl) = current_map.scopes[scope].declarations.get(name) {
                break *decl;
            }

            match current_map[scope].parent {
                Some(parent) => scope = parent,
                None => match current_map.src {
                    DefMapSource::Block(block) => {
                        let block = block.lookup(db);
                        arc = block.parent.def_map(db);
                        current_map = &*arc;
                        scope = block.parent.local_scope;
                    }
                    DefMapSource::Root => {
                        if let Some(builtin) = BUILTIN_SCOPE.get(name) {
                            break *builtin;
                        }

                        return Err(PathResolveError::NotFound { name: name.clone() });
                    }
                    DefMapSource::Function(_fun) => {
                        if let Some(builtin) = BUILTIN_SCOPE.get(name) {
                            break *builtin;
                        }

                        // let parent = fun.lookup(db).scope;
                        //parent.def_map(db).resolve_normal_path_in_scope(scope, path, db); TODO
                        //give hint if found in full def map

                        return Err(PathResolveError::NotFound { name: name.clone() });
                    }
                },
            }
        };

        if path.len() == 1 {
            return Ok(decl.into());
        }

        scope = match decl {
            ScopeDefItem::ModuleId(module) => module.lookup(db).scope.local_scope,

            ScopeDefItem::BlockId(block) => match db.block_def_map(block) {
                Some(block_map) => {
                    arc = block_map;
                    current_map = &*arc;
                    current_map.entry()
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
        &self,
        segments: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<ResolvedPath, PathResolveError> {
        debug_assert!(matches!(self.src, DefMapSource::Function(_) | DefMapSource::Root));
        self.resolve_names_in(self.root(), &kw::root, segments, db)
    }

    pub fn resolve_root_item_path<T: ScopeDefItemKind>(
        &self,
        segments: &[Name],
        db: &dyn HirDefDB,
    ) -> Result<T, PathResolveError> {
        let resolved_path = self.resolve_root_path(segments, db)?;
        let res: Result<ScopeDefItem, _> = resolved_path.clone().try_into();
        if let Ok(res) = res {
            if let Ok(res) = res.try_into() {
                return Ok(res);
            }
        }
        Err(PathResolveError::ExpectedItemKind {
            name: segments.last().unwrap().clone(),
            expected: T::NAME,
            found: resolved_path,
        })
    }

    fn resolve_names_in<'a>(
        &self,
        mut scope: LocalScopeId,
        mut scope_name: &'a Name,
        path: &'a [Name],
        db: &dyn HirDefDB,
    ) -> Result<ResolvedPath, PathResolveError> {
        let (segments, name) =
            if let [segments @ .., name] = path { (segments, name) } else { unreachable!() };

        let mut arc;
        let mut current_map = self;

        for (i, segment) in segments.iter().enumerate() {
            match current_map.scopes[scope].children.get(segment) {
                Some(child) => scope = *child,
                None => match current_map.scopes[scope].declarations.get(segment) {
                    Some(ScopeDefItem::BlockId(block)) => match db.block_def_map(*block) {
                        Some(block_map) => {
                            arc = block_map;
                            current_map = &*arc;
                            scope = current_map.entry();
                        }
                        None => {
                            return Err(PathResolveError::NotFoundIn {
                                name: path[i + 1].clone(),
                                scope: segment.clone(),
                            })
                        }
                    },
                    Some(ScopeDefItem::BranchId(branch))
                        if path.get(i + 1) == Some(&kw::potential) =>
                    {
                        let rem = &path[(i + 1)..];
                        if let [name] = rem {
                            return Ok(ResolvedPath::PotentialAttribute {
                                branch: *branch,
                                name: name.clone(),
                            });
                        } else {
                            return Err(PathResolveError::ExpectedNatureAttributeIdent {
                                found: rem.to_owned().into_boxed_slice(),
                            });
                        }
                    }

                    Some(ScopeDefItem::BranchId(branch)) if path.get(i + 1) == Some(&kw::flow) => {
                        let rem = &path[(i + 1)..];
                        if let [name] = rem {
                            return Ok(ResolvedPath::FlowAttriubte {
                                branch: *branch,
                                name: name.clone(),
                            });
                        } else {
                            return Err(PathResolveError::ExpectedNatureAttributeIdent {
                                found: rem.to_owned().into_boxed_slice(),
                            });
                        }
                    }

                    Some(found) => {
                        return Err(PathResolveError::ExpectedScope {
                            name: segment.clone(),
                            found: *found,
                        });
                    }
                    None => {
                        return Err(PathResolveError::NotFoundIn {
                            name: segment.clone(),
                            scope: scope_name.clone(),
                        })
                    }
                },
            }

            scope_name = segment;
        }

        match self.scopes[scope].declarations.get(name) {
            Some(res) => Ok(ResolvedPath::ScopeDefItem(*res)),
            None => {
                Err(PathResolveError::NotFoundIn { name: name.clone(), scope: scope_name.clone() })
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedPath {
    FlowAttriubte { branch: BranchId, name: Name },
    PotentialAttribute { branch: BranchId, name: Name },
    ScopeDefItem(ScopeDefItem),
}

impl_display! {
    match ResolvedPath{
        ResolvedPath::FlowAttriubte{..}  => "nature attribute";
        ResolvedPath::PotentialAttribute{..} => "nature attribute";
        ResolvedPath::ScopeDefItem(item) => "{}", item.item_kind();
    }
}

impl_from!(ScopeDefItem for ResolvedPath);
