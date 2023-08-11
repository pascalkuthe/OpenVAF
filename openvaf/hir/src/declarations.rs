use std::iter::once;
use std::mem::transmute;
use std::ops::Deref;
use std::slice;
use std::sync::Arc;

use hir_def::nameres::{self, DefMap, LocalScopeId, ScopeDefItem};
use smol_str::SmolStr;
use syntax::name::Name;

use crate::{
    AliasParameter, Block, Branch, HirDatabase, Module, Node, Parameter, ScopeDef, Variable,
};

struct Scope {
    _def_map: Arc<DefMap>,
    iter: slice::Iter<'static, (Name, nameres::ScopeDefItem)>,
    def: Option<(Name, ScopeDef)>,
}
impl Scope {
    fn new(def_map: Arc<DefMap>, scope: LocalScopeId, block: Option<(Name, ScopeDef)>) -> Scope {
        // safety: def_map is a immultable/an arc that will live atleast as long as the scop
        let iter = unsafe { transmute(def_map[scope].declarations.iter()) };
        Scope { _def_map: def_map, iter, def: block }
    }
}

pub struct ScopePaths<'a> {
    path: Vec<Name>,
    stack: Vec<Scope>,
    db: &'a dyn HirDatabase,
}

impl ScopePaths<'_> {
    /// crates a path in the current scope with the final
    /// component given by `name`
    pub fn to_path(&self, name: Name) -> SmolStr {
        if self.path.is_empty() {
            // fast path
            return name.into();
        }
        self.path.iter().flat_map(|path| [path, "."]).chain(once(name.deref())).collect()
    }
}

impl Iterator for ScopePaths<'_> {
    type Item = (Name, ScopeDef);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let scope = self.stack.last_mut()?;
            if let Some((name, item)) = scope.iter.next().cloned() {
                let def = match item {
                    ScopeDefItem::BlockId(id) => {
                        if let Some(def_map) = self.db.block_def_map(id) {
                            let entry = def_map.entry();
                            self.stack.push(Scope::new(
                                def_map,
                                entry,
                                Some((name, ScopeDef::Block(Block { id }))),
                            ))
                        }
                        continue;
                    }
                    ScopeDefItem::ModuleId(id) => ScopeDef::ModuleInstance(Module { id }),
                    ScopeDefItem::NodeId(id) => ScopeDef::Node(Node { id }),
                    ScopeDefItem::VarId(id) => ScopeDef::Variable(Variable { id }),
                    ScopeDefItem::ParamId(id) => ScopeDef::Parameter(Parameter { id }),
                    ScopeDefItem::AliasParamId(id) => {
                        ScopeDef::AliasParameter(AliasParameter { id })
                    }
                    ScopeDefItem::BranchId(id) => ScopeDef::Branch(Branch { id }),
                    _ => continue,
                };
                return Some((name, def));
            } else {
                self.path.pop();
                let scope = self.stack.pop()?;
                if let Some(def) = scope.def {
                    return Some(def);
                }
                continue;
            }
        }
    }
}
