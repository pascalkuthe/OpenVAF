use std::sync::Arc;

use basedb::FileId;
use data_structures::HashMap;
use la_arena::{Idx, RawIdx};

use crate::builtin::BUILTIN_FUNCTIONS;
use crate::{
    db::HirDefDB,
    item_tree::{BlockScope, BlockScopeItem, ItemTree, ItemTreeId, ItemTreeNode, Module, RootItem},
    nameres::NatureAccess,
    BlockLoc, Intern, ItemLoc, ModuleLoc, Name,
};

use super::{DefMap, Scope, ScopeDefItem, ScopeId, ScopeOrigin};

pub fn collect_defs(db: &dyn HirDefDB, root_file: FileId) -> Arc<DefMap> {
    let tree = &db.item_tree(root_file);
    let mut collector = DefCollector { map: DefMap::new(), tree: &tree, db, root_file };

    collector.collect();
    collector.propagate_visisbile_item(collector.map.root());

    Arc::new(collector.map)
}

fn next_id<N>(arena: &la_arena::Arena<N>) -> Idx<N> {
    Idx::from_raw(RawIdx::from(arena.len() as u32))
}

struct DefCollector<'a> {
    map: DefMap,
    tree: &'a ItemTree,
    db: &'a dyn HirDefDB,
    root_file: FileId,
}

impl DefCollector<'_> {
    fn collect_module(&mut self, item_tree: ItemTreeId<Module>, parent_scope: ScopeId) {
        let scope = next_id(&self.map.scopes);
        let id = ModuleLoc { item_tree, scope }.intern(self.db);

        let scope = self.new_scope(ScopeOrigin::Module(id), parent_scope);
        let module = &self.tree[item_tree];

        self.map.scopes[parent_scope].declarations.insert(module.name.clone(), id.into());
        self.map.scopes[parent_scope].children.insert(module.name.clone(), scope);

        if module.head_ports.is_empty() {
            let mut exptected_ports = module.exptected_ports.clone();
            for id in module.body_ports.clone() {
                let port = &self.tree[id];
                let pos = exptected_ports.iter().position(|it| it == &port.name);
                match pos {
                    Some(pos) => {
                        exptected_ports.remove(pos);
                    }
                    None => todo!("ERROR"),
                }

                self.insert_item_decl(scope, port.name.clone(), id)
            }
        } else {
            if !module.body_ports.is_empty() {
                todo!("ERROR")
            }

            if !module.exptected_ports.is_empty() {
                todo!("ERROR")
            }

            for id in module.head_ports.clone() {
                let port = &self.tree[id];
                self.insert_item_decl(scope, port.name.clone(), id)
            }
        }

        for id in module.branches.clone() {
            self.insert_item_decl(scope, self.tree[id].name.clone(), id)
        }

        for id in module.nets.clone() {
            self.insert_item_decl(scope, self.tree[id].name.clone(), id)
        }

        for id in module.functions.clone() {
            self.insert_item_decl(scope, self.tree[id].name.clone(), id)
        }

        for item in &module.scope_items {
            match item {
                BlockScopeItem::Scope(id) => self.collect_block_scope(*id, scope),
                BlockScopeItem::Parameter(id) => {
                    self.insert_item_decl(scope, self.tree[*id].name.clone(), *id)
                }
                BlockScopeItem::Variable(id) => {
                    self.insert_item_decl(scope, self.tree[*id].name.clone(), *id)
                }
            }
        }
    }

    fn collect_block_scope(&mut self, item_tree: ItemTreeId<BlockScope>, parent_scope: ScopeId) {
        let scope = next_id(&self.map.scopes);
        let id = BlockLoc { item_tree, scope }.intern(self.db);

        let scope = self.new_scope(ScopeOrigin::BlockScope(id), parent_scope);
        let block = &self.tree[item_tree];

        self.map.scopes[parent_scope].declarations.insert(block.name.clone(), id.into());
        self.map.scopes[parent_scope].children.insert(block.name.clone(), scope);

        for id in block.parameters.clone() {
            self.insert_item_decl(scope, self.tree[id].name.clone(), id)
        }

        for id in block.variables.clone() {
            self.insert_item_decl(scope, self.tree[id].name.clone(), id)
        }

        for child_scope in block.scopes.clone() {
            self.collect_block_scope(child_scope, scope)
        }
    }

    fn collect(&mut self) {
        let root_scope = self.map.scopes.alloc(Scope {
            origin: ScopeOrigin::Root,
            parent: None,
            children: HashMap::new(),
            visible_items: HashMap::new(),
            declarations: HashMap::new(),
        });

        debug_assert_eq!(root_scope, self.map.root());

        // let ast = self.db.parse(self.root_file);

        for item in &*self.tree.top_level {
            match *item {
                RootItem::Module(module) => self.collect_module(module, root_scope),
                RootItem::Nature(nature) => {
                    let nature_id =
                        ItemLoc { scope: root_scope, item_tree: nature }.intern(self.db);
                    self.insert_decl(root_scope, self.tree[nature].name.clone(), nature_id);
                    if let Some(access) = &self.tree[nature].access {
                        self.insert_decl(root_scope, access.clone(), NatureAccess(nature_id))
                    }
                }
                RootItem::Discipline(discipline) => self.insert_item_decl(
                    root_scope,
                    self.tree[discipline].name.clone(),
                    discipline,
                ), // TOOD discipline attributes?
            }
        }

        self.insert_prelude();

        // No parent to inherint from
        self.map.scopes[root_scope].visible_items = self.map[root_scope].declarations.clone();
    }

    fn insert_prelude(&mut self) {
        // TODO potential/flow/port_connected/param_given are not really functions:q
        let root = self.map.root();
        let root_scope = &mut self.map[root];
        for fun in BUILTIN_FUNCTIONS {
            root_scope
                .declarations
                .insert(Name::new_inline(fun.info().name), ScopeDefItem::BuiltInFunction(*fun));
        }
    }

    fn insert_item_decl<N>(&mut self, dst: ScopeId, name: Name, item_tree: ItemTreeId<N>)
    where
        N: ItemTreeNode,
        ItemLoc<N>: Intern,
        <ItemLoc<N> as Intern>::ID: Into<ScopeDefItem>,
    {
        let decl = ItemLoc { scope: dst, item_tree }.intern(self.db);
        self.insert_decl(dst, name, decl)
    }

    fn insert_decl(&mut self, dst: ScopeId, name: Name, decl: impl Into<ScopeDefItem>) {
        if name.is_reserved() {
            todo!("ERROR")
        }
        if let Some(old_decl) = self.map.scopes[dst].declarations.insert(name, decl.into()) {
            todo!("ERROR")
        }
    }

    /// Recruesively walk the DefMap after the colleciton completes and propgate the visisble item
    /// inside a each scope to its children. This allows name resolution to remain a single hashmap
    /// lookup everywhere. Since Scopes are always shadowing in VerilogAMS this works a follows
    /// visible_items[scope] = visible_items[parent] ∪ declarations[scope]
    /// This union can only be calculated once collection for the parent has finished and therefore
    /// requires a second tree transversal
    fn propagate_visisbile_item(&mut self, scope: ScopeId) {
        for child_id in self.map[scope].children.clone().values() {
            let visible_items = self.map[scope].visible_items.clone();
            let child = &mut self.map[*child_id];
            child.visible_items = visible_items;
            child
                .visible_items
                .extend(child.declarations.iter().map(|(k, v)| (k.clone(), v.clone())));
            self.propagate_visisbile_item(*child_id);
        }
    }

    fn new_scope(&mut self, origin: ScopeOrigin, parent: ScopeId) -> ScopeId {
        self.map.scopes.alloc(Scope {
            origin,
            parent: Some(parent),
            children: HashMap::new(),
            visible_items: HashMap::new(),
            declarations: HashMap::new(),
        })
    }
}
