use std::sync::Arc;

use ahash::AHashMap as HashMap;
use arena::{Arena, Idx};
use basedb::FileId;
use bitset::BitSet;

use crate::builtin::BUILTIN_FUNCTIONS;
use crate::item_tree::{Discipline, Net, Port};
use crate::name::kw;
use crate::{
    db::HirDefDB,
    item_tree::{BlockScope, BlockScopeItem, ItemTree, ItemTreeId, ItemTreeNode, Module, RootItem},
    nameres::NatureAccess,
    BlockLoc, DisciplineAttrLoc, DisciplineLoc, Intern, ItemLoc, ModuleLoc, Name, Nature,
    NatureAttrLoc, NatureId, NatureLoc,
};
use crate::{Lookup, Node, NodeId, ScopeId};
use stdx::vec::SliceExntesions;

use super::{DefMap, LocalScopeId, Scope, ScopeDefItem, ScopeDefItemKind, ScopeOrigin};

pub fn collect_defs(db: &dyn HirDefDB, root_file: FileId) -> Arc<DefMap> {
    let tree = &db.item_tree(root_file);
    let scope_cnt = tree.data.block_scopes.len()
        + tree.data.natures.len()
        + tree.data.disciplines.len()
        + tree.data.modules.len();

    let mut collector = DefCollector {
        map: DefMap {
            scopes: Arena::with_capacity(scope_cnt),
            nodes: Arena::with_capacity(tree.data.nets.len()),
        },
        tree,
        db,
        root_file,
    };

    collector.collect();
    collector.propagate_visisbile_item(collector.map.root());

    Arc::new(collector.map)
}

struct DefCollector<'a> {
    map: DefMap,
    tree: &'a ItemTree,
    db: &'a dyn HirDefDB,
    root_file: FileId,
}

impl DefCollector<'_> {
    fn next_scope(&self) -> ScopeId {
        ScopeId { root_file: self.root_file, local_scope: self.map.scopes.next_key() }
    }
    fn resolve_existing_decl_in_scope<T: ScopeDefItemKind>(
        &self,
        scope: LocalScopeId,
        name: &Name,
    ) -> Option<T> {
        let decl = self.map.scopes[scope].declarations.get(name).copied()?;

        let res = decl.try_into().ok();
        if res.is_none() {
            todo!("TODO already declared error")
        }
        res
    }

    fn insert_port(&mut self, id: ItemTreeId<Port>, scope: LocalScopeId) {
        let port = &self.tree[id];

        let node = Node {
            port: Some(id),
            discipline: port.discipline.as_ref().map(|_| id.into()),
            gnd_declaration: port.is_gnd.then(|| id.into()),
        };

        self.insert_or_update_node(node, scope, &port.name)
    }

    fn insert_net(&mut self, id: ItemTreeId<Net>, scope: LocalScopeId) {
        let net = &self.tree[id];

        let node = Node {
            port: None,
            discipline: net.discipline.as_ref().map(|_| id.into()),
            gnd_declaration: net.is_gnd.then(|| id.into()),
        };

        self.insert_or_update_node(node, scope, &net.name)
    }

    fn insert_or_update_node(&mut self, node: Node, scope: LocalScopeId, name: &Name) {
        match self.resolve_existing_decl_in_scope::<NodeId>(scope, name) {
            Some(res) => {
                let existing_node = &mut self.map.nodes[res];
                if let Some(port) = node.port {
                    if let Some(other) = existing_node.port.replace(port) {
                        todo!("direction redeclared for {:?}", other)
                    }
                }

                if let Some(gnd) = node.gnd_declaration {
                    if let Some(other) = existing_node.gnd_declaration.replace(gnd) {
                        todo!("gnd discipline redeclared for {:?}", other)
                    }
                }

                if let Some(discipline) = node.discipline {
                    if let Some(other) = existing_node.discipline.replace(discipline) {
                        todo!("ERROR discipline redeclared for {:?}", other)
                    }
                }
            }
            None => {
                let node = self.map.nodes.push_and_get_key(node);
                self.insert_decl(scope, name.clone(), node)
            }
        }
    }

    fn collect_module(&mut self, item_tree: ItemTreeId<Module>, parent_scope: LocalScopeId) {
        let id = ModuleLoc { id: item_tree, scope: self.next_scope() }.intern(self.db);

        let scope = self.new_scope(ScopeOrigin::Module(id), parent_scope);
        let module = &self.tree[item_tree];

        self.insert_scope(parent_scope, scope, module.name.clone(), id);

        if module.head_ports.is_empty() {
            let mut exptected_ports = module.exptected_ports.clone();
            for id in module.body_ports.clone() {
                let port = &self.tree[id];
                let pos = exptected_ports.iter().position(|it| it == &port.name);
                match pos {
                    Some(pos) => {
                        exptected_ports.remove(pos);
                    }
                    None if !module.exptected_ports.contains(&port.name) => todo!("ERROR"),
                    None => (),
                }
                self.insert_port(id, scope);
            }
        } else {
            if !module.body_ports.is_empty() {
                todo!("ERROR")
            }

            if !module.exptected_ports.is_empty() {
                todo!(
                    "ERROR expected ports {:?}, body_ports {:?}, head_ports {:?}",
                    module.exptected_ports,
                    module.body_ports,
                    module.head_ports
                )
            }

            for id in module.head_ports.clone() {
                self.insert_port(id, scope);
            }
        }

        for id in module.branches.clone() {
            self.insert_item_decl(scope, self.tree[id].name.clone(), id)
        }

        for id in module.nets.clone() {
            self.insert_net(id, scope);
        }

        for id in module.functions.clone() {
            self.insert_item_decl(scope, self.tree[id].name.clone(), id)
        }

        self.lower_scope_items(&module.scope_items, scope)
    }

    fn lower_scope_items(&mut self, scope_items: &[BlockScopeItem], scope: LocalScopeId) {
        for item in scope_items {
            match item {
                BlockScopeItem::Scope(id) => self.collect_block_scope(*id, scope),
                BlockScopeItem::Parameter(id) => {
                    // let file_map = self.db.ast_id_map(self.root_file);
                    // let param = file_map.get(self.tree[*id].ast_id);
                    // let param = param.to_node(&self.db.parse(self.root_file).syntax_node());
                    // use syntax::AstNode;
                    // error!("Parameter: {}", param.syntax().text());
                    self.insert_item_decl(scope, self.tree[*id].name.clone(), *id)
                }
                BlockScopeItem::Variable(id) => {
                    // let file_map = self.db.ast_id_map(self.root_file);
                    // let param = file_map.get(self.tree[*id].ast_id);
                    // let param = param.to_node(&self.db.parse(self.root_file).syntax_node());
                    // use syntax::AstNode;
                    // error!("Var: {}", param.syntax().text());
                    self.insert_item_decl(scope, self.tree[*id].name.clone(), *id)
                }
            }
        }
    }

    fn collect_block_scope(
        &mut self,
        item_tree: ItemTreeId<BlockScope>,
        parent_scope: LocalScopeId,
    ) {
        let id = BlockLoc { id: item_tree, scope: self.next_scope() }.intern(self.db);

        let scope = self.new_scope(ScopeOrigin::BlockScope(id), parent_scope);
        let block = &self.tree[item_tree];

        self.insert_scope(parent_scope, scope, block.name.clone(), id);
        self.lower_scope_items(&block.scope_items, scope)
    }

    fn collect_discipline(
        &mut self,
        item_tree: ItemTreeId<Discipline>,
        parent_scope: LocalScopeId,
    ) {
        let id = DisciplineLoc { id: item_tree, scope: self.next_scope() }.intern(self.db);

        let local_scope = self.new_scope(ScopeOrigin::Discipline(id), parent_scope);
        let discipline = &self.tree[item_tree];

        self.insert_scope(parent_scope, local_scope, discipline.name.clone(), id);

        for attr in discipline.attrs.clone() {
            let name = self.tree[attr].name.clone();
            if matches!(&*name, kw::raw::domain | kw::raw::flow | kw::raw::potential) {
                self.insert_into_scope(
                    local_scope,
                    name,
                    DisciplineAttrLoc {
                        scope: ScopeId { root_file: self.root_file, local_scope },
                        id: attr,
                    }
                    .intern(self.db),
                )
            } else {
                self.insert_item_decl(local_scope, name, attr)
            }
        }
    }

    fn collect_nature(
        &mut self,
        item_tree: ItemTreeId<Nature>,
        parent_scope: LocalScopeId,
    ) -> NatureId {
        let id = NatureLoc { id: item_tree, scope: self.next_scope() }.intern(self.db);

        let scope = self.new_scope(ScopeOrigin::Nature(id), parent_scope);
        let nature = &self.tree[item_tree];

        self.insert_scope(parent_scope, scope, nature.name.clone(), id);
        id
    }

    fn collect_nature_attrs(
        &mut self,
        id: NatureId,
        parent_scope: LocalScopeId,
        collected_natures: &mut BitSet<Idx<Nature>>,
    ) {
        let NatureLoc { scope, id: item_tree } = id.lookup(self.db);
        let local_scope = scope.local_scope;
        let nature = &self.tree[item_tree];

        if let Some(access) = &nature.access {
            self.insert_into_scope(parent_scope, access.clone(), NatureAccess(id))
        }

        // TODO check attributes
        // required for base_natures:
        // acccess
        // abstol
        // units

        // May not be overwritten
        // units
        // access

        // ddt_nature / idt_nature may be overwritten but the base nature must be the same

        if let Some(ddt_nature) = &nature.ddt_nature {
            match self.map.resolve_item_in_scope::<NatureId>(parent_scope, ddt_nature) {
                Ok(id) => self.insert_into_scope(local_scope, kw::ddt_nature, id),
                Err(err) => todo!("ERROR {:?}", err),
            }
        }

        if let Some(idt_nature) = &nature.idt_nature {
            match self.map.resolve_item_in_scope::<NatureId>(parent_scope, idt_nature) {
                Ok(id) => self.insert_into_scope(local_scope, kw::idt_nature, id),
                Err(err) => todo!(
                    "ERROR: idt_nature {} not found in {:?}: {:?}",
                    idt_nature,
                    self.map[parent_scope].visible_items,
                    err
                ),
            }
        }

        for attr in self.tree[item_tree].attrs.clone() {
            let name = self.tree[attr].name.clone();

            if matches!(
                &*name,
                kw::raw::access
                    | kw::raw::ddt_nature
                    | kw::raw::idt_nature
                    | kw::raw::units
                    | kw::raw::abstol
            ) {
                self.insert_into_scope(
                    local_scope,
                    name,
                    NatureAttrLoc { scope, id: attr }.intern(self.db),
                )
            } else {
                self.insert_item_decl(local_scope, name, attr)
            }
        }

        if let Some(parent) = &nature.parent {
            match self.map.resolve_item_in_scope::<NatureId>(parent_scope, parent) {
                Ok(id) => {
                    let NatureLoc {
                        scope: parent_nature_scope,
                        id: parent_nature_item_tree,
                    } = id.lookup(self.db);
                    // If the parent hasn't already been processed do so now
                    // TODO catch and report loops
                    if collected_natures.insert(parent_nature_item_tree) {
                        self.collect_nature_attrs(id, parent_scope, collected_natures)
                    }

                    let scopes: &mut [_] = &mut self.map.scopes.as_mut();
                    let (scope, parent_scope) = scopes.pick2_mut(
                        scope.local_scope.into(),
                        parent_nature_scope.local_scope.into(),
                    );

                    for (name, def) in &parent_scope.declarations {
                        scope.declarations.entry(name.clone()).or_insert(*def);
                    }
                }
                Err(err) => todo!("ERROR {:?}", err),
            }
        }
    }

    fn collect(&mut self) {
        let root_scope = self.map.scopes.push_and_get_key(Scope {
            origin: ScopeOrigin::Root,
            parent: None,
            children: HashMap::new(),
            visible_items: HashMap::new(),
            declarations: HashMap::new(),
            duplicate_children: Vec::new(),
        });

        debug_assert_eq!(root_scope, self.map.root());

        let mut natures = Vec::new();
        // let mut disciplines = Vec::new();

        for item in &*self.tree.top_level {
            match *item {
                RootItem::Module(module) => self.collect_module(module, root_scope),
                RootItem::Nature(nature) => {
                    let id = self.collect_nature(nature, root_scope);
                    natures.push(id)
                }
                RootItem::Discipline(discipline) => self.collect_discipline(discipline, root_scope),
            }
        }

        // No parent to inherint from
        self.map.scopes[root_scope].visible_items = self.map[root_scope].declarations.clone();

        let mut processed_natures = BitSet::new_empty(self.tree.data.natures.len());
        for nature in natures {
            if processed_natures.insert(nature.lookup(self.db).id) {
                self.collect_nature_attrs(nature, root_scope, &mut processed_natures)
            }
        }

        self.insert_prelude();
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

    fn insert_scope(
        &mut self,
        parent: LocalScopeId,
        scope: LocalScopeId,
        name: Name,
        id: impl Into<ScopeDefItem>,
    ) {
        self.insert_decl(parent, name.clone(), id);
        if let Some(duplicate) = self.map.scopes[parent].children.insert(name, scope) {
            // No need for diagnostic here already emitted in insert_item_decl
            // Save duplicate children to a seperate a vector so that they can still be found in
            // block_scope(..)
            self.map.scopes[parent].duplicate_children.push(duplicate)
        }
    }

    fn insert_item_decl<N>(&mut self, dst: LocalScopeId, name: Name, item_tree: ItemTreeId<N>)
    where
        N: ItemTreeNode,
        ItemLoc<N>: Intern,
        <ItemLoc<N> as Intern>::ID: Into<ScopeDefItem>,
    {
        let decl =
            ItemLoc { scope: ScopeId { root_file: self.root_file, local_scope: dst }, id: item_tree }
                .intern(self.db);
        self.insert_decl(dst, name, decl)
    }

    fn insert_decl(&mut self, dst: LocalScopeId, name: Name, decl: impl Into<ScopeDefItem>) {
        if name.is_reserved() {
            todo!("ERROR {}", name)
        }
        if name.is_reserved_compat() {
            eprintln!("WARN: USED RESERVED IDENTIFIER: {}", name);
            // TODO LINT
        }
        self.insert_into_scope(dst, name, decl)
    }

    /// This function does not check wether an identifier is reservered
    /// and should only be used to handle builtin attributes such as ddt_nature for natures
    fn insert_into_scope(&mut self, dst: LocalScopeId, name: Name, decl: impl Into<ScopeDefItem>) {
        if let Some(_old_decl) = self.map.scopes[dst].declarations.insert(name.clone(), decl.into())
        {
            todo!("ERROR: already declared {}", name)
        }
    }

    /// Recruesively walk the DefMap after the colleciton completes and propgate the visisble item
    /// inside a each scope to its children. This allows name resolution to remain a single hashmap
    /// lookup everywhere. Since Scopes are always shadowing in VerilogAMS this works a follows
    /// visible_items[scope] = visible_items[parent] ∪ declarations[scope]
    /// This union can only be calculated once collection for the parent has finished and therefore
    /// requires a second tree transversal
    fn propagate_visisbile_item(&mut self, scope: LocalScopeId) {
        for child_id in self.map[scope].children.clone().values() {
            let visible_items = self.map[scope].visible_items.clone();
            let child = &mut self.map[*child_id];
            child.visible_items = visible_items;
            child.visible_items.extend(child.declarations.iter().map(|(k, v)| (k.clone(), *v)));
            self.propagate_visisbile_item(*child_id);
        }
    }

    fn new_scope(&mut self, origin: ScopeOrigin, parent: LocalScopeId) -> LocalScopeId {
        self.map.scopes.push_and_get_key(Scope {
            origin,
            parent: Some(parent),
            children: HashMap::new(),
            visible_items: HashMap::new(),
            declarations: HashMap::new(),
            duplicate_children: Vec::new(),
        })
    }
}
