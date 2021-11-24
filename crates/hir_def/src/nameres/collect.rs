use std::sync::Arc;

use ahash::AHashMap as HashMap;
use arena::{Arena, Idx};
use basedb::FileId;
use bitset::BitSet;

use crate::{
    db::HirDefDB,
    item_tree::{BlockScopeItem, ItemTree, ItemTreeId, ItemTreeNode, Module, RootItem},
    item_tree::{Discipline, Function, Net, Port},
    name::kw,
    nameres::NatureAccess,
    nameres::Node,
    BlockId, BlockLoc, DisciplineAttrLoc, DisciplineLoc, FunctionArgLoc, FunctionId, FunctionLoc,
    Intern, ItemLoc, Lookup, ModuleLoc, Name, Nature, NatureAttrLoc, NatureId, NatureLoc, NodeId,
    NodeLoc, ScopeId,
};
use stdx::vec::SliceExntesions;

use super::{
    DefMap, DefMapSource, FunctionArg, LocalScopeId, Scope, ScopeDefItem, ScopeDefItemKind,
    ScopeOrigin,
};

pub fn collect_root_def_map(db: &dyn HirDefDB, root_file: FileId) -> Arc<DefMap> {
    let tree = &db.item_tree(root_file);
    let scope_cnt = tree.data.natures.len() + tree.data.disciplines.len() + tree.data.modules.len();

    let mut collector = DefCollector {
        map: DefMap {
            scopes: Arena::with_capacity(scope_cnt),
            // nodes: Arena::with_capacity(tree.data.nets.len()),
            entry_scope: LocalScopeId::from(0u32),
            args: Arena::new(),
            src: DefMapSource::Root,
        },
        tree,
        db,
        root_file,
    };

    collector.collect_root_map();

    Arc::new(collector.map)
}

pub fn collect_function_map(db: &dyn HirDefDB, function: FunctionId) -> Arc<DefMap> {
    let FunctionLoc { scope: ScopeId { root_file, local_scope, .. }, id } = function.lookup(db);
    let tree = &db.item_tree(root_file);

    let args = tree[id].args.clone().map(|arg| FunctionArg { direction: arg, var: None }).collect();

    let mut collector = DefCollector {
        map: DefMap {
            scopes: Arena::with_capacity(tree.data.modules.len() + 1),
            // nodes: Arena::with_capacity(tree.data.nets.len()),
            src: DefMapSource::Function(function),
            entry_scope: LocalScopeId::from(0u32), // This will be changed once the scope has been created
            args,
        },
        tree,
        db,
        root_file,
    };

    collector.collect_function(id, local_scope, function);

    Arc::new(collector.map)
}

pub fn collect_block_map(db: &dyn HirDefDB, block: BlockId) -> Option<Arc<DefMap>> {
    let BlockLoc { ast, parent } = block.lookup(db);

    let tree = &db.item_tree(parent.root_file);
    // if !tree.blocks.contains_key(&ast) {
    //     let block = db.ast_id_map(parent.root_file).get(ast);
    //     let block = block.to_node(db.parse(parent.root_file).tree().syntax());
    //     panic!("Missing block \n{}", block)
    // }
    let items = &tree.block_scope(ast).scope_items;

    if items.is_empty() {
        return None;
    }

    let mut collector = DefCollector {
        map: DefMap {
            scopes: Arena::with_capacity(1),
            src: DefMapSource::Block(block),
            entry_scope: LocalScopeId::from(0u32),
            args: Arena::new(),
        },
        tree,
        db,
        root_file: parent.root_file,
    };

    collector.collect_block_map(block, items);

    Some(Arc::new(collector.map))
}

struct DefCollector<'a> {
    map: DefMap,
    tree: &'a ItemTree,
    db: &'a dyn HirDefDB,
    root_file: FileId,
}

impl DefCollector<'_> {
    fn next_scope(&self) -> ScopeId {
        ScopeId {
            root_file: self.root_file,
            local_scope: self.map.scopes.next_key(),
            src: self.map.src,
        }
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

    fn insert_port(&mut self, id: ItemTreeId<Port>, local_scope: LocalScopeId) {
        let port = &self.tree[id];
        debug_assert_eq!(self.map.src, DefMapSource::Root);

        let node = Node {
            port: Some(id),
            discipline: port.discipline.as_ref().map(|_| id.into()),
            gnd_declaration: port.is_gnd.then(|| id.into()),
            scope: ScopeId { root_file: self.root_file, local_scope, src: self.map.src },
        };

        self.insert_or_update_node(node, local_scope, &port.name)
    }

    fn insert_net(&mut self, id: ItemTreeId<Net>, local_scope: LocalScopeId) {
        let net = &self.tree[id];

        debug_assert_eq!(self.map.src, DefMapSource::Root);
        let node = Node {
            port: None,
            discipline: net.discipline.as_ref().map(|_| id.into()),
            gnd_declaration: net.is_gnd.then(|| id.into()),
            scope: ScopeId { root_file: self.root_file, local_scope, src: self.map.src },
        };

        self.insert_or_update_node(node, local_scope, &net.name)
    }

    fn insert_or_update_node(&mut self, node: Node, scope: LocalScopeId, name: &Name) {
        match self.resolve_existing_decl_in_scope::<NodeId>(scope, name) {
            Some(res) => {
                let res = res.lookup(self.db).id;
                let existing_node = &mut self.map.scopes[scope].nodes[res];
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
                let id = self.map[scope].nodes.push_and_get_key(node);
                let node = NodeLoc {
                    scope: ScopeId {
                        root_file: self.root_file,
                        local_scope: scope,
                        src: self.map.src,
                    },
                    id,
                }
                .intern(self.db);
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

        for item in &module.scope_items {
            self.lower_scope_item(item, scope)
        }
    }

    fn collect_function(
        &mut self,
        item_tree: ItemTreeId<Function>,
        parent_module: LocalScopeId,
        id: FunctionId,
    ) {
        debug_assert_eq!(self.map.src, DefMapSource::Function(id));

        let root_def_map = self.db.def_map(self.root_file);

        let root = self.new_root_scope(ScopeOrigin::Root);
        debug_assert_eq!(self.map.root(), root);

        // First copy the modules and their parametes since these are the only declarations outside
        // of the function itself that are accessible insdie an analog funciton
        let main_root_scope = &root_def_map.scopes[root_def_map.root()];

        let mut parent_module_ = None;

        for (module_name, scope_id) in main_root_scope.children.iter() {
            let scope = &root_def_map[*scope_id];

            if let ScopeOrigin::Module(module) = scope.origin {
                let declarations = scope
                    .declarations
                    .iter()
                    .filter_map(|(name, decl)| {
                        matches!(decl, ScopeDefItem::ParamId(_) | ScopeDefItem::FunctionId(_))
                            .then(|| (name.clone(), *decl))
                    })
                    .collect();

                let scope = Scope {
                    origin: scope.origin,
                    parent: scope.parent,
                    children: HashMap::new(),
                    declarations,
                    nodes: Arena::new(),
                };

                debug_assert_eq!(scope.parent, Some(root_def_map.root()));
                debug_assert_eq!(scope.parent, Some(root));

                if *scope_id == parent_module {
                    parent_module_ = Some(parent_module);
                }

                let scope = self.map.scopes.push_and_get_key(scope);
                self.map.scopes[root].children.insert(module_name.clone(), scope);
                self.map.scopes[root].declarations.insert(module_name.clone(), module.into());
            }
        }

        let parent_module = parent_module_.expect("parent module was not among the root modules");
        let scope = self.new_scope(ScopeOrigin::Function(id), parent_module);
        self.map[scope]
            .declarations
            .insert(self.tree[item_tree].name.clone(), ScopeDefItem::FunctionReturn(id));

        for arg_pos in self.map.args.keys() {
            let id = FunctionArgLoc { fun: id, arg_pos }.intern(self.db);
            self.insert_decl(scope, self.tree[self.map.args[arg_pos].direction].name.clone(), id);
        }
        for item in &self.tree[item_tree].scope_items {
            if let BlockScopeItem::Variable(var) = item {
                if let Some(ScopeDefItem::FunctionArgId(arg)) =
                    self.map.scopes[scope].declarations.get(&self.tree[*var].name)
                {
                    let arg_pos = arg.lookup(self.db).arg_pos;
                    if self.map.args[arg_pos].var.is_none() {
                        self.map.args[arg_pos].var = Some(*var);
                        // Automatically emit an duplicate error otherwise
                        continue;
                    }
                }
            }
            self.lower_scope_item(item, scope);
        }
        self.map.entry_scope = scope;
        debug_assert_eq!(self.map.entry(), scope);
    }

    fn lower_scope_item(&mut self, item: &BlockScopeItem, scope: LocalScopeId) {
        match *item {
            BlockScopeItem::Scope(ast) => {
                if let Some(name) = &self.tree.block_scope(ast).name {
                    let loc = BlockLoc {
                        ast,
                        parent: ScopeId {
                            root_file: self.root_file,
                            local_scope: scope,
                            src: self.map.src,
                        },
                    };
                    let id = loc.intern(self.db);
                    self.insert_decl(scope, name.clone(), id);
                } else {
                    debug_assert!(false)
                }
            }
            BlockScopeItem::Parameter(id) => {
                // let file_map = self.db.ast_id_map(self.root_file);
                // let param = file_map.get(self.tree[*id].ast_id);
                // let param = param.to_node(&self.db.parse(self.root_file).syntax_node());
                // use syntax::AstNode;
                // error!("Parameter: {}", param.syntax().text());
                self.insert_item_decl(scope, self.tree[id].name.clone(), id)
            }
            BlockScopeItem::Variable(id) => {
                // let file_map = self.db.ast_id_map(self.root_file);
                // let param = file_map.get(self.tree[*id].ast_id);
                // let param = param.to_node(&self.db.parse(self.root_file).syntax_node());
                // use syntax::AstNode;
                // error!("Var: {}", param.syntax().text());
                self.insert_item_decl(scope, self.tree[id].name.clone(), id)
            }
        }
    }

    fn collect_block_map(&mut self, id: BlockId, items: &[BlockScopeItem]) {
        let scope = self.new_root_scope(id.into());
        debug_assert_eq!(scope, self.map.entry());
        for item in items {
            self.lower_scope_item(item, scope)
        }
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
                        scope: ScopeId {
                            root_file: self.root_file,
                            local_scope,
                            src: self.map.src,
                        },
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
            match self.map.resolve_local_item_in_scope::<NatureId>(parent_scope, ddt_nature) {
                Ok(id) => self.insert_into_scope(local_scope, kw::ddt_nature, id),
                Err(err) => todo!("ERROR {:?}", err),
            }
        }

        if let Some(idt_nature) = &nature.idt_nature {
            match self.map.resolve_local_item_in_scope::<NatureId>(parent_scope, idt_nature) {
                Ok(id) => self.insert_into_scope(local_scope, kw::idt_nature, id),
                Err(err) => todo!(
                    "ERROR: idt_nature {} not found in {:?}: {:?}",
                    idt_nature,
                    self.map[parent_scope].declarations,
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
            match self.map.resolve_local_item_in_scope::<NatureId>(parent_scope, parent) {
                Ok(id) => {
                    let NatureLoc { scope: parent_nature_scope, id: parent_nature_item_tree } =
                        id.lookup(self.db);
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

    fn new_root_scope(&mut self, origin: ScopeOrigin) -> LocalScopeId {
        self.map.scopes.push_and_get_key(Scope {
            origin,
            parent: None,
            children: HashMap::new(),
            declarations: HashMap::new(),
            nodes: Arena::new(),
        })

    }

    fn collect_root_map(&mut self) {
        let root_scope = self.new_root_scope(ScopeOrigin::Root);

        debug_assert_eq!(root_scope, self.map.entry());
        debug_assert_eq!(root_scope, self.map.root());
        // TODO builtin scope as lazy instead
        // insert_builtin_scope(&mut self.map.scopes[root_scope].declarations);

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

        let mut processed_natures = BitSet::new_empty(self.tree.data.natures.len());
        for nature in natures {
            if processed_natures.insert(nature.lookup(self.db).id) {
                self.collect_nature_attrs(nature, root_scope, &mut processed_natures)
            }
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
        self.map.scopes[parent].children.entry(name).or_insert(scope);
    }

    fn insert_item_decl<N>(&mut self, dst: LocalScopeId, name: Name, item_tree: ItemTreeId<N>)
    where
        N: ItemTreeNode,
        ItemLoc<N>: Intern,
        <ItemLoc<N> as Intern>::ID: Into<ScopeDefItem>,
    {
        let decl = ItemLoc {
            scope: ScopeId { root_file: self.root_file, local_scope: dst, src: self.map.src },
            id: item_tree,
        }
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
        let decl = decl.into();
        if let Some(old_decl) = self.map.scopes[dst].declarations.insert(name.clone(), decl) {
            todo!("ERROR: already declared {} {:?} {:?}", name, old_decl, decl)
        }
    }

    fn new_scope(&mut self, origin: ScopeOrigin, parent: LocalScopeId) -> LocalScopeId {
        self.map.scopes.push_and_get_key(Scope {
            origin,
            parent: Some(parent),
            children: HashMap::new(),
            // visible_items: HashMap::new(),
            declarations: HashMap::new(),
            nodes: Arena::new(),
        })
    }
}
