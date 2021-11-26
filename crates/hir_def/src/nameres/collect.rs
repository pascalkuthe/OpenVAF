use std::sync::Arc;

use ahash::AHashMap as HashMap;
use arena::Arena;
use basedb::{AstId, FileId};
use syntax::{ast, name::Name};

use crate::{
    db::HirDefDB,
    item_tree::Function,
    item_tree::{
        BlockScopeItem, FunctionItem, ItemTree, ItemTreeId, ItemTreeNode, Module, ModuleItem,
        RootItem,
    },
    BlockId, BlockLoc, DisciplineLoc, FunctionArgLoc, FunctionId, FunctionLoc, Intern, ItemLoc,
    Lookup, ModuleLoc, NatureAttrLoc, NatureLoc, NodeLoc, ScopeId,
};

use super::{
    diagnostics::DefDiagnostic, DefMap, DefMapSource, LocalScopeId, Scope, ScopeDefItem,
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
            src: DefMapSource::Root,
            diagnostics: Vec::new(),
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

    let mut collector = DefCollector {
        map: DefMap {
            scopes: Arena::with_capacity(tree.data.modules.len() + 1),
            // nodes: Arena::with_capacity(tree.data.nets.len()),
            src: DefMapSource::Function(function),
            entry_scope: LocalScopeId::from(0u32), // This will be changed once the scope has been created
            diagnostics: Vec::new(),
        },
        tree,
        db,
        root_file,
    };

    collector.collect_function_map(id, local_scope, function);

    Arc::new(collector.map)
}

pub fn collect_block_map(db: &dyn HirDefDB, block: BlockId) -> Option<Arc<DefMap>> {
    let BlockLoc { ast, parent } = block.lookup(db);

    let tree = &db.item_tree(parent.root_file);
    let items = &tree.block_scope(ast).scope_items;

    if items.is_empty() {
        return None;
    }

    let mut collector = DefCollector {
        map: DefMap {
            scopes: Arena::with_capacity(1),
            src: DefMapSource::Block(block),
            entry_scope: LocalScopeId::from(0u32),
            diagnostics: Vec::new(),
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
    fn collect_function_map(
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

        for item in &self.tree[item_tree].items {
            match *item {
                FunctionItem::Scope(ast) => self.collect_block_scope(scope, ast),

                FunctionItem::Parameter(id) => {
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
                FunctionItem::Variable(id) => {
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
                FunctionItem::FunctionArg(arg) => {
                    let id = FunctionArgLoc { fun: id, id: arg }.intern(self.db);
                    self.insert_decl(scope, self.tree[item_tree].args[arg].name.clone(), id)
                }
            }
        }
        self.map.entry_scope = scope;
        debug_assert_eq!(self.map.entry(), scope);
    }

    fn collect_block_map(&mut self, id: BlockId, items: &[BlockScopeItem]) {
        let scope = self.new_root_scope(id.into());
        debug_assert_eq!(scope, self.map.entry());
        for item in items {
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
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
                BlockScopeItem::Variable(id) => {
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
            }
        }
    }

    // Verilog-ams standard does not specify any way to access user-defined discipline attributes
    // I am guessing this is an oversight but until this is clarified we are not adding this
    // TODO talk to committee about discipline attributes

    fn collect_root_map(&mut self) {
        let root_scope = self.new_root_scope(ScopeOrigin::Root);

        debug_assert_eq!(root_scope, self.map.entry());
        debug_assert_eq!(root_scope, self.map.root());

        for item in &*self.tree.top_level {
            match *item {
                RootItem::Module(module) => self.collect_module(module, root_scope),
                RootItem::Nature(nature) => {
                    let id = NatureLoc { root_file: self.root_file, id: nature }.intern(self.db);
                    self.insert_decl(root_scope, self.tree[nature].name.clone(), id);
                    if let Some((name, attr)) = self.tree[nature].access.clone() {
                        self.insert_decl(
                            root_scope,
                            name,
                            ScopeDefItem::NatureAccess(
                                NatureAttrLoc { nature: id, id: attr }.intern(self.db).into(),
                            ),
                        )
                    }
                }
                RootItem::Discipline(discipline) => {
                    self.insert_decl(
                        root_scope,
                        self.tree[discipline].name.clone(),
                        DisciplineLoc { root_file: self.root_file, id: discipline }.intern(self.db),
                    );
                }
            }
        }
    }

    fn collect_module(&mut self, item_tree: ItemTreeId<Module>, parent_scope: LocalScopeId) {
        let module_id = ModuleLoc { id: item_tree, scope: self.next_scope() }.intern(self.db);

        let scope = self.new_scope(ScopeOrigin::Module(module_id), parent_scope);
        let module = &self.tree[item_tree];

        self.insert_scope(parent_scope, scope, module.name.clone(), module_id);

        for item in &module.items {
            match *item {
                ModuleItem::Scope(ast) => self.collect_block_scope(scope, ast),

                ModuleItem::Node(id) => self.insert_decl(
                    scope,
                    module.nodes[id].name.clone(),
                    NodeLoc { module: module_id, id }.intern(self.db),
                ),
                ModuleItem::Branch(id) => {
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
                ModuleItem::Parameter(id) => {
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
                ModuleItem::Variable(id) => {
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
                ModuleItem::Function(id) => {
                    self.insert_item_decl(scope, self.tree[id].name.clone(), id)
                }
            }
        }
    }

    fn collect_block_scope(&mut self, scope: LocalScopeId, ast: AstId<ast::BlockStmt>) {
        let loc = BlockLoc {
            ast,
            parent: ScopeId { root_file: self.root_file, local_scope: scope, src: self.map.src },
        };
        let id = loc.intern(self.db);
        self.insert_decl(
            scope,
            self.tree
                .block_scope(ast)
                .name
                .clone()
                .expect("Item tree must only contain named blocks"),
            id,
        );
    }

    fn next_scope(&self) -> ScopeId {
        ScopeId {
            root_file: self.root_file,
            local_scope: self.map.scopes.next_key(),
            src: self.map.src,
        }
    }

    fn new_scope(&mut self, origin: ScopeOrigin, parent: LocalScopeId) -> LocalScopeId {
        self.map.scopes.push_and_get_key(Scope {
            origin,
            parent: Some(parent),
            children: HashMap::new(),
            declarations: HashMap::new(),
        })
    }

    fn new_root_scope(&mut self, origin: ScopeOrigin) -> LocalScopeId {
        self.map.scopes.push_and_get_key(Scope {
            origin,
            parent: None,
            children: HashMap::new(),
            declarations: HashMap::new(),
        })
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
        let decl = decl.into();
        if let Some(old_decl) = self.map.scopes[dst].declarations.insert(name.clone(), decl) {
            self.map.diagnostics.push(DefDiagnostic::AlreadyDeclard {
                new: decl,
                old: old_decl,
                name,
            })
        }
    }
}
