use std::sync::Arc;

use basedb::FileId;
use la_arena::{Idx, RawIdx};
use syntax::ast::{self, BlockItem};

use crate::{
    db::HirDefDB,
    name::{kw, AsIdent, AsName},
    types::AsType,
    AstIdMap, Name,
};

use super::{
    BlockScope, BlockScopeItem, Branch, Discipline, DisciplineAttr, Domain, Function, FunctionArg,
    IdRange, ItemTree, ItemTreeId, ItemTreeNode, Module, Nature, NatureAttr, Net, Param, Port,
    RootItem, Var,
};

fn id<N: ItemTreeNode>(index: Idx<N>) -> ItemTreeId<N> {
    index
}

fn next_id<N>(arena: &la_arena::Arena<N>) -> Idx<N> {
    Idx::from_raw(RawIdx::from(arena.len() as u32))
}

fn is_input(direction: &Option<ast::Direction>) -> bool {
    direction.as_ref().map_or(false, |it| it.input_token().is_some() || it.inout_token().is_some())
}

fn is_output(direction: &Option<ast::Direction>) -> bool {
    direction.as_ref().map_or(false, |it| it.output_token().is_some() || it.inout_token().is_some())
}

pub(super) struct Ctx<'a> {
    db: &'a dyn HirDefDB,
    tree: ItemTree,
    source_ast_id_map: Arc<AstIdMap>,
}

impl<'a> Ctx<'a> {
    pub(super) fn new(db: &'a dyn HirDefDB, file: FileId) -> Self {
        Self { db, tree: ItemTree::default(), source_ast_id_map: db.ast_id_map(file) }
    }

    pub(super) fn lower_root_items(mut self, file: &ast::SourceFile) -> ItemTree {
        self.tree.top_level = file.items().filter_map(|it| self.lower_root_item(it)).collect();
        self.tree
    }

    fn lower_root_item(&mut self, item: ast::Item) -> Option<RootItem> {
        let item = match item {
            ast::Item::DisciplineDecl(discipline) => self.lower_discipline(discipline)?.into(),
            ast::Item::NatureDecl(nature) => self.lower_nature(nature)?.into(),
            ast::Item::ModuleDecl(module) => self.lower_module(module)?.into(),
        };
        Some(item)
    }

    fn lower_discipline(&mut self, decl: ast::DisciplineDecl) -> Option<ItemTreeId<Discipline>> {
        let name = decl.name()?.as_name();
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        let mut potential = None;
        let mut flow = None;
        let mut domain = None;
        let attr_start = next_id(&self.tree.data.discipline_attrs);
        for attr in decl.discipline_attrs() {
            if let Some(name) = attr.name().and_then(|path| path.as_ident()) {
                use kw::raw as kw;
                match &*name {
                    kw::potential if potential.is_none() => {
                        if let Some(name) = attr.val().and_then(|e| e.as_ident()) {
                            potential = Some(name);
                            continue;
                        }
                    }
                    kw::flow if flow.is_none() => {
                        if let Some(name) = attr.val().and_then(|e| e.as_ident()) {
                            flow = Some(name);
                            continue;
                        }
                    }
                    kw::domain if domain.is_none() => {
                        match attr.val().and_then(|e| e.as_ident()).as_deref() {
                            Some(kw::continuous) => {
                                domain = Some(Domain::Continous);
                                continue;
                            }
                            Some(kw::discrete) => {
                                domain = Some(Domain::Discrete);
                                continue;
                            }
                            _ => (),
                        }
                    }

                    _ => (),
                };

                // If an attr is not detected as an inbuilt one we add it to the discipline anyway.
                // This is mainly used to get duplication errors for free during def map
                // construction
                self.tree
                    .data
                    .discipline_attrs
                    .alloc(DisciplineAttr { name, ast_id: self.source_ast_id_map.ast_id(&attr) });
            }
        }
        let attr_end = next_id(&self.tree.data.discipline_attrs);
        let res = Discipline {
            ast_id,
            name,
            potential,
            flow,
            attrs: IdRange::new(attr_start..attr_end),
            domain,
        };
        Some(id(self.tree.data.disciplines.alloc(res)))
    }

    fn lower_nature(&mut self, decl: ast::NatureDecl) -> Option<ItemTreeId<Nature>> {
        let name = decl.name()?.as_name();
        let parent = decl.parent().map(|it| it.as_name());
        let attr_start = next_id(&self.tree.data.nature_attrs);
        let mut access = None;
        let mut ddt_nature = None;
        let mut idt_nature = None;
        for attr in decl.nature_attrs() {
            if let Some(name) = attr.name().map(|name| name.as_name()) {
                use kw::raw as kw;
                match &*name {
                    kw::access if access.is_none() => {
                        if let Some(name) = attr.val().and_then(|e| e.as_ident()) {
                            access = Some(name);
                            continue;
                        }
                    }
                    kw::ddt_nature if ddt_nature.is_none() => {
                        if let Some(name) = attr.val().and_then(|e| e.as_ident()) {
                            ddt_nature = Some(name);
                            continue;
                        }
                    }
                    kw::idt_nature if idt_nature.is_none() => {
                        if let Some(name) = attr.val().and_then(|e| e.as_ident()) {
                            idt_nature = Some(name);
                            continue;
                        }
                    }

                    _ => (),
                };

                // If an attr is not detected as an inbuilt one we add it to the discipline anyway.
                // This is mainly used to get duplication errors for free during def map
                // construction
                self.tree
                    .data
                    .nature_attrs
                    .alloc(NatureAttr { name, ast_id: self.source_ast_id_map.ast_id(&attr) });
            }
        }
        let attr_end = next_id(&self.tree.data.nature_attrs);
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        let res = Nature {
            ast_id,
            name,
            parent,
            access,
            ddt_nature,
            idt_nature,
            attrs: IdRange::new(attr_start..attr_end),
        };
        Some(id(self.tree.data.natures.alloc(res)))
    }

    fn lower_module(&mut self, decl: ast::ModuleDecl) -> Option<ItemTreeId<Module>> {
        let name = decl.name()?.as_name();

        let (head_ports, exptected_ports) = self.lower_module_ports(decl.ports());

        let port_start = next_id(&self.tree.data.ports);
        let function_start = next_id(&self.tree.data.functions);
        let net_start = next_id(&self.tree.data.nets);
        let branch_start = next_id(&self.tree.data.branches);

        let scope_items = self.lower_module_items(decl.module_items());

        let port_end = next_id(&self.tree.data.ports);
        let function_end = next_id(&self.tree.data.functions);
        let net_end = next_id(&self.tree.data.nets);
        let branch_end = next_id(&self.tree.data.branches);

        let ast_id = self.source_ast_id_map.ast_id(&decl);

        let res = Module {
            name,

            exptected_ports,
            head_ports,

            scope_items,
            body_ports: IdRange::new(port_start..port_end),
            nets: IdRange::new(net_start..net_end),
            branches: IdRange::new(branch_start..branch_end),
            functions: IdRange::new(function_start..function_end),

            ast_id,
        };
        Some(id(self.tree.data.modules.alloc(res)))
    }

    fn lower_module_items(
        &mut self,
        items: ast::AstChildren<ast::ModuleItem>,
    ) -> Vec<BlockScopeItem> {
        let mut block_scope_items = Vec::with_capacity(512);
        for item in items {
            match item {
                ast::ModuleItem::BodyPortDecl(decl) => {
                    if let Some(decl) = decl.port_decl() {
                        self.lower_port_decl(decl);
                    }
                }
                ast::ModuleItem::NetDecl(decl) => {
                    self.lower_net_decl(decl);
                }
                ast::ModuleItem::AnalogBehaviour(behaviour) => {
                    if let Some(stmt) = behaviour.stmt() {
                        struct AddRootScope<'a>(&'a mut Vec<BlockScopeItem>);
                        impl AddScope for AddRootScope<'_> {
                            fn add_scope(&mut self, scope: ItemTreeId<BlockScope>) {
                                self.0.push(BlockScopeItem::Scope(scope))
                            }
                        }
                        self.lower_stmt(stmt, &mut AddRootScope(&mut block_scope_items));
                    }
                }
                ast::ModuleItem::VarDecl(var) => {
                    self.lower_var(var, |var| {
                        block_scope_items.push(BlockScopeItem::Variable(var))
                    });
                }
                ast::ModuleItem::ParamDecl(param) => {
                    self.lower_param(param, |param| {
                        block_scope_items.push(BlockScopeItem::Parameter(param))
                    });
                }
                ast::ModuleItem::Function(fun) => {
                    self.lower_fun(fun);
                }
                ast::ModuleItem::BranchDecl(branch) => self.lower_branch(branch),
            };
        }
        block_scope_items
    }

    fn lower_fun(&mut self, fun: ast::Function) {
        let var_start = next_id(&self.tree.data.variables);
        let param_start = next_id(&self.tree.data.parameters);
        let args_start = next_id(&self.tree.data.function_args);

        for item in fun.function_items() {
            match item {
                ast::FunctionItem::ParamDecl(decl) => self.lower_param(decl, |_| ()),
                ast::FunctionItem::VarDecl(decl) => self.lower_var(decl, |_| ()),
                ast::FunctionItem::FunctionArg(arg) => {
                    for name in arg.names() {
                        self.tree.data.function_args.alloc(FunctionArg {
                            name: name.as_name(),
                            is_input: is_input(&arg.direction()),
                            is_output: is_output(&arg.direction()),
                            ast_id: self.source_ast_id_map.ast_id(&arg),
                        });
                    }
                }
                ast::FunctionItem::Stmt(_) => (), // No need to visit this, named blocks are not allowed here
            }
        }

        let args_end = next_id(&self.tree.data.function_args);
        let var_end = next_id(&self.tree.data.variables);
        let param_end = next_id(&self.tree.data.parameters);

        if let Some(name) = fun.name() {
            let fun = Function {
                name: name.as_name(),
                ty: fun.ty().as_type(),
                args: IdRange::new(args_start..args_end),
                params: IdRange::new(param_start..param_end),
                vars: IdRange::new(var_start..var_end),
                ast_id: self.source_ast_id_map.ast_id(&fun),
            };
            self.tree.data.functions.alloc(fun);
        }
    }

    fn lower_branch(&mut self, decl: ast::BranchDecl) {
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        for name in decl.names() {
            let branch = Branch { name: name.as_name(), ast_id };
            self.tree.data.branches.alloc(branch);
        }
    }

    fn lower_module_ports(
        &mut self,
        ports: ast::AstChildren<ast::ModulePort>,
    ) -> (IdRange<Port>, Vec<Name>) {
        let port_start = next_id(&self.tree.data.ports);
        let expected_ports = ports
            .filter_map(|port_or_name| match port_or_name {
                ast::ModulePort::Name(name) => Some(name.as_name()),
                ast::ModulePort::PortDecl(decl) => {
                    self.lower_port_decl(decl);
                    None
                }
            })
            .collect();
        let port_end = next_id(&self.tree.data.ports);
        (IdRange::new(port_start..port_end), expected_ports)
    }

    fn lower_port_decl(&mut self, decl: ast::PortDecl) {
        let discipline = decl.discipline().map(|it| it.as_name());
        let direction = decl.direction();

        let ast_id = self.source_ast_id_map.ast_id(&decl);
        for name in decl.names() {
            self.tree.data.ports.alloc(Port {
                name: name.as_name(),
                discipline: discipline.clone(),
                is_input: is_input(&direction),
                is_output: is_output(&direction),
                ast_id,
            });
        }
    }

    fn lower_net_decl(&mut self, decl: ast::NetDecl) {
        let discipline = decl.discipline().map(|it| it.as_name());
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        for name in decl.names() {
            self.tree.data.nets.alloc(Net {
                name: name.as_name(),
                discipline: discipline.clone(),
                ast_id,
            });
        }
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt, add_scope: &mut impl AddScope) {
        if let ast::Stmt::BlockStmt(block) = stmt {
            if let Some(scope_name) = block.block_scope().and_then(|it| Some(it.name()?.as_name()))
            {
                let param_start = next_id(&self.tree.data.parameters);
                let var_start = next_id(&self.tree.data.variables);
                let mut child_scopes = Vec::with_capacity(4);

                for item in block.body() {
                    struct AddBlockScope<'a>(&'a mut Vec<ItemTreeId<BlockScope>>);
                    impl AddScope for AddBlockScope<'_> {
                        fn add_scope(&mut self, scope: ItemTreeId<BlockScope>) {
                            self.0.push(scope)
                        }
                    }
                    self.lower_block_item(item, &mut AddBlockScope(&mut child_scopes));
                }

                let param_end = next_id(&self.tree.data.parameters);
                let var_end = next_id(&self.tree.data.variables);

                let scope = BlockScope {
                    ast_id: self.source_ast_id_map.ast_id(&block),
                    name: scope_name,
                    parameters: IdRange::new(param_start..param_end),
                    variables: IdRange::new(var_start..var_end),
                    scopes: child_scopes,
                };
                add_scope.add_scope(id(self.tree.data.block_scopes.alloc(scope)))
            } else {
                // For unnamed blocks add the scope to the current active scope
                for item in block.body() {
                    self.lower_block_item(item, add_scope);
                }
            }
        }
    }

    fn lower_block_item(&mut self, item: BlockItem, add_scope: &mut impl AddScope) {
        match item {
            ast::BlockItem::VarDecl(var) => {
                self.lower_var(var, |_| ());
            }
            ast::BlockItem::ParamDecl(param) => {
                self.lower_param(param, |_| ());
            }
            ast::BlockItem::Stmt(stmt) => self.lower_stmt(stmt, add_scope),
        }
    }

    fn lower_var(&mut self, decl: ast::VarDecl, mut add_var: impl FnMut(ItemTreeId<Var>)) {
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        let ty = decl.ty().as_type();
        for name in decl.vars().filter_map(|it| it.name()) {
            let var = Var { name: name.as_name(), ast_id, ty: ty.clone() };
            let id = id(self.tree.data.variables.alloc(var));
            add_var(id)
        }
    }

    fn lower_param(&mut self, decl: ast::ParamDecl, mut add_var: impl FnMut(ItemTreeId<Param>)) {
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        let ty = decl.ty().as_type();
        for name in decl.paras().filter_map(|it| it.name()) {
            let param = Param { name: name.as_name(), ast_id, ty: ty.clone() };
            let id = id(self.tree.data.parameters.alloc(param));
            add_var(id)
        }
    }
}

trait AddScope {
    fn add_scope(&mut self, scope: ItemTreeId<BlockScope>);
}
