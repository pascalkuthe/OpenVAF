use std::sync::Arc;

use basedb::FileId;
use data_structures::arena::IdxRange;
use syntax::ast::{self, BlockItem};
use tracing::trace;

use crate::{
    db::HirDefDB,
    name::{kw, AsIdent, AsName},
    types::AsType,
    AstIdMap, Name, Path,
};

use super::{
    BlockScope, BlockScopeItem, Branch, BranchKind, Discipline, DisciplineAttr, Domain, Function,
    FunctionArg, ItemTree, ItemTreeId, Module, Nature, NatureAttr, Net, Param, Port, RootItem, Var,
};

fn is_input(direction: &Option<ast::Direction>) -> bool {
    direction.as_ref().map_or(false, |it| it.input_token().is_some() || it.inout_token().is_some())
}

fn is_output(direction: &Option<ast::Direction>) -> bool {
    direction.as_ref().map_or(false, |it| it.output_token().is_some() || it.inout_token().is_some())
}

pub(super) struct Ctx {
    tree: ItemTree,
    source_ast_id_map: Arc<AstIdMap>,
}

impl Ctx {
    pub(super) fn new(db: &dyn HirDefDB, file: FileId) -> Self {
        Self { tree: ItemTree::default(), source_ast_id_map: db.ast_id_map(file) }
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
        let attr_start = self.tree.data.discipline_attrs.len_idx();
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
                            _ => {
                                trace!(
                                    discipline = debug(decl.name()),
                                    val = debug(attr.val()),
                                    "Value for domnain attr is invalid!"
                                );
                            }
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
                    .push(DisciplineAttr { name, ast_id: self.source_ast_id_map.ast_id(&attr) });
            }
        }
        let attr_end = self.tree.data.discipline_attrs.len_idx();
        let res = Discipline {
            ast_id,
            name,
            potential,
            flow,
            attrs: IdxRange::new(attr_start..attr_end),
            domain,
        };
        Some(self.tree.data.disciplines.push(res))
    }

    fn lower_nature(&mut self, decl: ast::NatureDecl) -> Option<ItemTreeId<Nature>> {
        let name = decl.name()?.as_name();
        let parent = decl.parent().map(|it| it.as_name());
        let attr_start = self.tree.data.nature_attrs.len_idx();
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
                    .push(NatureAttr { name, ast_id: self.source_ast_id_map.ast_id(&attr) });
            }
        }
        let attr_end = self.tree.data.nature_attrs.len_idx();
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        let res = Nature {
            ast_id,
            name,
            parent,
            access,
            ddt_nature,
            idt_nature,
            attrs: IdxRange::new(attr_start..attr_end),
        };
        Some(self.tree.data.natures.push(res))
    }

    fn lower_module(&mut self, decl: ast::ModuleDecl) -> Option<ItemTreeId<Module>> {
        let name = decl.name()?.as_name();

        let (head_ports, exptected_ports) = self.lower_module_ports(decl.ports());

        let port_start = self.tree.data.ports.len_idx();
        let function_start = self.tree.data.functions.len_idx();
        let net_start = self.tree.data.nets.len_idx();
        let branch_start = self.tree.data.branches.len_idx();

        let scope_items = self.lower_module_items(decl.module_items());

        let port_end = self.tree.data.ports.len_idx();
        let function_end = self.tree.data.functions.len_idx();
        let net_end = self.tree.data.nets.len_idx();
        let branch_end = self.tree.data.branches.len_idx();

        let ast_id = self.source_ast_id_map.ast_id(&decl);

        let res = Module {
            name,

            exptected_ports,
            head_ports,

            scope_items,
            body_ports: IdxRange::new(port_start..port_end),
            nets: IdxRange::new(net_start..net_end),
            branches: IdxRange::new(branch_start..branch_end),
            functions: IdxRange::new(function_start..function_end),

            ast_id,
        };
        Some(self.tree.data.modules.push(res))
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
                        self.lower_stmt(stmt, &mut block_scope_items);
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
        let var_start = self.tree.data.variables.len_idx();
        let param_start = self.tree.data.parameters.len_idx();
        let args_start = self.tree.data.function_args.len_idx();

        for item in fun.function_items() {
            match item {
                ast::FunctionItem::ParamDecl(decl) => self.lower_param(decl, |_| ()),
                ast::FunctionItem::VarDecl(decl) => self.lower_var(decl, |_| ()),
                ast::FunctionItem::FunctionArg(arg) => {
                    for name in arg.names() {
                        self.tree.data.function_args.push(FunctionArg {
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

        let args_end = self.tree.data.function_args.len_idx();
        let var_end = self.tree.data.variables.len_idx();
        let param_end = self.tree.data.parameters.len_idx();

        if let Some(name) = fun.name() {
            let fun = Function {
                name: name.as_name(),
                ty: fun.ty().as_type(),
                args: IdxRange::new(args_start..args_end),
                params: IdxRange::new(param_start..param_end),
                vars: IdxRange::new(var_start..var_end),
                ast_id: self.source_ast_id_map.ast_id(&fun),
            };
            self.tree.data.functions.push(fun);
        }
    }

    fn lower_branch(&mut self, decl: ast::BranchDecl) {
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        let kind = decl
            .branch_kind()
            .and_then(|kind| {
                let res = match kind {
                    ast::BranchKind::PortFlow(flow) => {
                        BranchKind::PortFlow(Path::resolve(flow.port()?)?)
                    }
                    ast::BranchKind::NodeGnd(path) => BranchKind::NodeGnd(Path::resolve(path)?),
                    ast::BranchKind::Nodes(hi, lo) => {
                        BranchKind::Nodes(Path::resolve(hi)?, Path::resolve(lo)?)
                    }
                };
                Some(res)
            })
            .unwrap_or(BranchKind::Missing);
        for name in decl.names() {
            let branch = Branch { name: name.as_name(), kind: kind.clone(), ast_id };
            self.tree.data.branches.push(branch);
        }
    }

    fn lower_module_ports(
        &mut self,
        ports: ast::AstChildren<ast::ModulePort>,
    ) -> (IdxRange<Port>, Vec<Name>) {
        let port_start = self.tree.data.ports.len_idx();
        let expected_ports = ports
            .filter_map(|module_port| match module_port.kind() {
                ast::ModulePortKind::Name(name) => Some(name.as_name()),
                ast::ModulePortKind::PortDecl(decl) => {
                    self.lower_port_decl(decl);
                    None
                }
            })
            .collect();
        let port_end = self.tree.data.ports.len_idx();
        (IdxRange::new(port_start..port_end), expected_ports)
    }

    fn lower_port_decl(&mut self, decl: ast::PortDecl) {
        let discipline = decl.discipline().map(|it| it.as_name());
        let direction = decl.direction();

        let is_gnd = decl.net_type_token().map_or(false, |it| it.text() == kw::raw::ground);
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        for (name_idx, name) in decl.names().enumerate() {
            self.tree.data.ports.push(Port {
                name: name.as_name(),
                discipline: discipline.clone(),
                is_input: is_input(&direction),
                is_output: is_output(&direction),
                ast_id,
                name_idx,
                is_gnd,
            });
        }
    }

    fn lower_net_decl(&mut self, decl: ast::NetDecl) {
        let discipline = decl.discipline().map(|it| it.as_name());
        let ast_id = self.source_ast_id_map.ast_id(&decl);

        let is_gnd = decl.net_type_token().map_or(false, |it| it.text() == kw::raw::ground);
        for (name_idx, name) in decl.names().enumerate() {
            self.tree.data.nets.push(Net {
                name: name.as_name(),
                discipline: discipline.clone(),
                ast_id,
                is_gnd,
                name_idx,
            });
        }
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt, scope: &mut Vec<BlockScopeItem>) {
        if let ast::Stmt::BlockStmt(block) = stmt {
            if let Some(scope_name) = block.block_scope().and_then(|it| Some(it.name()?.as_name()))
            {
                let mut scope_items = Vec::with_capacity(4);

                for item in block.items() {
                    self.lower_block_item(item, &mut scope_items);
                }

                let ast_id = self.source_ast_id_map.ast_id(&block);
                let block_scope = BlockScope { ast_id, name: scope_name, scope_items };
                let block_scope = self.tree.data.block_scopes.push(block_scope);
                scope.push(block_scope.into());
            } else {
                // For unnamed blocks add the scope to the current active scope
                for item in block.items() {
                    self.lower_block_item(item, scope);
                }
            }
        }
    }

    fn lower_block_item(&mut self, item: BlockItem, scope: &mut Vec<BlockScopeItem>) {
        match item {
            ast::BlockItem::VarDecl(var) => {
                self.lower_var(var, |var| scope.push(var.into()));
            }
            ast::BlockItem::ParamDecl(param) => {
                self.lower_param(param, |param| scope.push(param.into()));
            }
            ast::BlockItem::Stmt(stmt) => self.lower_stmt(stmt, scope),
        }
    }

    fn lower_var(&mut self, decl: ast::VarDecl, mut add_var: impl FnMut(ItemTreeId<Var>)) {
        let ty = decl.ty().as_type();
        for var in decl.vars() {
            if let Some(name) = var.name() {
                let var = Var {
                    name: name.as_name(),
                    ast_id: self.source_ast_id_map.ast_id(&var),
                    ty: ty.clone(),
                };
                let id = self.tree.data.variables.push(var);
                add_var(id)
            }
        }
    }

    fn lower_param(&mut self, decl: ast::ParamDecl, mut add_param: impl FnMut(ItemTreeId<Param>)) {
        let ty = decl.ty().as_type();
        for param in decl.paras() {
            if let Some(name) = param.name() {
                let ast_id = self.source_ast_id_map.ast_id(&param);
                let param = Param { name: name.as_name(), ty: ty.clone(), ast_id };
                let id = self.tree.data.parameters.push(param);
                add_param(id)
            }
        }
    }
}
