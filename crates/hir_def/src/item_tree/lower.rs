use std::{mem, sync::Arc};

use arena::IdxRange;
use basedb::{AstId, AstIdMap, FileId};

use syntax::{
    ast::{self, PathSegmentKind},
    match_ast,
    name::{kw, AsIdent, AsName},
    AstNode, WalkEvent,
};
use typed_index_collections::TiVec;
// use tracing::trace;

use crate::{db::HirDefDB, types::AsType, LocalFunctionArgId, LocalNodeId, Path, Type};

use super::{
    Block, Branch, BranchKind, Discipline, DisciplineAttr, DisciplineAttrKind, Domain, Function,
    FunctionArg, FunctionItem, ItemTree, ItemTreeId, Module, ModuleItem, Nature, NatureAttr,
    NatureRef, NatureRefKind, Net, Node, Param, Port, RootItem, Var,
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
        use kw::raw as kw;
        let name = decl.name()?.as_name();
        let ast_id = self.source_ast_id_map.ast_id(&decl);

        let mut potential = None;
        let mut flow = None;
        let mut domain = None;
        let attr_start = self.tree.data.discipline_attrs.next_key();
        for (id, attr) in decl.discipline_attrs().enumerate() {
            if let Some(name) = attr.name() {
                let kind = if let Some(qual) = name.qualifier() {
                    let qual = qual.segment_token();
                    match qual.as_ref().map(|t| t.text()) {
                        Some(kw::potential) => DisciplineAttrKind::PotentialOverwrite,
                        Some(kw::flow) => DisciplineAttrKind::FlowOverwrite,
                        _ => continue,
                    }
                } else {
                    DisciplineAttrKind::UserDefined
                };

                if let Some(name) = name.segment_token().map(|t| t.as_name()) {
                    let ast_id = self.source_ast_id_map.ast_id(&attr);

                    match &*name {
                        kw::potential if potential.is_none() => {
                            if let Some(name) = attr.val().and_then(Self::lower_nature_expr) {
                                potential = Some((name, id.into()))
                            }
                        }
                        kw::flow if flow.is_none() => {
                            if let Some(name) = attr.val().and_then(Self::lower_nature_expr) {
                                flow = Some((name, id.into()))
                            }
                        }
                        kw::domain if domain.is_none() => {
                            match attr.val().and_then(|e| e.as_ident()).as_deref() {
                                Some(kw::continuous) => {
                                    domain = Some((Domain::Continous, id.into()));
                                }
                                Some(kw::discrete) => {
                                    domain = Some((Domain::Discrete, id.into()));
                                }
                                _ => (),
                            }
                        }

                        _ => (),
                    };

                    self.tree.data.discipline_attrs.push(DisciplineAttr {
                        name: name.clone(),
                        kind,
                        ast_id,
                    });
                }
            }
        }
        let attr_end = self.tree.data.discipline_attrs.next_key();
        let res = Discipline {
            ast_id,
            name,
            potential,
            flow,
            extra_attrs: IdxRange::new(attr_start..attr_end),
            domain,
        };
        Some(self.tree.data.disciplines.push_and_get_key(res))
    }

    fn lower_nature_path(decl: &ast::Path) -> Option<NatureRef> {
        let mut name = decl.segment_token()?.as_name();

        let kind = match &*name {
            kw::raw::potential => NatureRefKind::DisciplinePotential,
            kw::raw::flow => NatureRefKind::DisciplinePotential,
            _ if decl.qualifier().is_none() && decl.segment_kind()? == PathSegmentKind::Name => {
                NatureRefKind::Nature
            }
            _ => return None,
        };

        if matches!(kind, NatureRefKind::DisciplineFlow | NatureRefKind::DisciplinePotential) {
            let qual = decl.qualifier()?;
            let segment = qual.segment()?;
            if segment.kind == PathSegmentKind::Root || qual.qualifier().is_some() {
                return None;
            }
            name = segment.syntax.as_name();
        }

        Some(NatureRef { name, kind })
    }

    fn lower_nature_expr(decl: ast::Expr) -> Option<NatureRef> {
        if let ast::Expr::PathExpr(path) = decl {
            Self::lower_nature_path(&path.path()?)
        } else {
            None
        }
    }

    fn lower_nature(&mut self, decl: ast::NatureDecl) -> Option<ItemTreeId<Nature>> {
        let name = decl.name()?.as_name();

        let parent = decl.parent().and_then(|it| Self::lower_nature_path(&it));
        let attr_start = self.tree.data.nature_attrs.next_key();

        let mut access = None;
        let mut ddt_nature = None;
        let mut idt_nature = None;
        let mut units = None;
        let mut abstol = None;

        for (id, attr) in decl.nature_attrs().enumerate() {
            if let Some(name) = attr.name().map(|name| name.as_name()) {
                use kw::raw as kw;

                let ast_id = self.source_ast_id_map.ast_id(&attr);

                match &*name {
                    kw::access if access.is_none() => {
                        if let Some(name) = attr.val().and_then(|e| e.as_ident()) {
                            access = Some((name, id.into()));
                        }
                    }
                    kw::ddt_nature if ddt_nature.is_none() => {
                        if let Some(name) = attr.val().and_then(Self::lower_nature_expr) {
                            ddt_nature = Some((name, id.into()));
                        }
                    }
                    kw::idt_nature if idt_nature.is_none() => {
                        if let Some(name) = attr.val().and_then(Self::lower_nature_expr) {
                            idt_nature = Some((name, id.into()));
                        }
                    }

                    kw::units if units.is_none() => {
                        if let Some(ast::LiteralKind::String(lit)) =
                            attr.val().and_then(|e| e.as_literal())
                        {
                            units = Some((lit.unescaped_value(), id.into()));
                        }
                    }

                    kw::abs if abstol.is_none() => {
                        abstol = Some(id.into());
                    }
                    _ => (),
                };

                self.tree.data.nature_attrs.push(NatureAttr { name, ast_id });
            }
        }

        let attr_end = self.tree.data.nature_attrs.next_key();
        let ast_id = self.source_ast_id_map.ast_id(&decl);

        let res = Nature {
            ast_id,
            name,
            parent,
            access,
            ddt_nature,
            idt_nature,
            abstol,
            units,
            attrs: IdxRange::new(attr_start..attr_end),
        };
        Some(self.tree.data.natures.push_and_get_key(res))
    }

    fn lower_module(&mut self, decl: ast::ModuleDecl) -> Option<ItemTreeId<Module>> {
        let name = decl.name()?.as_name();
        let ast_id = self.source_ast_id_map.ast_id(&decl);

        let mut nodes = TiVec::new();
        let mut items = Vec::new();
        if let Some(ports) = decl.module_ports() {
            self.lower_module_ports(ports, &mut nodes, &mut items, ast_id);
        }

        self.lower_module_items(decl.module_items(), &mut nodes, &mut items);

        let res = Module { name, nodes, items, ast_id };
        Some(self.tree.data.modules.push_and_get_key(res))
    }

    fn lower_module_items(
        &mut self,
        items: ast::AstChildren<ast::ModuleItem>,
        nodes: &mut TiVec<LocalNodeId, Node>,
        dst: &mut Vec<ModuleItem>,
    ) {
        for item in items {
            match item {
                ast::ModuleItem::BodyPortDecl(decl) => {
                    if let Some(decl) = decl.port_decl() {
                        self.lower_port_decl(decl, nodes, dst);
                    }
                }
                ast::ModuleItem::NetDecl(decl) => {
                    self.lower_net_decl(decl, nodes, dst);
                }
                ast::ModuleItem::AnalogBehaviour(behaviour) => {
                    if let Some(stmt) = behaviour.stmt() {
                        self.lower_stmt(stmt, dst);
                    }
                }
                ast::ModuleItem::VarDecl(var) => {
                    self.lower_var(var, dst);
                }
                ast::ModuleItem::ParamDecl(param) => {
                    self.lower_param(param, dst);
                }
                ast::ModuleItem::Function(fun) => {
                    self.lower_fun(fun, dst);
                }
                ast::ModuleItem::BranchDecl(branch) => self.lower_branch(branch, dst),
            };
        }
    }

    fn lower_fun(&mut self, fun: ast::Function, dst: &mut Vec<ModuleItem>) {
        let mut items = Vec::new();
        let mut args: TiVec<LocalFunctionArgId, FunctionArg> = TiVec::new();
        for item in fun.function_items() {
            match item {
                ast::FunctionItem::ParamDecl(decl) => self.lower_param(decl, &mut items),
                ast::FunctionItem::VarDecl(decl) => self.lower_var(decl, &mut items),
                ast::FunctionItem::FunctionArg(arg) => {
                    let ast_id = self.source_ast_id_map.ast_id(&arg);
                    let is_input = is_input(&arg.direction());
                    let is_output = is_output(&arg.direction());
                    for (name_idx, name) in arg.names().enumerate() {
                        let name = name.as_name();
                        if let Some(arg) = args.iter_mut().find(|arg| arg.name == name) {
                            // TODO validation
                            arg.ast_ids.push(ast_id)
                        }
                        let arg = args.push_and_get_key(FunctionArg {
                            name,
                            name_idx,
                            is_input,
                            is_output,
                            declarations: Vec::new(),
                            ast_ids: vec![ast_id],
                        });
                        items.push(arg.into());
                    }
                }
                ast::FunctionItem::Stmt(stmt) => self.lower_stmt(stmt, &mut items),
            }
        }

        items.retain(|decl| {
            if let FunctionItem::Variable(var) = decl {
                if let Some(arg) = args.iter_mut().find(|arg| arg.name == self.tree[*var].name) {
                    // TODO validation
                    arg.declarations.push(*var);
                    return false;
                }
            };
            true
        });

        if let Some(name) = fun.name() {
            let fun = Function {
                name: name.as_name(),
                ty: fun.ty().map_or(Type::Real, |ty| ty.as_type()),
                args,
                items,
                ast_id: self.source_ast_id_map.ast_id(&fun),
            };
            let fun = self.tree.data.functions.push_and_get_key(fun);
            dst.push(fun.into())
        }
    }

    fn lower_branch(&mut self, decl: ast::BranchDecl, dst: &mut Vec<ModuleItem>) {
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
        for (name_idx, name) in decl.names().enumerate() {
            let branch = Branch { name: name.as_name(), kind: kind.clone(), ast_id, name_idx };
            let id = self.tree.data.branches.push_and_get_key(branch);
            dst.push(id.into());
        }
    }

    fn lower_module_ports(
        &mut self,
        ports: ast::ModulePorts,
        nodes: &mut TiVec<LocalNodeId, Node>,
        dst: &mut Vec<ModuleItem>,
        module: AstId<ast::ModuleDecl>,
    ) {
        for port in ports.ports() {
            match port.kind() {
                ast::ModulePortKind::Name(name) => {
                    let name = name.as_name();
                    if nodes.iter().all(|node| node.name != name) {
                        let node = nodes.push_and_get_key(Node {
                            name,
                            ast_id: module.into(),
                            decls: Vec::new(),
                        });
                        dst.push(node.into())
                    }
                }
                ast::ModulePortKind::PortDecl(decl) => {
                    self.lower_port_decl(decl, nodes, dst);
                }
            }
        }
    }

    fn lower_port_decl(
        &mut self,
        decl: ast::PortDecl,
        nodes: &mut TiVec<LocalNodeId, Node>,
        dst: &mut Vec<ModuleItem>,
    ) {
        let discipline = decl.discipline().map(|it| it.as_name());
        let direction = decl.direction();

        let is_gnd = decl.net_type_token().map_or(false, |it| it.text() == kw::raw::ground);
        let ast_id = self.source_ast_id_map.ast_id(&decl);
        for (name_idx, name) in decl.names().enumerate() {
            let name = name.as_name();
            let id = self.tree.data.ports.push_and_get_key(Port {
                name: name.clone(),
                discipline: discipline.clone(),
                is_input: is_input(&direction),
                is_output: is_output(&direction),
                ast_id,
                name_idx,
                is_gnd,
            });

            match nodes.iter_mut().find(|node| node.name == name) {
                Some(node) => node.decls.push(id.into()),
                None => {
                    let node = nodes.push_and_get_key(Node {
                        name,
                        ast_id: ast_id.into(),
                        decls: vec![id.into()],
                    });
                    dst.push(node.into())
                }
            }
        }
    }

    fn lower_net_decl(
        &mut self,
        decl: ast::NetDecl,
        nodes: &mut TiVec<LocalNodeId, Node>,
        dst: &mut Vec<ModuleItem>,
    ) {
        let discipline = decl.discipline().map(|it| it.as_name());
        let ast_id = self.source_ast_id_map.ast_id(&decl);

        let is_gnd = decl.net_type_token().map_or(false, |it| it.text() == kw::raw::ground);
        for (name_idx, name) in decl.names().enumerate() {
            let name = name.as_name();
            let id = self.tree.data.nets.push_and_get_key(Net {
                name: name.clone(),
                discipline: discipline.clone(),
                ast_id,
                is_gnd,
                name_idx,
            });

            match nodes.iter_mut().find(|node| node.name == name) {
                Some(node) => node.decls.push(id.into()),
                None => {
                    let node = nodes.push_and_get_key(Node {
                        name,
                        ast_id: ast_id.into(),
                        decls: vec![id.into()],
                    });
                    dst.push(node.into());
                }
            }
        }
    }

    fn lower_stmt<
        T: From<ItemTreeId<Param>> + From<ItemTreeId<Var>> + From<AstId<ast::BlockStmt>>,
    >(
        &mut self,
        stmt: ast::Stmt,
        parent_scope: &mut Vec<T>,
    ) {
        let mut block_stack = Vec::new();
        let mut block_scope_stack = Vec::new();
        let mut blocks = mem::take(&mut self.tree.blocks);

        for event in stmt.syntax().preorder() {
            match event {
                WalkEvent::Enter(node) => {
                    match_ast! {
                        match node {
                            ast::BlockStmt(block) => {
                                let ast_id = self.source_ast_id_map.ast_id(&block);
                                let name = block.block_scope().and_then(|it| Some(it.name()?.as_name()));
                                let block_info = Block { name, scope_items: Vec::new()};
                                if block.block_scope().is_some() {
                                    match block_scope_stack.last() {
                                        Some(block) => {
                                            let block = blocks.get_mut(block).unwrap();
                                             block.scope_items.push(ast_id.into());
                                        }
                                        None =>  parent_scope.push(ast_id.into()),
                                    };

                                    block_scope_stack.push(ast_id);
                                }

                                blocks.insert(ast_id, block_info);
                                block_stack.push(ast_id);
                            },
                            ast::VarDecl(var) => {
                              match block_stack.last() {
                                    Some(block) => {
                                        let block = blocks.get_mut(block).unwrap();
                                        self.lower_var(var, &mut block.scope_items)
                                    }
                                    None => self.lower_var(var, parent_scope),
                                }
                            },
                            ast::ParamDecl(param) => {
                              match block_stack.last() {
                                    Some(block) => {
                                        let block = blocks.get_mut(block).unwrap();
                                        self.lower_param(param, &mut block.scope_items)
                                    }
                                 None => self.lower_param(param, parent_scope),
                                }
                            },
                            _ => ()
                        }
                    }
                }
                WalkEvent::Leave(node) => {
                    if let Some(block) = ast::BlockStmt::cast(node) {
                        block_stack.pop();
                        if block.block_scope().is_some() {
                            block_scope_stack.pop();
                        }
                    }
                }
            }
        }

        self.tree.blocks = blocks;
    }

    fn lower_var<T: From<ItemTreeId<Var>>>(&mut self, decl: ast::VarDecl, dst: &mut Vec<T>) {
        let ty = decl.ty().as_type();
        for var in decl.vars() {
            if let Some(name) = var.name() {
                let var = Var {
                    name: name.as_name(),
                    ast_id: self.source_ast_id_map.ast_id(&var),
                    ty: ty.clone(),
                };
                let id = self.tree.data.variables.push_and_get_key(var);
                dst.push(id.into())
            }
        }
    }

    fn lower_param<T: From<ItemTreeId<Param>>>(&mut self, decl: ast::ParamDecl, dst: &mut Vec<T>) {
        let ty = decl.ty().as_type();
        for param in decl.paras() {
            if let Some(name) = param.name() {
                let ast_id = self.source_ast_id_map.ast_id(&param);
                let param = Param { name: name.as_name(), ty: ty.clone(), ast_id };
                let id = self.tree.data.parameters.push_and_get_key(param);
                dst.push(id.into())
            }
        }
    }
}
