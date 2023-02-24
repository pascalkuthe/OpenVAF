//! A simplified AST that only contains items.
//!
//! This is the primary IR used throughout `hir_def`. It is the input to the name resolution
//! algorith
//!
//! One important purpose of this layer is to provide an "invalidation barrier" for incremental
//! computations: when typing inside an item body, the `ItemTree` of the modified file is typically
//! unaffected, so we don't have to recompute name resolution results or item data (see `data.rs`).
//!
//! In general, any item in the `ItemTree` stores its `AstId`, which allows mapping it back to its
//! surface syntax.

mod lower;
mod pretty;

use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Index;
use std::sync::Arc;

use ahash::AHashMap;
use arena::{Arena, Idx, IdxRange};
use basedb::{AstId, ErasedAstId, FileId};
use stdx::impl_from_typed;
use syntax::ast::{self, BlockStmt, NameRef};
use syntax::name::Name;
use syntax::AstNode;
use typed_index_collections::TiVec;

use crate::db::HirDefDB;
use crate::{
    LocalDisciplineAttrId, LocalFunctionArgId, LocalNatureAttrId, LocalNodeId, Path, Type,
};

/// The item tree of a source file.
#[derive(Debug, Eq, PartialEq)]
pub struct ItemTree {
    pub top_level: Box<[RootItem]>,
    pub(crate) data: ItemTreeData,
    pub(crate) blocks: AHashMap<AstId<BlockStmt>, Block>,
}

impl Default for ItemTree {
    fn default() -> Self {
        Self { top_level: Default::default(), data: Default::default(), blocks: AHashMap::new() }
    }
}

impl ItemTree {
    pub(crate) fn file_item_tree_query(db: &dyn HirDefDB, file: FileId) -> Arc<ItemTree> {
        let syntax_tree = db.parse(file).tree();
        let ctx = lower::Ctx::new(db, file);
        let mut item_tree = ctx.lower_root_items(&syntax_tree);
        item_tree.shrink_to_fit();
        Arc::new(item_tree)
    }

    fn shrink_to_fit(&mut self) {
        let ItemTreeData {
            modules,
            disciplines,
            natures,
            nature_attrs,
            discipline_attrs,
            variables,
            parameters,
            alias_parameters,
            nets,
            ports,
            branches,
            functions,
        } = &mut self.data;
        modules.shrink_to_fit();
        disciplines.shrink_to_fit();
        natures.shrink_to_fit();
        variables.shrink_to_fit();
        parameters.shrink_to_fit();
        alias_parameters.shrink_to_fit();
        nets.shrink_to_fit();
        ports.shrink_to_fit();
        ports.shrink_to_fit();
        branches.shrink_to_fit();
        functions.shrink_to_fit();
        nature_attrs.shrink_to_fit();
        discipline_attrs.shrink_to_fit();
    }

    pub fn block_scope(&self, block: AstId<BlockStmt>) -> &Block {
        &self.blocks[&block]
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
pub(crate) struct ItemTreeData {
    pub modules: Arena<Module>,
    pub disciplines: Arena<Discipline>,
    pub natures: Arena<Nature>,
    pub nature_attrs: Arena<NatureAttr>,
    pub discipline_attrs: Arena<DisciplineAttr>,

    pub variables: Arena<Var>,
    pub parameters: Arena<Param>,
    pub alias_parameters: Arena<AliasParam>,
    pub nets: Arena<Net>,
    pub ports: Arena<Port>,
    pub branches: Arena<Branch>,
    pub functions: Arena<Function>,
}

/// Trait implemented by all item nodes in the item tree.
pub trait ItemTreeNode: Clone {
    type Source: AstNode;

    fn name(&self) -> &Name;

    fn ast_id(&self) -> AstId<Self::Source>;

    /// Looks up an instance of `Self` in an item tree.
    fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self;

    /// Downcasts a `ScopeItem` to a `FileItemTreeId` specific to this type.
    fn id_from_mod_item(mod_item: ScopeItem) -> Option<ItemTreeId<Self>>;

    /// Upcasts a `FileItemTreeId` to a generic `ScopeItem`.
    fn id_to_mod_item(id: ItemTreeId<Self>) -> ScopeItem;
}

pub type ItemTreeId<N> = Idx<N>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum RootItem {
    Module(ItemTreeId<Module>),
    Nature(ItemTreeId<Nature>),
    Discipline(ItemTreeId<Discipline>),
}

impl_from_typed! (
    Module(ItemTreeId<Module>),
    Nature(ItemTreeId<Nature>),
    Discipline(ItemTreeId<Discipline>) for RootItem
);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BlockScopeItem {
    Scope(AstId<BlockStmt>),
    Parameter(ItemTreeId<Param>),
    Variable(ItemTreeId<Var>),
}

impl_from_typed! (
    Scope(AstId<BlockStmt>),
    Parameter(ItemTreeId<Param>),
    Variable(ItemTreeId<Var>) for BlockScopeItem
);

macro_rules! item_tree_nodes {
    ( $( $typ:ident in $fld:ident -> $ast:ty ),+ $(,)? ) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub enum ScopeItem {
            $(
                $typ(ItemTreeId<$typ>),
            )+
        }

        $(
            impl From<ItemTreeId<$typ>> for ScopeItem {
                fn from(id: ItemTreeId<$typ>) -> ScopeItem {
                    ScopeItem::$typ(id)
                }
            }
        )+

        $(
            impl ItemTreeNode for $typ {
                type Source = $ast;


                fn name(&self) -> &Name {
                    &self.name
                }

                fn ast_id(&self) -> AstId<Self::Source> {
                    self.ast_id
                }


                fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self {
                    &tree.data.$fld[index]
                }

                fn id_from_mod_item(mod_item: ScopeItem) -> Option<ItemTreeId<Self>> {
                    if let ScopeItem::$typ(id) = mod_item {
                        Some(id)
                    } else {
                        None
                    }
                }

                fn id_to_mod_item(id: ItemTreeId<Self>) -> ScopeItem {
                    ScopeItem::$typ(id)
                }
            }

            impl Index<Idx<$typ>> for ItemTree {
                type Output = $typ;

                fn index(&self, index: Idx<$typ>) -> &Self::Output {
                    &self.data.$fld[index]
                }
            }
        )+
    };
}

item_tree_nodes! {
    Module in modules -> ast::ModuleDecl,
    Discipline in disciplines -> ast::DisciplineDecl,
    Nature in natures -> ast::NatureDecl,

    Var in variables -> ast::Var,
    Param in parameters -> ast::Param,
    AliasParam in alias_parameters -> ast::AliasParam,
    Net in nets -> ast::NetDecl,
    Port in ports -> ast::PortDecl,
    Branch in branches -> ast::BranchDecl,
    Function in functions -> ast::Function,
    NatureAttr in nature_attrs -> ast::NatureAttr,
    DisciplineAttr in discipline_attrs -> ast::DisciplineAttr,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Module {
    pub name: Name,
    pub nodes: TiVec<LocalNodeId, Node>,
    pub num_ports: u32,
    pub items: Vec<ModuleItem>,
    pub ast_id: AstId<ast::ModuleDecl>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ModuleItem {
    Scope(AstId<BlockStmt>),
    Parameter(ItemTreeId<Param>),
    AliasParameter(ItemTreeId<AliasParam>),
    Variable(ItemTreeId<Var>),
    Branch(ItemTreeId<Branch>),
    Node(LocalNodeId),
    Function(ItemTreeId<Function>),
}

impl_from_typed! (
    Scope(AstId<BlockStmt>),
    Parameter(ItemTreeId<Param>),
    AliasParameter(ItemTreeId<AliasParam>),
    Variable(ItemTreeId<Var>),
    Branch(ItemTreeId<Branch>),
    Node(LocalNodeId),
    Function(ItemTreeId<Function>) for ModuleItem
);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Port {
    pub name: Name,
    pub discipline: Option<Name>,
    pub is_gnd: bool,
    pub is_input: bool,
    pub is_output: bool,

    pub name_idx: usize,
    pub ast_id: AstId<ast::PortDecl>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Net {
    pub name: Name,
    pub discipline: Option<Name>,
    pub is_gnd: bool,

    pub name_idx: usize,
    pub ast_id: AstId<ast::NetDecl>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Var {
    pub name: Name,
    pub ty: Type,
    pub ast_id: AstId<ast::Var>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Param {
    pub name: Name,
    pub ty: Option<Type>,
    pub is_local: bool,
    pub ast_id: AstId<ast::Param>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct AliasParam {
    pub name: Name,
    pub src: Option<Path>,
    pub ast_id: AstId<ast::AliasParam>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct NatureRef {
    pub name: Name,
    pub kind: NatureRefKind,
}
#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
pub enum NatureRefKind {
    Nature,
    DisciplinePotential,
    DisciplineFlow,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Nature {
    pub name: Name,
    pub parent: Option<NatureRef>,
    pub access: Option<(Name, LocalNatureAttrId)>,
    pub ddt_nature: Option<(NatureRef, LocalNatureAttrId)>,
    pub idt_nature: Option<(NatureRef, LocalNatureAttrId)>,
    pub abstol: Option<LocalNatureAttrId>,
    pub units: Option<(String, LocalNatureAttrId)>,
    pub attrs: IdxRange<NatureAttr>,
    pub ast_id: AstId<ast::NatureDecl>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NatureAttr {
    pub name: Name,
    pub ast_id: AstId<ast::NatureAttr>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
pub enum DisciplineAttrKind {
    FlowOverwrite,
    PotentialOverwrite,
    UserDefined,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DisciplineAttr {
    pub name: Name,
    pub kind: DisciplineAttrKind,
    pub ast_id: AstId<ast::DisciplineAttr>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Domain {
    Discrete,
    Continous,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Discipline {
    pub name: Name,

    pub potential: Option<(NatureRef, LocalDisciplineAttrId)>,
    pub flow: Option<(NatureRef, LocalDisciplineAttrId)>,
    pub domain: Option<(Domain, LocalDisciplineAttrId)>,

    pub extra_attrs: IdxRange<DisciplineAttr>,

    pub ast_id: AstId<ast::DisciplineDecl>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BranchKind {
    PortFlow(Path),
    NodeGnd(Path),
    Nodes(Path, Path),
    Missing,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Branch {
    pub name: Name,
    pub name_idx: usize,
    pub kind: BranchKind,
    pub ast_id: AstId<ast::BranchDecl>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block {
    pub name: Option<Name>,
    pub scope_items: Vec<BlockScopeItem>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Function {
    pub name: Name,
    pub ty: Type,
    pub args: TiVec<LocalFunctionArgId, FunctionArg>,
    pub items: Vec<FunctionItem>,
    pub ast_id: AstId<ast::Function>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FunctionItem {
    Scope(AstId<BlockStmt>),
    Parameter(ItemTreeId<Param>),
    Variable(ItemTreeId<Var>),
    FunctionArg(LocalFunctionArgId),
}

impl_from_typed! (
    Scope(AstId<BlockStmt>),
    Parameter(ItemTreeId<Param>),
    Variable(ItemTreeId<Var>),
    FunctionArg(LocalFunctionArgId) for FunctionItem
);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionArg {
    pub name: Name,
    pub name_idx: usize,
    pub is_input: bool,
    pub is_output: bool,
    pub declarations: Vec<ItemTreeId<Var>>,
    pub ast_ids: Vec<AstId<ast::FunctionArg>>,
}

impl FunctionArg {
    pub fn ty(&self, tree: &ItemTree) -> Type {
        self.declarations.first().map_or(Type::Err, |decl| tree[*decl].ty.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum NodeTypeDecl {
    Net(ItemTreeId<Net>),
    Port(ItemTreeId<Port>),
}

impl NodeTypeDecl {
    pub fn discipline(self, tree: &ItemTree) -> &Option<Name> {
        match self {
            NodeTypeDecl::Net(net) => &tree[net].discipline,
            NodeTypeDecl::Port(port) => &tree[port].discipline,
        }
    }

    pub fn discipline_src(self, db: &dyn HirDefDB, root_file: FileId) -> Option<NameRef> {
        let ast_id_map = db.ast_id_map(root_file);
        let tree = db.item_tree(root_file);
        let ast = db.parse(root_file).syntax_node();
        match self {
            NodeTypeDecl::Net(net) => ast_id_map.get(tree[net].ast_id).to_node(&ast).discipline(),
            NodeTypeDecl::Port(port) => {
                ast_id_map.get(tree[port].ast_id).to_node(&ast).discipline()
            }
        }
    }

    pub fn is_gnd(self, tree: &ItemTree) -> bool {
        match self {
            NodeTypeDecl::Net(net) => tree[net].is_gnd,
            NodeTypeDecl::Port(port) => tree[port].is_gnd,
        }
    }

    pub fn direction(self, tree: &ItemTree) -> Option<(bool, bool)> {
        match self {
            NodeTypeDecl::Port(port) => Some((tree[port].is_input, tree[port].is_output)),
            NodeTypeDecl::Net(_) => None,
        }
    }

    pub fn ast_id(self, tree: &ItemTree) -> ErasedAstId {
        match self {
            NodeTypeDecl::Net(net) => tree[net].ast_id.into(),
            NodeTypeDecl::Port(port) => tree[port].ast_id.into(),
        }
    }
}

impl_from_typed!(
    Net(ItemTreeId<Net>),
    Port(ItemTreeId<Port>) for NodeTypeDecl
);

impl NodeTypeDecl {
    pub fn name<'a>(&self, tree: &'a ItemTree) -> &'a Name {
        match *self {
            NodeTypeDecl::Net(net) => &tree[net].name,
            NodeTypeDecl::Port(port) => &tree[port].name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub name: Name,
    pub is_port: bool,
    pub ast_id: ErasedAstId,
    // TODO small vec?
    pub decls: Vec<NodeTypeDecl>,
}

impl Node {
    pub fn direction(&self, tree: &ItemTree) -> (bool, bool) {
        match self.decls.iter().find_map(|decl| decl.direction(tree)) {
            Some(direction) => direction,
            // default to inout to avoid confusing error messages
            // and to allow omitting direction specification
            // for backwards compatability with cadence, see #40
            None if self.is_port => (true, true),
            None => (false, false),
        }
    }

    pub fn is_gnd(&self, tree: &ItemTree) -> bool {
        self.decls.iter().any(|decl| decl.is_gnd(tree))
    }

    pub fn discipline(&self, tree: &ItemTree) -> Option<Name> {
        self.decls.iter().find_map(|decl| decl.discipline(tree).clone())
    }
}
