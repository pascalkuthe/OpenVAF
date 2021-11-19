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

use std::{fmt::Debug, hash::Hash, ops::Index, sync::Arc};

use crate::{
    attrs::{AttrDiagnostic, LintAttrs},
    db::HirDefDB,
    FileAstId, Name, Path, Type,
};
// use ahash::AHashMap as HashMap;
use arena::{Arena, Idx, IdxRange};
use basedb::{
    lints::{ErasedItemTreeId, Lint, LintLevel},
    FileId,
};
use stdx::impl_from_typed;
use syntax::{ast, AstNode};
use typed_index_collections::TiVec;

/// The item tree of a source file.
#[derive(Debug, Eq, PartialEq)]
pub struct ItemTree {
    pub top_level: Box<[RootItem]>,
    pub(crate) data: ItemTreeData,
    pub(crate) lint_attrs: TiVec<ErasedItemTreeId, LintAttrs>,
    pub diagnostics: Vec<AttrDiagnostic>,
}

impl Default for ItemTree {
    fn default() -> Self {
        Self {
            top_level: Default::default(),
            data: Default::default(),
            // Ensure roo sctx
            lint_attrs: TiVec::from(vec![LintAttrs::empty(None)]),
            diagnostics: Default::default(),
        }
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
            nets,
            ports,
            branches,
            functions,
            function_args,
            block_scopes,
        } = &mut self.data;
        function_args.shrink_to_fit();
        modules.shrink_to_fit();
        disciplines.shrink_to_fit();
        natures.shrink_to_fit();
        variables.shrink_to_fit();
        parameters.shrink_to_fit();
        nets.shrink_to_fit();
        ports.shrink_to_fit();
        ports.shrink_to_fit();
        branches.shrink_to_fit();
        functions.shrink_to_fit();
        block_scopes.shrink_to_fit();
        nature_attrs.shrink_to_fit();
        discipline_attrs.shrink_to_fit();
    }

    pub fn lint_lvl(&self, mut id: ErasedItemTreeId, lint: Lint) -> Option<LintLevel> {
        loop {
            let attrs = &self.lint_attrs[id];
            if let Some(lvl) = attrs.level(lint) {
                return Some(lvl);
            }
            id = attrs.parent()?;
        }
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
    pub nets: Arena<Net>,
    pub ports: Arena<Port>,
    pub branches: Arena<Branch>,
    pub functions: Arena<Function>,
    pub function_args: Arena<FunctionArg>,
    pub block_scopes: Arena<BlockScope>,
}

/// Trait implemented by all item nodes in the item tree.
pub trait ItemTreeNode: Clone {
    type Source: AstNode;

    fn ast_id(&self) -> FileAstId<Self::Source>;
    fn erased_id(&self) -> ErasedItemTreeId;

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
    Scope(ItemTreeId<BlockScope>),
    Parameter(ItemTreeId<Param>),
    Variable(ItemTreeId<Var>),
}

impl_from_typed! (
    Scope(ItemTreeId<BlockScope>),
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

                fn ast_id(&self) -> FileAstId<Self::Source> {
                    self.ast_id
                }

                fn erased_id(&self) -> ErasedItemTreeId {
                    self.erased_id
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
    Net in nets -> ast::NetDecl,
    Port in ports -> ast::PortDecl,
    Branch in branches -> ast::BranchDecl,
    Function in functions -> ast::Function,
    BlockScope in block_scopes -> ast::BlockStmt,
    NatureAttr in nature_attrs -> ast::NatureAttr,
    DisciplineAttr in discipline_attrs -> ast::DisciplineAttr,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Module {
    pub name: Name,

    pub exptected_ports: Vec<Name>,
    pub head_ports: IdxRange<Port>,

    pub body_ports: IdxRange<Port>,
    pub nets: IdxRange<Net>,
    pub branches: IdxRange<Branch>,
    pub functions: IdxRange<Function>,
    pub scope_items: Vec<BlockScopeItem>,
    pub ast_id: FileAstId<ast::ModuleDecl>,
    pub erased_id: ErasedItemTreeId,
}

impl Module {
    /// The Verilog-A standard only allows `body_ports` or `head_ports`.
    /// A lint seperatly checks that the ports are delared legally.
    /// This function simply returns all relevant ports for later stages of the compiler
    pub fn ports(&self) -> IdxRange<Port> {
        self.head_ports.cover(&self.body_ports)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Port {
    pub name: Name,
    pub discipline: Option<Name>,
    pub is_gnd: bool,
    pub is_input: bool,
    pub is_output: bool,

    pub name_idx: usize,
    pub ast_id: FileAstId<ast::PortDecl>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Net {
    pub name: Name,
    pub discipline: Option<Name>,
    pub is_gnd: bool,

    pub name_idx: usize,
    pub ast_id: FileAstId<ast::NetDecl>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Var {
    pub name: Name,
    pub ty: Type,
    pub ast_id: FileAstId<ast::Var>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Param {
    pub name: Name,
    pub ty: Type,
    pub ast_id: FileAstId<ast::Param>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Nature {
    pub name: Name,
    pub parent: Option<Name>,
    pub access: Option<Name>,
    pub ddt_nature: Option<Name>,
    pub idt_nature: Option<Name>,
    pub attrs: IdxRange<NatureAttr>,
    pub ast_id: FileAstId<ast::NatureDecl>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NatureAttr {
    pub name: Name,
    pub ast_id: FileAstId<ast::NatureAttr>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DisciplineAttr {
    pub name: Name,
    pub ast_id: FileAstId<ast::DisciplineAttr>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Domain {
    Discrete,
    Continous,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Discipline {
    pub name: Name,

    pub potential: Option<Name>,
    pub flow: Option<Name>,
    pub attrs: IdxRange<DisciplineAttr>,

    // Not strictly neccessary to resolve this here but
    // we already have to do the other attributes here
    // adding extra handeling is not worth it
    pub domain: Option<Domain>,

    pub ast_id: FileAstId<ast::DisciplineDecl>,
    pub erased_id: ErasedItemTreeId,
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
    // Strictly speaking this shouldn't be part of the item tree, because it doesn't effect name
    // resolution but reconstructing the red layer is just not worth it for such a small thing
    pub kind: BranchKind,
    pub ast_id: FileAstId<ast::BranchDecl>,
    pub erased_id: ErasedItemTreeId,
}

// #[derive(Debug, Eq, PartialEq, Clone)]
// pub enum BranchType {
//     PortFlowProbe { port: Name },
//     NodeToGnd { node: Name },
//     Connection { hi: Name, lo: Name },
// }

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockScope {
    pub name: Name,
    pub scope_items: Vec<BlockScopeItem>,
    pub ast_id: FileAstId<ast::BlockStmt>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Function {
    pub name: Name,
    pub ty: Type,
    pub args: IdxRange<FunctionArg>,
    pub params: IdxRange<Param>,
    pub vars: IdxRange<Var>,
    pub ast_id: FileAstId<ast::Function>,
    pub erased_id: ErasedItemTreeId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionArg {
    pub name: Name,
    pub is_input: bool,
    pub is_output: bool,
    pub ast_id: FileAstId<ast::FunctionArg>,
    pub erased_id: ErasedItemTreeId,
}
