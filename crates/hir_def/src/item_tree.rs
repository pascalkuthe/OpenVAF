//! A simplified AST that only contains items.
//!
//! This is the primary IR used throughout `hir_def`. It is the input to the name resolution
//! algorithm, as well as to the queries defined in `adt.rs`, `data.rs`, and most things in
//! `attr.rs`.
//!
//! `ItemTree`s are built per `HirFileId`, from the syntax tree of the parsed file. This means that
//! they are crate-independent: they don't know which `#[cfg]`s are active or which module they
//! belong to, since those concepts don't exist at this level (a single `ItemTree` might be part of
//! multiple crates, or might be included into the same crate twice via `#[path]`).
//!
//! One important purpose of this layer is to provide an "invalidation barrier" for incremental
//! computations: when typing inside an item body, the `ItemTree` of the modified file is typically
//! unaffected, so we don't have to recompute name resolution results or item data (see `data.rs`).
//!
//! The `ItemTree` for the currently open file can be displayed by using the VS Code command
//! "Rust Analyzer: Debug ItemTree".
//!
//! Compared to rustc's architecture, `ItemTree` has properties from both rustc's AST and HIR: many
//! syntax-level Rust features are already desugared to simpler forms in the `ItemTree`, but name
//! resolution has not yet been performed. `ItemTree`s are per-file, while rustc's AST and HIR are
//! per-crate, because we are interested in incrementally computing it.
//!
//! The representation of items in the `ItemTree` should generally mirror the surface syntax: it is
//! usually a bad idea to desugar a syntax-level construct to something that is structurally
//! different here. Name resolution needs to be able to process attributes and expand macros
//! (including attribute macros), and having a 1-to-1 mapping between syntax and the `ItemTree`
//! avoids introducing subtle bugs.
//!
//! In general, any item in the `ItemTree` stores its `AstId`, which allows mapping it back to its
//! surface syntax.

mod lower;

use std::{fmt::Debug, hash::Hash, ops::Index, sync::Arc};

use crate::{db::HirDefDB, FileAstId, Name, Path, Type};
use basedb::FileId;
use data_structures::{
    arena::{Arena, Idx, IdxRange},
    HashMap,
};
use derive_more::{From, TryInto};
use syntax::{ast, AstNode};

/// The item tree of a source file.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct ItemTree {
    pub top_level: Box<[RootItem]>,
    pub(crate) data: ItemTreeData,
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
    // syntax_ctx: Arena<SyntaxCtx>,
}

/// Trait implemented by all item nodes in the item tree.
pub trait ItemTreeNode: Clone {
    type Source: AstNode;

    fn ast_id(&self) -> FileAstId<Self::Source>;

    /// Looks up an instance of `Self` in an item tree.
    fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self;

    /// Downcasts a `ScopeItem` to a `FileItemTreeId` specific to this type.
    fn id_from_mod_item(mod_item: ScopeItem) -> Option<ItemTreeId<Self>>;

    /// Upcasts a `FileItemTreeId` to a generic `ScopeItem`.
    fn id_to_mod_item(id: ItemTreeId<Self>) -> ScopeItem;
}

pub type ItemTreeId<N: ItemTreeNode> = Idx<N>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, From, TryInto)]
pub enum RootItem {
    Module(ItemTreeId<Module>),
    Nature(ItemTreeId<Nature>),
    Discipline(ItemTreeId<Discipline>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, From, TryInto)]
pub enum BlockScopeItem {
    Scope(ItemTreeId<BlockScope>),
    Parameter(ItemTreeId<Param>),
    Variable(ItemTreeId<Var>),
}

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
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Net {
    pub name: Name,
    pub discipline: Option<Name>,
    pub is_gnd: bool,

    pub name_idx: usize,
    pub ast_id: FileAstId<ast::NetDecl>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Var {
    pub name: Name,
    pub ty: Type,
    pub ast_id: FileAstId<ast::Var>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Param {
    pub name: Name,
    pub ty: Type,
    pub ast_id: FileAstId<ast::Param>,
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NatureAttr {
    pub name: Name,
    pub ast_id: FileAstId<ast::NatureAttr>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DisciplineAttr {
    pub name: Name,
    pub ast_id: FileAstId<ast::DisciplineAttr>,
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Function {
    pub name: Name,
    pub ty: Type,
    pub args: IdxRange<FunctionArg>,
    pub params: IdxRange<Param>,
    pub vars: IdxRange<Var>,
    pub ast_id: FileAstId<ast::Function>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionArg {
    pub name: Name,
    pub is_input: bool,
    pub is_output: bool,
    pub ast_id: FileAstId<ast::FunctionArg>,
}
