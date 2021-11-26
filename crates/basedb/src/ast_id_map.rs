//! `AstIdMap` allows to create stable IDs for "large" syntax nodes like items
//! and macro calls.
//!
//! Specifically, it enumerates all items in a file and uses position of a an
//! item as an ID. That way, id's don't change unless the set of items itself
//! changes.

use std::{
    any::type_name,
    fmt,
    hash::{Hash, Hasher},
    iter::repeat,
    marker::PhantomData,
};

use crate::BaseDB;
use arena::{Arena, ArenaMap, Idx, RawIdx};
use syntax::{ast, AstNode, AstPtr, SyntaxKind, SyntaxNode, SyntaxNodePtr};
use vfs::FileId;

/// `AstId` points to an AST node in a specific file.
pub struct AstId<N: AstNode> {
    raw: ErasedAstId,
    _ty: PhantomData<fn() -> N>,
}

impl<N: AstNode> From<AstId<N>> for ErasedAstId {
    fn from(id: AstId<N>) -> Self {
        id.raw
    }
}
impl<N: AstNode> Clone for AstId<N> {
    fn clone(&self) -> AstId<N> {
        *self
    }
}

impl<N: AstNode> Copy for AstId<N> {}

impl<N: AstNode> PartialEq for AstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<N: AstNode> Eq for AstId<N> {}
impl<N: AstNode> Hash for AstId<N> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.raw.hash(hasher);
    }
}

impl<N: AstNode> fmt::Debug for AstId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AstId::<{}>({})", type_name::<N>(), RawIdx::from(self.raw))
    }
}

impl<N: AstNode> AstId<N> {
    // Can't make this a From implementation because of coherence
    pub fn upcast<M: AstNode>(self) -> AstId<M>
    where
        N: Into<M>,
    {
        AstId { raw: self.raw, _ty: PhantomData }
    }
}

pub type ErasedAstId = Idx<SyntaxNodePtr>;

/// Maps items' `SyntaxNode`s to `ErasedAstId`s and back.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    parents: ArenaMap<SyntaxNodePtr, Option<ErasedAstId>>,
}

pub(crate) fn has_id_map_entry(kind: SyntaxKind) -> bool {
    if ast::BodyPortDecl::can_cast(kind) {
        // This just adds a semicolon to a port decl... No need to add the same port twice
        false
    } else {
        ast::Item::can_cast(kind)
            || ast::BlockStmt::can_cast(kind)
            || ast::Param::can_cast(kind)
            || ast::Var::can_cast(kind)
            || ast::FunctionArg::can_cast(kind)
            || ast::NatureAttr::can_cast(kind)
            || ast::DisciplineAttr::can_cast(kind)
            || ast::PortDecl::can_cast(kind)
            || ast::ModuleItem::can_cast(kind)
            || ast::AnalogBehaviour::can_cast(kind)
    }
}

impl AstIdMap {
    pub(crate) fn from_source(node: &SyntaxNode) -> AstIdMap {
        assert!(node.parent().is_none());
        let mut res = AstIdMap::default();
        // By walking the tree in breadth-first order we make sure that parents
        // get lower ids then children. That is, adding a new child does not
        // change parent's id. This means that, say, adding a new function to a
        // module does not change ids of top-level items, which helps caching.
        //
        // Compared to rust analyzer the parent mapping was added here for lint attribute
        // resolution
        // TODO does this hurt caching in any way. Probably not:
        // if the parent changes then something before changed aswell and the item is moved anyway)
        bdfs(node, |it, parent| has_id_map_entry(it.kind()).then(|| res.alloc(it, parent)));
        res
    }

    pub fn ast_id<N: AstNode>(&self, item: &N) -> AstId<N> {
        let raw = self.erased_ast_id(item.syntax());
        AstId { raw, _ty: PhantomData }
    }

    pub(crate) fn erased_ast_id(&self, item: &SyntaxNode) -> ErasedAstId {
        let ptr = SyntaxNodePtr::new(item);
        self.erased_ast_id_of_ptr(ptr)
    }

    pub(crate) fn erased_ast_id_of_ptr(&self, ptr: SyntaxNodePtr) -> ErasedAstId {
        match self.arena.iter_enumerated().find(|(_id, i)| **i == ptr) {
            Some((it, _)) => it,
            None => panic!(
                "Can't find {:?} in AstIdMap",
                ptr,
                // self.arena.iter_enumerated().map(|(_id, i)| i).collect::<Vec<_>>(),
            ),
        }
    }

    pub fn nearest_ast_id_to_node(&self, mut node: SyntaxNode) -> Option<ErasedAstId> {
        while !has_id_map_entry(node.kind()) {
            node = node.parent()?;
        }
        Some(self.erased_ast_id(&node))
    }

    pub fn nearest_ast_id_to_ptr(
        &self,
        ptr: SyntaxNodePtr,
        db: &dyn BaseDB,
        root_file: FileId,
    ) -> Option<ErasedAstId> {
        if has_id_map_entry(ptr.syntax_kind()) {
            Some(self.erased_ast_id_of_ptr(ptr))
        } else {
            let node = ptr.to_node(db.parse(root_file).tree().syntax());
            self.nearest_ast_id_to_node(node)
        }
    }

    pub fn get<N: AstNode>(&self, id: AstId<N>) -> AstPtr<N> {
        self.arena[id.raw].cast::<N>().unwrap()
    }

    pub(crate) fn entries(&self) -> impl Iterator<Item = (ErasedAstId, &SyntaxNodePtr)> {
        self.arena.iter_enumerated()
    }
    pub fn get_syntax(&self, id: ErasedAstId) -> SyntaxNodePtr {
        self.arena[id]
    }

    pub(crate) fn get_parent(&self, id: ErasedAstId) -> Option<ErasedAstId> {
        self.parents[id]
    }

    fn alloc(&mut self, item: &SyntaxNode, parent: Option<ErasedAstId>) -> ErasedAstId {
        let id1 = self.parents.push_and_get_key(parent);
        let id2 = self.arena.push_and_get_key(SyntaxNodePtr::new(item));
        debug_assert_eq!(id1, id2);
        id2
    }
}

/// Walks the subtree in bdfs order, calling `f` for each node. What is bdfs
/// order? It is a mix of breadth-first and depth first orders. Nodes for which
/// `f` returns true are visited breadth-first, all the other nodes are explored
/// depth-first.
///
/// In other words, the size of the bfs queue is bound by the number of "true"
/// nodes.
fn bdfs(
    node: &SyntaxNode,
    mut f: impl FnMut(&SyntaxNode, Option<ErasedAstId>) -> Option<ErasedAstId>,
) {
    let mut curr_layer = vec![(node.clone(), None)];
    let mut next_layer = vec![];
    while !curr_layer.is_empty() {
        curr_layer.drain(..).for_each(|(node, id)| {
            let mut preorder = node.preorder();
            while let Some(event) = preorder.next() {
                match event {
                    syntax::WalkEvent::Enter(node) => {
                        if let Some(it) = f(&node, id) {
                            next_layer.extend(node.children().zip(repeat(Some(it))));
                            preorder.skip_subtree();
                        }
                    }
                    syntax::WalkEvent::Leave(_) => {}
                }
            }
        });
        std::mem::swap(&mut curr_layer, &mut next_layer);
    }
}
