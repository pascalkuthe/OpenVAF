use bitset::BitSet;
use once_cell::sync::OnceCell;

use crate::{BasicBlock, ControlFlowGraph};

/// Helper type to cache the result of `graph::is_cyclic`.
#[derive(Clone, Debug, Default)]
pub struct GraphIsCyclicCache {
    cache: OnceCell<bool>,
}

impl GraphIsCyclicCache {
    pub(super) fn is_cyclic(&self, cfg: &ControlFlowGraph) -> bool {
        *self.cache.get_or_init(|| {
            TriColorDepthFirstSearch::new(cfg).run_from(cfg.entry(), &mut CycleDetector).is_some()
        })
    }

    /// Invalidates the cache.
    #[inline]
    pub fn invalidate(&mut self) {
        // Invalidating the cache requires mutating the MIR, which in turn requires a unique
        // reference (`&mut`) to the `mir::Body`. Because of this, we can assume that all
        // callers of `invalidate` have a unique reference to the MIR and thus to the
        // cache. This means we never need to do synchronization when `invalidate` is called,
        // we can simply reinitialize the `OnceCell`.
        self.cache = OnceCell::new();
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ControlFlow<B> {
    /// Move on to the next phase of the operation as normal.
    Continue,
    /// Exit the operation without running subsequent phases.
    Break(B),
    // Yes, the order of the variants doesn't match the type parameters.
    // They're in this order so that `ControlFlow<A, B>` <-> `Result<B, A>`
    // is a no-op conversion in the `Try` implementation.
}

/// The status of a node in the depth-first search.
///
/// See the documentation of `TriColorDepthFirstSearch` to see how a node's status is updated
/// during DFS.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeStatus {
    /// This node has been examined by the depth-first search but is not yet `Settled`.
    ///
    /// Also referred to as "gray" or "discovered" nodes in [CLR].
    ///
    /// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
    Visited,

    /// This node and all nodes reachable from it have been examined by the depth-first search.
    ///
    /// Also referred to as "black" or "finished" nodes in [CLR].
    ///
    /// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
    Settled,
}

struct Event<N> {
    node: N,
    becomes: NodeStatus,
}

/// A depth-first search that also tracks when all successors of a node have been examined.
///
/// This is based on the DFS described in [Introduction to Algorithms (1st ed.)][CLR], hereby
/// referred to as **CLR**. However, we use the terminology in [`NodeStatus`] above instead of
/// "discovered"/"finished" or "white"/"grey"/"black". Each node begins the search with no status,
/// becomes `Visited` when it is first examined by the DFS and is `Settled` when all nodes
/// reachable from it have been examined. This allows us to differentiate between "tree", "back"
/// and "forward" edges (see [`TriColorVisitor::node_examined`]).
///
/// Unlike the pseudocode in [CLR], this implementation is iterative and does not use timestamps.
/// We accomplish this by storing `Event`s on the stack that result in a (possible) state change
/// for each node. A `Visited` event signifies that we should examine this node if it has not yet
/// been `Visited` or `Settled`. When a node is examined for the first time, we mark it as
/// `Visited` and push a `Settled` event for it on stack followed by `Visited` events for all of
/// its predecessors, scheduling them for examination. Multiple `Visited` events for a single node
/// may exist on the stack simultaneously if a node has multiple predecessors, but only one
/// `Settled` event will ever be created for each node. After all `Visited` events for a node's
/// successors have been popped off the stack (as well as any new events triggered by visiting
/// those successors), we will pop off that node's `Settled` event.
///
/// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
pub struct TriColorDepthFirstSearch<'graph> {
    graph: &'graph ControlFlowGraph,
    stack: Vec<Event<BasicBlock>>,
    visited: BitSet<BasicBlock>,
    settled: BitSet<BasicBlock>,
}

impl<'a> TriColorDepthFirstSearch<'a> {
    pub fn new(graph: &'a ControlFlowGraph) -> Self {
        TriColorDepthFirstSearch {
            graph,
            stack: vec![],
            visited: BitSet::new_empty(graph.blocks.len()),
            settled: BitSet::new_empty(graph.blocks.len()),
        }
    }

    /// Performs a depth-first search, starting from the given `root`.
    ///
    /// This won't visit nodes that are not reachable from `root`.
    pub fn run_from<V: TriColorVisitor>(
        mut self,
        root: BasicBlock,
        visitor: &mut V,
    ) -> Option<V::BreakVal> {
        use NodeStatus::{Settled, Visited};
        self.stack.push(Event { node: root, becomes: Visited });

        loop {
            match self.stack.pop()? {
                Event { node, becomes: Settled } => {
                    let not_previously_settled = self.settled.insert(node);
                    debug_assert!(not_previously_settled, "A node should be settled exactly once");
                    if let ControlFlow::Break(val) = visitor.node_settled(node) {
                        return Some(val);
                    }
                }

                Event { node, becomes: Visited } => {
                    let not_previously_visited = self.visited.insert(node);
                    let prior_status = if not_previously_visited {
                        None
                    } else if self.settled.contains(node) {
                        Some(Settled)
                    } else {
                        Some(Visited)
                    };

                    if let ControlFlow::Break(val) = visitor.node_examined(node, prior_status) {
                        return Some(val);
                    }

                    // If this node has already been examined, we are done.
                    if prior_status.is_some() {
                        continue;
                    }

                    // Otherwise, push a `Settled` event for this node onto the stack, then
                    // schedule its successors for examination.
                    self.stack.push(Event { node, becomes: Settled });
                    for succ in self.graph.successors(node) {
                        if !visitor.ignore_edge(node, succ) {
                            self.stack.push(Event { node: succ, becomes: Visited });
                        }
                    }
                }
            }
        }
    }
}

/// What to do when a node is examined or becomes `Settled` during DFS.
pub trait TriColorVisitor {
    /// The value returned by this search.
    type BreakVal;

    /// Called when a node is examined by the depth-first search.
    ///
    /// By checking the value of `prior_status`, this visitor can determine whether the edge
    /// leading to this node was a tree edge (`None`), forward edge (`Some(Settled)`) or back edge
    /// (`Some(Visited)`). For a full explanation of each edge type, see the "Depth-first Search"
    /// chapter in [CLR] or [wikipedia].
    ///
    /// If you want to know *both* nodes linked by each edge, you'll need to modify
    /// `TriColorDepthFirstSearch` to store a `source` node for each `Visited` event.
    ///
    /// [wikipedia]: https://en.wikipedia.org/wiki/Depth-first_search#Output_of_a_depth-first_search
    /// [CLR]: https://en.wikipedia.org/wiki/Introduction_to_Algorithms
    fn node_examined(
        &mut self,
        _node: BasicBlock,
        _prior_status: Option<NodeStatus>,
    ) -> ControlFlow<Self::BreakVal> {
        ControlFlow::Continue
    }

    /// Called after all nodes reachable from this one have been examined.
    fn node_settled(&mut self, _node: BasicBlock) -> ControlFlow<Self::BreakVal> {
        ControlFlow::Continue
    }

    /// Behave as if no edges exist from `source` to `target`.
    fn ignore_edge(&mut self, _source: BasicBlock, _target: BasicBlock) -> bool {
        false
    }
}

/// This `TriColorVisitor` looks for back edges in a graph, which indicate that a cycle exists.
pub struct CycleDetector;

impl TriColorVisitor for CycleDetector {
    type BreakVal = ();

    fn node_examined(
        &mut self,
        _node: BasicBlock,
        prior_status: Option<NodeStatus>,
    ) -> ControlFlow<Self::BreakVal> {
        match prior_status {
            Some(NodeStatus::Visited) => ControlFlow::Break(()),
            _ => ControlFlow::Continue,
        }
    }
}
