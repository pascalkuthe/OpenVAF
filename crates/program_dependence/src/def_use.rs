use bitset::{BitSet, HybridBitSet, HybridIter, SparseBitMatrix};
use cfg::{BasicBlock, Local};
use stdx::impl_from;

use crate::use_def::UseDefGraph;
use crate::Assignment;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Def {
    Local(Local),
    Assignment(Assignment),
}

impl_from!(Local,Assignment for Def);

pub struct Uses<'a> {
    pub locals: Option<&'a HybridBitSet<Local>>,
    pub assignments: Option<&'a HybridBitSet<Assignment>>,
    pub terminators: Option<&'a HybridBitSet<BasicBlock>>,
}

pub struct DependentDefs<'a> {
    pub locals: Option<HybridIter<'a, Local>>,
    pub assignments: Option<HybridIter<'a, Assignment>>,
}

impl<'a> Iterator for DependentDefs<'a> {
    type Item = Def;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(local) = self.locals.as_mut().and_then(|iter| iter.next()) {
            Some(local.into())
        } else {
            self.assignments.as_mut().and_then(|iter| iter.next()).map(|assign| assign.into())
        }
    }
}

#[derive(Debug, Clone)]
pub struct DefUseGraph {
    assign_local: SparseBitMatrix<Assignment, Local>,
    assign_assign: SparseBitMatrix<Assignment, Assignment>,
    assign_term: SparseBitMatrix<Assignment, BasicBlock>,
    local_local: SparseBitMatrix<Local, Local>,
    local_assign: SparseBitMatrix<Local, Assignment>,
    local_term: SparseBitMatrix<Local, BasicBlock>,
}

impl DefUseGraph {
    pub fn build(udg: &UseDefGraph) -> DefUseGraph {
        let mut def_use_graph = DefUseGraph {
            assign_local: udg.local_assign.inverse(),
            assign_assign: udg.assign_assign.inverse(),
            assign_term: udg.term_assign.inverse(),
            local_local: udg.local_local.inverse(),
            local_assign: udg.assign_local.inverse(),
            local_term: SparseBitMatrix::new(
                udg.local_local.num_rows(),
                udg.term_assign.num_rows(),
            ),
        };

        for (bb, local) in udg.term_local.iter() {
            def_use_graph.local_term.insert(*local, *bb);
        }

        def_use_graph
    }

    #[inline]
    pub fn dependent_defs(&self, def: Def) -> DependentDefs<'_> {
        match def {
            Def::Local(local) => DependentDefs {
                locals: self.local_local.row(local).map(|uses| uses.iter()),
                assignments: self.local_assign.row(local).map(|uses| uses.iter()),
            },
            Def::Assignment(assign) => DependentDefs {
                locals: self.assign_local.row(assign).map(|uses| uses.iter()),
                assignments: self.assign_assign.row(assign).map(|uses| uses.iter()),
            },
        }
    }
    #[inline]
    pub fn uses(&self, def: Def) -> Uses<'_> {
        match def {
            Def::Local(local) => Uses {
                locals: self.local_local.row(local),
                assignments: self.local_assign.row(local),
                terminators: self.local_term.row(local),
            },
            Def::Assignment(assign) => Uses {
                locals: self.assign_local.row(assign),
                assignments: self.assign_assign.row(assign),
                terminators: self.assign_term.row(assign),
            },
        }
    }

    pub fn dependent_defs_rec_postorder(
        &self,
        start: impl IntoIterator<Item = Def>,
    ) -> Postorder<'_> {
        Postorder::new(self, start)
    }
}

pub struct Postorder<'a> {
    pub dug: &'a DefUseGraph,
    pub visit_stack: Vec<(Def, DependentDefs<'a>)>,
    pub visited_locals: BitSet<Local>,
    pub visited_assigns: BitSet<Assignment>,
}

impl<'a> Postorder<'a> {
    pub fn new(dug: &'a DefUseGraph, start_defs: impl IntoIterator<Item = Def>) -> Postorder {
        let mut po = Postorder {
            dug,
            visit_stack: Vec::new(),
            visited_locals: BitSet::new_empty(dug.local_local.num_rows()),
            visited_assigns: BitSet::new_empty(dug.assign_assign.num_rows()),
        };

        for start in start_defs.into_iter() {
            po.mark_visited(start);
            po.visit_stack.push((start, dug.dependent_defs(start)));
        }

        po.traverse_successor();
        po
    }

    fn mark_visited(&mut self, def: Def) -> bool {
        match def {
            Def::Local(local) => self.visited_locals.insert(local),
            Def::Assignment(assign) => self.visited_assigns.insert(assign),
        }
    }

    fn traverse_successor(&mut self) {
        while let Some(def) = self.visit_stack.last_mut().and_then(|(_, iter)| iter.next()) {
            if self.mark_visited(def) {
                self.visit_stack.push((def, self.dug.dependent_defs(def)));
            }
        }
    }
}

impl Iterator for Postorder<'_> {
    type Item = Def;

    fn next(&mut self) -> Option<Def> {
        self.visit_stack.pop().map(|(next, _)| {
            self.traverse_successor();
            next
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.visit_stack.len(), None)
    }
}
