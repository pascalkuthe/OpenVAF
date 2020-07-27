use core::fmt::Display;
use core::fmt::Formatter;

use annotate_snippets::snippet::AnnotationType;

use crate::diagnostic::{DiagnosticSlice, Text};
use crate::lints::{Lint, LintDiagnostic, Linter};
use crate::symbol::Ident;

use crate::analysis::data_flow::reaching_definitions::UseDefGraph;
use crate::cfg::Terminator;
use crate::data_structures::BitSet;
use crate::hir::DisciplineAccess;
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::ids::{BranchId, NetId, ParameterId, VariableId};
use crate::ir::mir::visit::ExpressionVisit;
use crate::ir::mir::Mir;
use crate::lints::builtin;
use crate::mir::Statement;
use crate::sourcemap::Span;
use std::error::Error;
use std::iter::FromIterator;

impl Mir {
    pub fn lint_unreadable_assignments(&self, cfg: &ControlFlowGraph, udg: &UseDefGraph) {
        let mut unused = BitSet::from_iter(
            cfg.blocks
                .iter()
                .map(|block| &block.statements)
                .flatten()
                .filter_map(|&stmt| {
                    if let Statement::Assignment(_, _) = self[stmt].contents {
                        Some(stmt)
                    } else {
                        None
                    }
                }),
        );

        for reaching_defintions in udg
            .terminator_use_def_chains
            .rows()
            .chain(udg.stmt_use_def_chains.rows())
        {
            unused.difference_with(reaching_defintions)
        }

        for (&stmt, &origin) in &self.statement_origins {
            if unused.contains(origin) && !unused.contains(stmt) {
                unused.set(origin, false);
            }
        }

        for stmt in unused.ones() {
            if !self.statement_origins.contains_key(&stmt) {
                Linter::dispatch_late(Box::new(UnusedStatement(self[stmt].span)), stmt.into())
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct UnusedStatement(Span);

impl Display for UnusedStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Assigned value can never be read!")
    }
}

impl Error for UnusedStatement {}

impl LintDiagnostic for UnusedStatement {
    fn lint(&self) -> Lint {
        builtin::dead_code
    }

    fn slices(&self, _main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.0.data(),
            messages: vec![(
                AnnotationType::Help,
                Text::const_str("Consider removing this statement"),
                self.0.data(),
            )],
            fold: false,
        }]
    }
}

#[derive(Debug, Copy, Clone)]
enum Item {
    Variable,
    Parameter,
    Branch,
    Net,
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable => f.write_str("variable"),
            Self::Parameter => f.write_str("parameter"),
            Self::Branch => f.write_str("branch"),
            Self::Net => f.write_str("net"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct UnusedItem(Ident, Item);

impl Display for UnusedItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} {} is never used!", self.1, self.0))
    }
}

impl Error for UnusedItem {}

impl LintDiagnostic for UnusedItem {
    fn lint(&self) -> Lint {
        match self.1 {
            Item::Variable => builtin::unused_variables,
            Item::Branch => builtin::unused_branches,
            Item::Parameter => builtin::unused_parameters,
            Item::Net => builtin::unused_nets,
        }
    }

    fn slices(&self, _main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.0.span.data(),
            messages: vec![(
                AnnotationType::Help,
                Text::const_str("Consider removing this"),
                self.0.span.data(),
            )],
            fold: false,
        }]
    }
}

impl Mir {
    pub fn lint_unused_items(&self) {
        let mut finder = UnusedItemFinder {
            mir: self,
            unused_variables: BitSet::new_filled(self.variables.len_idx()),
            unused_parameters: BitSet::new_filled(self.parameters.len_idx()),
            unused_branches: BitSet::new_filled(self.branches.len_idx()),
            unused_nets: BitSet::new_filled(self.nets.len_idx()),
        };

        // TODO only root modues
        for module in &self.modules {
            for port in module.contents.port_list.clone() {
                finder.unused_nets.set(self[port].net, false);
            }

            for (_, bb) in module.contents.analog_cfg.borrow().postorder_iter() {
                for &stmt in &bb.statements {
                    finder.visit_stmt(stmt)
                }
                if let Terminator::Split { condition, .. } = bb.terminator {
                    finder.visit_integer_expr(condition)
                }
            }
        }

        // sets nets as used that are connected to used nets trough branches
        // this already happend for used nets so we only do this for unsued nets
        let mut changed = true;
        while changed {
            changed = false;
            for branch in finder.unused_branches.ones() {
                let hi = self[branch].contents.hi;
                let lo = self[branch].contents.lo;

                if finder.unused_nets.contains(hi) != finder.unused_nets.contains(lo) {
                    finder.unused_nets.set(hi, false);
                    finder.unused_nets.set(lo, false);
                    changed = true;
                }
            }
        }

        finder.dispatch_lints()
    }
}

/// Contains a bitset for each item kind that is initially completly filled (all bits set to 1)
/// When visiting expressions/statements used items are removed from this set
/// In the end only the unused items remain
struct UnusedItemFinder<'lt> {
    mir: &'lt Mir,
    unused_variables: BitSet<VariableId>,
    unused_parameters: BitSet<ParameterId>,
    unused_branches: BitSet<BranchId>,
    unused_nets: BitSet<NetId>,
}

impl<'lt> UnusedItemFinder<'lt> {
    /// Dispatches the [`UnusedItem`] lint for any items that are still left unused
    pub fn dispatch_lints(&self) {
        /*println!(
            "Unused variable count {}",
            self.unused_variables.count_ones(..)
        );*/
        for var in self.unused_variables.ones() {
            Linter::dispatch_late(
                Box::new(UnusedItem(self.mir[var].contents.ident, Item::Variable)),
                var.into(),
            )
        }

        for param in self.unused_parameters.ones() {
            Linter::dispatch_late(
                Box::new(UnusedItem(self.mir[param].contents.ident, Item::Parameter)),
                param.into(),
            )
        }

        for net in self.unused_nets.ones() {
            Linter::dispatch_late(
                Box::new(UnusedItem(self.mir[net].contents.ident, Item::Net)),
                net.into(),
            )
        }

        for branch in self.unused_branches.ones() {
            if !self.unused_nets.contains(self.mir[branch].contents.hi)
                || !self.unused_nets.contains(self.mir[branch].contents.lo)
            {
                continue;
            }

            Linter::dispatch_late(
                Box::new(UnusedItem(self.mir[branch].contents.ident, Item::Branch)),
                branch.into(),
            )
        }
    }
}

impl<'lt> ExpressionVisit for UnusedItemFinder<'lt> {
    #[inline]
    fn mir(&self) -> &Mir {
        self.mir
    }

    fn visit_variable_reference(&mut self, var: VariableId) {
        self.unused_variables.set(var, false);
        // if the derivative of a variable is used that means its original variable was used to create that derivative
        if let Some(&origin) = self.mir.derivative_origins().get(&var) {
            self.unused_variables.set(origin, false)
        }
    }

    fn visit_parameter_reference(&mut self, param: ParameterId) {
        self.unused_parameters.set(param, false);
    }

    #[inline(always)]
    fn visit_net_reference(&mut self, net: NetId) {
        // This will probably be removed in the future because net references are only really a digital thing
        self.unused_nets.set(net, false);
    }

    #[inline(always)]
    fn visit_branch_access(
        &mut self,
        _discipline_accesss: DisciplineAccess,
        branch: BranchId,
        _time_derivative_order: u8,
    ) {
        self.unused_branches.set(branch, false);
        self.unused_nets.set(self.mir[branch].contents.hi, false);
        self.unused_nets.set(self.mir[branch].contents.lo, false);
    }
}
