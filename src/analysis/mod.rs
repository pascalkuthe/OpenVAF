//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/VARF/blob/master/LICENSE.
//  *  No part of VARF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//TODO logic analyser
//mod z3_expressions;
pub mod dominator_tree;
pub use dominator_tree::DominatorTree;
#[cfg(test)]
mod test;

use crate::analysis::dominator_tree::{
    DominatorTreeBranches, DominatorTreeId, DominatorTreeNodeType,
};
use crate::ast::UnaryOperator;
use crate::ir::hir::DisciplineAccess;
use crate::ir::{
    BlockId, BranchId, IntegerExpressionId, ParameterId, RealExpressionId, StatementId,
    StringExpressionId, VariableId,
};
use crate::mir::control_flow_graph::{BasicBlockId, Terminator};
use crate::mir::{ControlFlowGraph, ExpressionId, Mir, Statement, StringExpression};
use crate::mir::{IntegerExpression, RealExpression};
use crate::symbol::Ident;
use ahash::AHashSet;
use core::mem;
use log::*;
use rustc_hash::{FxHashMap, FxHashSet};

impl<'tag, 'cfg> DominatorTree<'tag, 'cfg> {
    pub fn fold_in_reverse_execution_order<M, V, T, P, S>(
        &self,
        mut visit: V,
        mut merge: M,
        mut split: P,
        initial: T,
        shared_data: &mut S,
    ) -> T
    where
        V: FnMut(&mut S, BasicBlockId<'cfg>, T) -> T,
        M: FnMut(&mut S, T, T, BasicBlockId<'cfg>) -> T,
        P: FnMut(&mut S, T) -> (T, T),
    {
        self.fold_in_reverse_execution_order_internal(
            self.end(),
            &mut visit,
            &mut merge,
            &mut split,
            initial,
            None,
            shared_data,
        )
    }
    fn fold_in_reverse_execution_order_internal<M, V, T, S, P>(
        &self,
        mut node: DominatorTreeId<'tag>,
        mut visit: &mut V,
        merge: &mut M,
        split: &mut P,
        mut value: T,
        start: Option<DominatorTreeId<'tag>>,
        shared_data: &mut S,
    ) -> T
    where
        V: FnMut(&mut S, BasicBlockId<'cfg>, T) -> T,
        M: FnMut(&mut S, T, T, BasicBlockId<'cfg>) -> T,
        P: FnMut(&mut S, T) -> (T, T),
    {
        loop {
            let next = match self[node].node_type {
                DominatorTreeNodeType::Root(ref branch) => {
                    value = self.fold_branch_in_reverse_execution_order(
                        self[node].basic_block,
                        branch,
                        visit,
                        merge,
                        split,
                        value,
                        shared_data,
                    );
                    value = visit(shared_data, self[node].basic_block, value);
                    break;
                }
                DominatorTreeNodeType::Branch(parent, ref branch) => {
                    value = self.fold_branch_in_reverse_execution_order(
                        self[node].basic_block,
                        branch,
                        visit,
                        merge,
                        split,
                        value,
                        shared_data,
                    );
                    value = visit(shared_data, self[node].basic_block, value);
                    parent.id()
                }
                DominatorTreeNodeType::Leaf(parent) => {
                    value = visit(shared_data, self[node].basic_block, value);
                    parent.id()
                }
            };
            //println!("{} -> {}", node, next);
            if let Some(start) = start {
                //   println!("till {}", start);
                if node == start {
                    break;
                }
            }
            node = next
        }
        value
    }

    fn fold_branch_in_reverse_execution_order<M, V, T, P, S>(
        &self,
        block: BasicBlockId<'cfg>,
        branch: &DominatorTreeBranches<'tag>,
        mut visit: &mut V,
        merge: &mut M,
        split: &mut P,
        mut value: T,
        shared_data: &mut S,
    ) -> T
    where
        V: FnMut(&mut S, BasicBlockId<'cfg>, T) -> T,
        M: FnMut(&mut S, T, T, BasicBlockId<'cfg>) -> T,
        P: FnMut(&mut S, T) -> (T, T),
    {
        let (true_value, false_value) = split(shared_data, value);
        let false_value = if let Some((start, leaf)) = branch.false_child() {
            self.fold_in_reverse_execution_order_internal(
                leaf,
                visit,
                merge,
                split,
                false_value,
                Some(start),
                shared_data,
            )
        } else {
            false_value
        };

        let true_value = self.fold_in_reverse_execution_order_internal(
            branch.true_child_main_leaf(),
            visit,
            merge,
            split,
            true_value,
            Some(branch.true_child()),
            shared_data,
        );
        merge(shared_data, false_value, true_value, block)
    }

    #[inline]
    pub fn variable_extraction<'mir>(
        self,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
        mir: &Mir<'mir>,
        variables_to_extract: FxHashSet<VariableId<'mir>>,
        mut handle_variable_reference: impl FnMut(VariableId<'mir>),
        mut handle_parameter_reference: impl FnMut(ParameterId<'mir>),
        mut handle_branch_reference: impl FnMut(DisciplineAccess, BranchId<'mir>),
        mut handle_system_function_call: impl FnMut(Ident),
    ) -> FxHashSet<VariableId<'mir>> {
        self.fold_in_reverse_execution_order(
            |(mir, cfg, hv, hp, hb, hs), block, (tracked, modified)| {
                extract_variable_from_basic_block(
                    *mir, cfg, block, modified, tracked, hv, hp, hb, hs,
                )
            },
            |(mir, cfg, hv, hp, hb, hs),
             (false_tracked, false_modified),
             (true_tracked, true_modified),
             block| {
                merge_extraction_split(
                    *mir,
                    *cfg,
                    block,
                    true_modified,
                    true_tracked,
                    false_modified,
                    false_tracked,
                    hv,
                    hp,
                    hb,
                    hs,
                )
            },
            |_, (tracked, _)| ((tracked.clone(), false), (tracked.clone(), false)),
            (variables_to_extract, false),
            &mut (
                mir,
                cfg,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            ),
        )
        .0
    }
}
pub fn merge_extraction_split<'mir, 'cfg>(
    mir: &Mir<'mir>,
    cfg: &ControlFlowGraph<'cfg, 'mir>,
    block: BasicBlockId<'cfg>,
    true_modified: bool,
    mut tracked_variables_true_block: FxHashSet<VariableId<'mir>>,
    false_modified: bool,
    mut tracked_variables_false_block: FxHashSet<VariableId<'mir>>,
    handle_variable_reference: &mut impl FnMut(VariableId<'mir>),
    handle_parameter_reference: &mut impl FnMut(ParameterId<'mir>),
    handle_branch_reference: &mut impl FnMut(DisciplineAccess, BranchId<'mir>),
    handle_system_function_call: &mut impl FnMut(Ident),
) -> (FxHashSet<VariableId<'mir>>, bool) {
    tracked_variables_false_block.extend(tracked_variables_true_block);

    if let Terminator::Split {
        condition,
        false_block,
        merge,
        ..
    } = cfg.blocks[block].terminator
    {
        let modified = (false_modified && merge != block) || true_modified;

        if modified {
            track_integer_expression(
                &mir,
                condition,
                &mut tracked_variables_false_block,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }

        (tracked_variables_false_block, modified)
    } else {
        unreachable_unchecked!("Malformed dominator tree")
    }
}

pub fn extract_variable_from_basic_block<'mir, 'cfg>(
    mir: &Mir<'mir>,
    cfg: &mut ControlFlowGraph<'cfg, 'mir>,
    block: BasicBlockId<'cfg>,
    mut modified: bool,
    mut tracked_variables: FxHashSet<VariableId<'mir>>,
    handle_variable_reference: &mut impl FnMut(VariableId<'mir>),
    handle_parameter_reference: &mut impl FnMut(ParameterId<'mir>),
    handle_branch_reference: &mut impl FnMut(DisciplineAccess, BranchId<'mir>),
    handle_system_function_call: &mut impl FnMut(Ident),
) -> (FxHashSet<VariableId<'mir>>, bool) {
    let statements = mem::take(&mut cfg.blocks[block].statements);
    cfg.blocks[block].statements = statements
        .into_iter()
        .rev()
        .filter(|&stmt| match mir[stmt] {
            Statement::Assignment(_, ref dst, val) if tracked_variables.remove(dst) => {
                track_expression(
                    mir,
                    val,
                    &mut tracked_variables,
                    handle_variable_reference,
                    handle_parameter_reference,
                    handle_branch_reference,
                    handle_system_function_call,
                );
                true
            }
            _ => false,
        })
        .collect();

    cfg.blocks[block].statements.reverse();
    (
        tracked_variables,
        modified || !cfg.blocks[block].statements.is_empty(),
    )
}

pub fn track_expression<'tag>(
    mir: &Mir<'tag>,
    expr: ExpressionId<'tag>,
    tracked_variables: &mut FxHashSet<VariableId<'tag>>,
    handle_variable_reference: &mut impl FnMut(VariableId<'tag>),
    handle_parameter_reference: &mut impl FnMut(ParameterId<'tag>),
    handle_branch_reference: &mut impl FnMut(DisciplineAccess, BranchId<'tag>),
    handle_system_function_call: &mut impl FnMut(Ident),
) {
    match expr {
        ExpressionId::Real(expr) => track_real_expression(
            mir,
            expr,
            tracked_variables,
            handle_variable_reference,
            handle_parameter_reference,
            handle_branch_reference,
            handle_system_function_call,
        ),
        ExpressionId::Integer(expr) => track_integer_expression(
            mir,
            expr,
            tracked_variables,
            handle_variable_reference,
            handle_parameter_reference,
            handle_branch_reference,
            handle_system_function_call,
        ),
        ExpressionId::String(expr) => track_string_expression(
            mir,
            expr,
            tracked_variables,
            handle_variable_reference,
            handle_parameter_reference,
            handle_branch_reference,
            handle_system_function_call,
        ),
    }
}

pub fn track_real_expression<'tag>(
    mir: &Mir<'tag>,
    expr: RealExpressionId<'tag>,
    tracked_variables: &mut FxHashSet<VariableId<'tag>>,
    handle_variable_reference: &mut impl FnMut(VariableId<'tag>),
    handle_parameter_reference: &mut impl FnMut(ParameterId<'tag>),
    handle_branch_reference: &mut impl FnMut(DisciplineAccess, BranchId<'tag>),
    handle_system_function_call: &mut impl FnMut(Ident),
) {
    match mir[expr].contents {
        RealExpression::Literal(_) => (),
        RealExpression::VariableReference(var) => {
            handle_variable_reference(var);
            tracked_variables.insert(var);
        }
        RealExpression::ParameterReference(param) => handle_parameter_reference(param),
        RealExpression::SystemFunctionCall(ident) => handle_system_function_call(ident),

        RealExpression::BuiltInFunctionCall2p(_, arg1, arg2)
        | RealExpression::BinaryOperator(arg1, _, arg2) => {
            track_real_expression(
                mir,
                arg1,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_real_expression(
                mir,
                arg2,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }

        RealExpression::Condition(cond, _, val1, _, val2) => {
            track_integer_expression(
                mir,
                cond,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_real_expression(
                mir,
                val1,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_real_expression(
                mir,
                val2,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }

        RealExpression::BranchAccess(discipline, branch) => {
            handle_branch_reference(discipline, branch)
        }

        RealExpression::Negate(_, expr) | RealExpression::BuiltInFunctionCall1p(_, expr) => {
            track_real_expression(
                mir,
                expr,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            )
        }
        RealExpression::IntegerConversion(expr) => track_integer_expression(
            mir,
            expr,
            tracked_variables,
            handle_variable_reference,
            handle_parameter_reference,
            handle_branch_reference,
            handle_system_function_call,
        ),
        RealExpression::FunctionCall(_, _) => todo!("Function calls"),
    }
}

pub fn track_integer_expression<'tag>(
    mir: &Mir<'tag>,
    expr: IntegerExpressionId<'tag>,
    tracked_variables: &mut FxHashSet<VariableId<'tag>>,
    handle_variable_reference: &mut impl FnMut(VariableId<'tag>),
    handle_parameter_reference: &mut impl FnMut(ParameterId<'tag>),
    handle_branch_reference: &mut impl FnMut(DisciplineAccess, BranchId<'tag>),
    handle_system_function_call: &mut impl FnMut(Ident),
) {
    match mir[expr].contents {
        IntegerExpression::Literal(_) => (),

        IntegerExpression::VariableReference(var) => {
            handle_variable_reference(var);
            tracked_variables.insert(var);
        }
        IntegerExpression::ParameterReference(param) => handle_parameter_reference(param),

        IntegerExpression::NetReference(_) | IntegerExpression::PortReference(_) => {
            todo!("digital")
        }

        IntegerExpression::FunctionCall(_, _) => todo!("Function calls"),

        IntegerExpression::StringEq(arg1, arg2) | IntegerExpression::StringNEq(arg1, arg2) => {
            track_string_expression(
                mir,
                arg1,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_string_expression(
                mir,
                arg2,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }

        IntegerExpression::RealComparison(arg1, _, arg2) => {
            track_real_expression(
                mir,
                arg1,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_real_expression(
                mir,
                arg2,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }

        IntegerExpression::RealCast(expr) => {
            track_real_expression(
                mir,
                expr,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }

        IntegerExpression::UnaryOperator(_, expr) | IntegerExpression::Abs(expr) => {
            track_integer_expression(
                mir,
                expr,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            )
        }

        IntegerExpression::Max(arg1, arg2)
        | IntegerExpression::Min(arg1, arg2)
        | IntegerExpression::BinaryOperator(arg1, _, arg2)
        | IntegerExpression::IntegerComparison(arg1, _, arg2) => {
            track_integer_expression(
                mir,
                arg1,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_integer_expression(
                mir,
                arg2,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }

        IntegerExpression::Condition(cond, _, val1, _, val2) => {
            track_integer_expression(
                mir,
                cond,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_integer_expression(
                mir,
                val1,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_integer_expression(
                mir,
                val2,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }
    }
}

pub fn track_string_expression<'tag>(
    mir: &Mir<'tag>,
    expr: StringExpressionId<'tag>,
    tracked_variables: &mut FxHashSet<VariableId<'tag>>,
    handle_variable_reference: &mut impl FnMut(VariableId<'tag>),
    handle_parameter_reference: &mut impl FnMut(ParameterId<'tag>),
    handle_branch_reference: &mut impl FnMut(DisciplineAccess, BranchId<'tag>),
    handle_system_function_call: &mut impl FnMut(Ident),
) {
    match mir[expr].contents {
        StringExpression::Literal(val) => (),
        StringExpression::VariableReference(var) => {
            handle_variable_reference(var);
            tracked_variables.insert(var);
        }
        StringExpression::Condition(cond, _, val1, _, val2) => {
            track_integer_expression(
                mir,
                cond,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_string_expression(
                mir,
                val1,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
            track_string_expression(
                mir,
                val2,
                tracked_variables,
                handle_variable_reference,
                handle_parameter_reference,
                handle_branch_reference,
                handle_system_function_call,
            );
        }
        StringExpression::ParameterReference(param) => handle_parameter_reference(param),
    }
}
