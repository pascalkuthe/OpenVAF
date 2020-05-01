use crate::analysis::dominator_tree::{
    DominatorTreeBranches, DominatorTreeId, DominatorTreeNodeType,
};
use crate::analysis::DominatorTree;
use crate::ir::hir::DisciplineAccess;
use crate::ir::{
    BranchId, IntegerExpressionId, ParameterId, RealExpressionId, StringExpressionId, VariableId,
};
use crate::mir::control_flow_graph::{BasicBlockId, Terminator};
use crate::mir::{ControlFlowGraph, ExpressionId, Mir, Statement, StringExpression};
use crate::mir::{IntegerExpression, RealExpression};
use crate::symbol::Ident;
use core::mem;
use indexmap::Equivalent;
use log::debug;
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
        let false_child = branch.false_child();
        let true_child = branch.true_child();

        if false_child.is_none() && true_child.is_none() {
            return value;
        }

        let (true_value, false_value) = split(shared_data, value);

        //TODO refactor the following two if blocks into function (DRY!)
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

        let true_value = if let Some((start, leaf)) = branch.true_child() {
            self.fold_in_reverse_execution_order_internal(
                leaf,
                visit,
                merge,
                split,
                true_value,
                Some(start),
                shared_data,
            )
        } else {
            true_value
        };

        merge(shared_data, false_value, true_value, block)
    }

    pub fn variable_extraction<'mir>(
        &self,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
        mir: &Mir<'mir>,
        tracked_variables: &mut FxHashSet<VariableId<'mir>>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'mir>,
    ) -> bool {
        self.extract_variables_internal(
            mir,
            cfg,
            dependency_handler,
            &ControlFlowGraph::extract_variable_from_basic_block,
            tracked_variables,
            self.end(),
            None,
        )
    }

    fn extract_variables_internal<'mir, E, H>(
        &self,
        mir: &Mir<'mir>,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
        dependency_handler: &mut H,
        extractor: &E,
        tracked_variables: &mut FxHashSet<VariableId<'mir>>,
        mut node: DominatorTreeId<'tag>,
        start: Option<DominatorTreeId<'tag>>,
    ) -> bool
    where
        E: Fn(
            &mut ControlFlowGraph<'cfg, 'mir>,
            BasicBlockId<'cfg>,
            &Mir<'mir>,
            &mut FxHashSet<VariableId<'mir>>,
            &mut H,
        ) -> bool,
        H: ExtractionDependencyHandler<'mir>,
    {
        let mut modified = false;
        loop {
            let next = match self[node].node_type {
                DominatorTreeNodeType::Root(ref branch) => {
                    modified = self.extract_variables_from_branch(
                        mir,
                        cfg,
                        dependency_handler,
                        extractor,
                        tracked_variables,
                        self[node].basic_block,
                        branch,
                    ) || modified;
                    return extractor(
                        cfg,
                        self[node].basic_block,
                        mir,
                        tracked_variables,
                        dependency_handler,
                    ) || modified;
                }

                DominatorTreeNodeType::Branch(parent, ref branch) => {
                    modified = self.extract_variables_from_branch(
                        mir,
                        cfg,
                        dependency_handler,
                        extractor,
                        tracked_variables,
                        self[node].basic_block,
                        branch,
                    ) || modified;
                    modified = extractor(
                        cfg,
                        self[node].basic_block,
                        mir,
                        tracked_variables,
                        dependency_handler,
                    ) || modified;
                    parent.id()
                }

                DominatorTreeNodeType::Leaf(parent) => {
                    modified = extractor(
                        cfg,
                        self[node].basic_block,
                        mir,
                        tracked_variables,
                        dependency_handler,
                    ) || modified;
                    parent.id()
                }
            };
            //println!("{} -> {}", node, next);
            if let Some(start) = start {
                //println!("till {}", start);
                if node == start {
                    //println!("ending on {}", start);
                    return modified;
                }
            }
            node = next
        }
    }

    fn extract_variables_from_branch<'mir, E, H>(
        &self,
        mir: &Mir<'mir>,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
        dependency_handler: &mut H,
        extractor: &E,
        tracked_variables: &mut FxHashSet<VariableId<'mir>>,
        block: BasicBlockId<'cfg>,
        branch: &DominatorTreeBranches<'tag>,
    ) -> bool
    where
        E: Fn(
            &mut ControlFlowGraph<'cfg, 'mir>,
            BasicBlockId<'cfg>,
            &Mir<'mir>,
            &mut FxHashSet<VariableId<'mir>>,
            &mut H,
        ) -> bool,
        H: ExtractionDependencyHandler<'mir>,
    {
        let modified = match (branch.true_child(), branch.false_child()) {
            (None, None) => return false,

            (Some((start, leaf)), None) | (None, Some((start, leaf))) => {
                if let Terminator::Split {
                    condition, merge, ..
                } = cfg.blocks[block].terminator
                {
                    // Loops are special snowflakes
                    if merge == block
                    /*&& cfg.visit_branch_while(block, merge, &mut |cfg, block| {
                        cfg.blocks[block].statements.iter().any(|&stmt| {
                            if let Statement::Assignment(_, dst, _) = mir[stmt] {
                                tracked_variables.contains(&dst)
                            } else {
                                false
                            }
                        })
                    })*/
                    {
                        loop {
                            let mut tracked_variable_iteration = tracked_variables.clone();
                            if self.extract_variables_internal(
                                mir,
                                cfg,
                                dependency_handler,
                                &ControlFlowGraph::extract_variable_from_looped_basic_block,
                                &mut tracked_variable_iteration,
                                leaf,
                                Some(start),
                            ) {
                                mir.track_integer_expression(
                                    condition,
                                    &mut tracked_variable_iteration,
                                    &mut (),
                                );
                                // println!("{:#?}", tracked_variable_iteration);
                                let mut modified = false;
                                // println!("Loop iteration");
                                for var in tracked_variable_iteration {
                                    if tracked_variables.insert(var) {
                                        modified = true;
                                        // println!("{}", mir[var].contents.name);
                                    }
                                }
                                if !modified {
                                    // println!("{:#?}", tracked_variables);
                                    break;
                                }
                            } else {
                                debug!("Loop ignored");
                                break;
                            }
                        }
                    }
                    let mut tracked_variables_without_branch = tracked_variables.clone();
                    let modified = self.extract_variables_internal(
                        mir,
                        cfg,
                        dependency_handler,
                        extractor,
                        &mut tracked_variables_without_branch,
                        leaf,
                        Some(start),
                    );
                    tracked_variables.extend(tracked_variables_without_branch);
                    modified
                } else {
                    unreachable_unchecked!("Malformed dominator tree")
                }
            }

            (Some((true_start, true_leaf)), Some((false_start, false_leaf))) => {
                let mut tracked_variables_true = tracked_variables.clone();
                let mut modified = self.extract_variables_internal(
                    mir,
                    cfg,
                    dependency_handler,
                    extractor,
                    &mut tracked_variables_true,
                    true_leaf,
                    Some(true_start),
                );
                modified = self.extract_variables_internal(
                    mir,
                    cfg,
                    dependency_handler,
                    extractor,
                    tracked_variables,
                    false_leaf,
                    Some(false_start),
                ) || modified;
                tracked_variables.extend(tracked_variables_true);
                modified
            }
        };
        if let Terminator::Split {
            condition, merge, ..
        } = cfg.blocks[block].terminator
        {
            if modified {
                mir.track_integer_expression(condition, tracked_variables, dependency_handler);
                true
            } else {
                false
            }
        } else {
            unreachable_unchecked!("Malformed dominator tree")
        }
    }
}

impl<'cfg, 'mir> ControlFlowGraph<'cfg, 'mir> {
    pub fn extract_variable_from_basic_block(
        &mut self,
        block: BasicBlockId<'cfg>,
        mir: &Mir<'mir>,

        tracked_variables: &mut FxHashSet<VariableId<'mir>>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'mir>,
    ) -> bool {
        let statements = mem::take(&mut self.blocks[block].statements);

        self.blocks[block].statements = statements
            .into_iter()
            .rev()
            .filter(|&stmt| match mir[stmt] {
                Statement::Assignment(_, ref dst, val) if tracked_variables.remove(dst) => {
                    mir.track_expression(val, tracked_variables, dependency_handler);
                    true
                }
                _ => false,
            })
            .collect();

        self.blocks[block].statements.reverse();

        !self.blocks[block].statements.is_empty()
    }

    pub fn extract_variable_from_looped_basic_block(
        &mut self,
        block: BasicBlockId<'cfg>,
        mir: &Mir<'mir>,

        tracked_variables: &mut FxHashSet<VariableId<'mir>>,
        _: &mut impl ExtractionDependencyHandler<'mir>,
    ) -> bool {
        let mut modified = false;
        for &stmt in self.blocks[block].statements.iter().rev() {
            match mir[stmt] {
                Statement::Assignment(_, ref dst, val) if tracked_variables.remove(dst) => {
                    modified = true;
                    mir.track_expression(val, tracked_variables, &mut ());
                }
                _ => (),
            }
        }
        modified
    }
}

impl<'tag> Mir<'tag> {
    pub fn track_expression(
        &self,
        expr: ExpressionId<'tag>,
        tracked_variables: &mut FxHashSet<VariableId<'tag>>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match expr {
            ExpressionId::Real(expr) => {
                self.track_real_expression(expr, tracked_variables, dependency_handler)
            }
            ExpressionId::Integer(expr) => {
                self.track_integer_expression(expr, tracked_variables, dependency_handler)
            }
            ExpressionId::String(expr) => {
                self.track_string_expression(expr, tracked_variables, dependency_handler)
            }
        }
    }

    pub fn track_real_expression(
        &self,
        expr: RealExpressionId<'tag>,
        tracked_variables: &mut FxHashSet<VariableId<'tag>>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match self[expr].contents {
            RealExpression::Literal(_) => (),
            RealExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
                tracked_variables.insert(var);
            }
            RealExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }
            RealExpression::SystemFunctionCall(ident) => {
                dependency_handler.handle_system_function_call(ident)
            }

            RealExpression::BuiltInFunctionCall2p(_, arg1, arg2)
            | RealExpression::BinaryOperator(arg1, _, arg2) => {
                self.track_real_expression(arg1, tracked_variables, dependency_handler);
                self.track_real_expression(arg2, tracked_variables, dependency_handler);
            }

            RealExpression::Condition(cond, _, val1, _, val2) => {
                self.track_integer_expression(cond, tracked_variables, dependency_handler);
                self.track_real_expression(val1, tracked_variables, dependency_handler);
                self.track_real_expression(val2, tracked_variables, dependency_handler);
            }

            RealExpression::BranchAccess(discipline, branch) => {
                dependency_handler.handle_branch_reference(discipline, branch)
            }

            RealExpression::Negate(_, expr) | RealExpression::BuiltInFunctionCall1p(_, expr) => {
                self.track_real_expression(expr, tracked_variables, dependency_handler)
            }
            RealExpression::IntegerConversion(expr) => {
                self.track_integer_expression(expr, tracked_variables, dependency_handler)
            }
            RealExpression::FunctionCall(_, _) => todo!("Function calls"),
        }
    }

    pub fn track_integer_expression(
        &self,
        expr: IntegerExpressionId<'tag>,
        tracked_variables: &mut FxHashSet<VariableId<'tag>>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match self[expr].contents {
            IntegerExpression::Literal(_) => (),

            IntegerExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
                tracked_variables.insert(var);
            }
            IntegerExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }

            IntegerExpression::NetReference(_) | IntegerExpression::PortReference(_) => {
                todo!("digital")
            }

            IntegerExpression::FunctionCall(_, _) => todo!("Function calls"),

            IntegerExpression::StringEq(arg1, arg2) | IntegerExpression::StringNEq(arg1, arg2) => {
                self.track_string_expression(arg1, tracked_variables, dependency_handler);
                self.track_string_expression(arg2, tracked_variables, dependency_handler);
            }

            IntegerExpression::RealComparison(arg1, _, arg2) => {
                self.track_real_expression(arg1, tracked_variables, dependency_handler);
                self.track_real_expression(arg2, tracked_variables, dependency_handler);
            }

            IntegerExpression::RealCast(expr) => {
                self.track_real_expression(expr, tracked_variables, dependency_handler);
            }

            IntegerExpression::UnaryOperator(_, expr) | IntegerExpression::Abs(expr) => {
                self.track_integer_expression(expr, tracked_variables, dependency_handler)
            }

            IntegerExpression::Max(arg1, arg2)
            | IntegerExpression::Min(arg1, arg2)
            | IntegerExpression::BinaryOperator(arg1, _, arg2)
            | IntegerExpression::IntegerComparison(arg1, _, arg2) => {
                self.track_integer_expression(arg1, tracked_variables, dependency_handler);
                self.track_integer_expression(arg2, tracked_variables, dependency_handler);
            }

            IntegerExpression::Condition(cond, _, val1, _, val2) => {
                self.track_integer_expression(cond, tracked_variables, dependency_handler);
                self.track_integer_expression(val1, tracked_variables, dependency_handler);
                self.track_integer_expression(val2, tracked_variables, dependency_handler);
            }
        }
    }

    pub fn track_string_expression(
        &self,
        expr: StringExpressionId<'tag>,
        tracked_variables: &mut FxHashSet<VariableId<'tag>>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match self[expr].contents {
            StringExpression::Literal(val) => (),
            StringExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
                tracked_variables.insert(var);
            }
            StringExpression::Condition(cond, _, val1, _, val2) => {
                self.track_integer_expression(cond, tracked_variables, dependency_handler);
                self.track_string_expression(val1, tracked_variables, dependency_handler);
                self.track_string_expression(val2, tracked_variables, dependency_handler);
            }
            StringExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }
        }
    }
}

pub trait ExtractionDependencyHandler<'tag> {
    fn handle_variable_reference(&mut self, var: VariableId<'tag>);
    fn handle_parameter_reference(&mut self, param: ParameterId<'tag>);
    fn handle_branch_reference(&mut self, access: DisciplineAccess, branch: BranchId<'tag>);
    fn handle_system_function_call(&mut self, name: Ident);
}

impl ExtractionDependencyHandler<'_> for () {
    fn handle_variable_reference(&mut self, _: VariableId<'_>) {}

    fn handle_parameter_reference(&mut self, _: ParameterId<'_>) {}

    fn handle_branch_reference(&mut self, _: DisciplineAccess, _: BranchId<'_>) {}

    fn handle_system_function_call(&mut self, _: Ident) {}
}
