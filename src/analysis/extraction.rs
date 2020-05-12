use crate::analysis::data_flow::framework::DataFlowGraph;
use crate::analysis::data_flow::reaching_variables::{DefiningSet, UseDefGraph};
use crate::analysis::dominator_tree::DominatorTreeNode;
use crate::analysis::DominatorTree;
use crate::ir::hir::DisciplineAccess;
use crate::ir::mir::ControlFlowGraph;
use crate::ir::{
    BranchId, IntegerExpressionId, ParameterId, RealExpressionId, StatementId, StringExpressionId,
    VariableId,
};
use crate::mir::control_flow_graph::BasicBlockId;
use crate::mir::{ExpressionId, Mir, StringExpression};
use crate::mir::{IntegerExpression, RealExpression};
use crate::symbol::Ident;
use crate::BitSet;

impl<'cfg, 'mir> ControlFlowGraph<'cfg, 'mir> {
    pub fn extract_relevant_statements_for_variable(
        &mut self,
        var: VariableId<'mir>,
        udg: &UseDefGraph<'mir, 'cfg>,
        dfg: &DataFlowGraph<'cfg>,
        dtree: &DominatorTree<'cfg>,
    ) {
        self.eliminate_dead_code(
            &mut udg.get_assignments(var).clone(),
            |_| true,
            udg,
            dfg,
            dtree,
        );
    }

    pub fn extract_relevant_statements_for_variable_assuming_input(
        &mut self,
        input: DefiningSet,
        var: VariableId<'mir>,
        udg: &UseDefGraph<'mir, 'cfg>,
        dfg: &DataFlowGraph<'cfg>,
        dtree: &DominatorTree<'cfg>,
    ) {
        self.eliminate_dead_code(
            &mut udg.get_assignments(var).clone(),
            |stmt| !input.contains(stmt),
            udg,
            dfg,
            dtree,
        );
    }

    pub fn extract_relevant_statements_for_variable_assuming_input_variables(
        &mut self,
        input: impl Iterator<Item = VariableId<'mir>>,
        var: VariableId<'mir>,
        udg: &UseDefGraph<'mir, 'cfg>,
        dfg: &DataFlowGraph<'cfg>,
        dtree: &DominatorTree<'cfg>,
    ) {
        let mut input_statements = BitSet::with_capacity(udg.statement_count as usize);
        for variable in input {
            input_statements.union_with(&udg.get_assignments(variable).0)
        }
        self.extract_relevant_statements_for_variable_assuming_input(
            DefiningSet(input_statements),
            var,
            udg,
            dfg,
            dtree,
        )
    }

    pub fn eliminate_dead_code(
        &mut self,
        relevant_stmts: &mut DefiningSet,
        mut predicate: impl FnMut(StatementId<'mir>) -> bool,
        udg: &UseDefGraph<'mir, 'cfg>,
        dfg: &DataFlowGraph<'cfg>,
        dtree: &DominatorTree<'cfg>,
    ) {
        relevant_stmts.0.intersect_with(&dfg.out_sets[self.end()]);

        self.eliminate_dead_code_internal(
            relevant_stmts,
            &mut predicate,
            udg,
            dfg,
            dtree,
            self.end(),
            None,
        );

        self.for_all_blocks_mut(|cfg, block| {
            cfg[block]
                .statements
                .retain(|&stmt| relevant_stmts.contains(stmt))
        })
    }

    fn eliminate_dead_code_internal(
        &self,
        relevant_stmts: &mut DefiningSet,
        pred: &mut impl FnMut(StatementId<'mir>) -> bool,
        udg: &UseDefGraph<'mir, 'cfg>,
        dfg: &DataFlowGraph<'cfg>,
        dtree: &DominatorTree<'cfg>,
        start: BasicBlockId<'cfg>,
        end: Option<BasicBlockId<'cfg>>,
    ) -> bool {
        let mut relevant = false;
        let mut temporary_set = relevant_stmts.clone();
        let mut current = start;
        loop {
            match dtree[current] {
                DominatorTreeNode::Leaf(parent) => (),
                DominatorTreeNode::Root(branch) | DominatorTreeNode::Branch(_, branch) => {
                    let mut branch_relevant = false;
                    if let Some((start, end)) = branch.true_child() {
                        temporary_set.0.union_with(&relevant_stmts.0);
                        temporary_set.0.intersect_with(&dfg.out_sets[end]);
                        if temporary_set.0.as_slice().iter().any(|&chunk| chunk != 0) {
                            branch_relevant = self.eliminate_dead_code_internal(
                                relevant_stmts,
                                pred,
                                udg,
                                dfg,
                                dtree,
                                end,
                                Some(current),
                            );
                        }
                    }
                    if let Some((start, end)) = branch.false_child() {
                        temporary_set.0.union_with(&relevant_stmts.0);
                        temporary_set.0.intersect_with(&dfg.out_sets[end]);
                        if temporary_set.0.as_slice().iter().any(|&chunk| chunk != 0) {
                            branch_relevant |= self.eliminate_dead_code_internal(
                                relevant_stmts,
                                pred,
                                udg,
                                dfg,
                                dtree,
                                end,
                                Some(current),
                            );
                        }
                    }
                    if branch_relevant {
                        relevant = true;
                        relevant_stmts.0.union_with(&udg.terminator_uses[current].0)
                    }
                }
            }

            for stmt in self[current].statements.iter().rev().copied() {
                if relevant_stmts.contains(stmt) && pred(stmt) {
                    relevant_stmts
                        .0
                        .union_with(&udg.get_reachable_definitions(stmt).0);
                    relevant = true;
                }
            }

            current = match dtree[current].parent() {
                None => break,
                parent if parent == end => break,
                Some(parent) => parent,
            };
        }
        relevant
    }
}

impl<'tag> Mir<'tag> {
    pub fn track_expression(
        &self,
        expr: ExpressionId<'tag>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match expr {
            ExpressionId::Real(expr) => self.track_real_expression(expr, dependency_handler),
            ExpressionId::Integer(expr) => self.track_integer_expression(expr, dependency_handler),
            ExpressionId::String(expr) => self.track_string_expression(expr, dependency_handler),
        }
    }

    pub fn track_real_expression(
        &self,
        expr: RealExpressionId<'tag>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match self[expr].contents {
            RealExpression::Literal(_) => (),
            RealExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
            }
            RealExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }
            RealExpression::SystemFunctionCall(ident) => {
                dependency_handler.handle_system_function_call(ident)
            }

            RealExpression::BuiltInFunctionCall2p(_, arg1, arg2)
            | RealExpression::BinaryOperator(arg1, _, arg2) => {
                self.track_real_expression(arg1, dependency_handler);
                self.track_real_expression(arg2, dependency_handler);
            }

            RealExpression::Condition(cond, _, val1, _, val2) => {
                self.track_integer_expression(cond, dependency_handler);
                self.track_real_expression(val1, dependency_handler);
                self.track_real_expression(val2, dependency_handler);
            }

            RealExpression::BranchAccess(discipline, branch) => {
                dependency_handler.handle_branch_reference(discipline, branch)
            }

            RealExpression::Negate(_, expr) | RealExpression::BuiltInFunctionCall1p(_, expr) => {
                self.track_real_expression(expr, dependency_handler)
            }
            RealExpression::IntegerConversion(expr) => {
                self.track_integer_expression(expr, dependency_handler)
            }
            RealExpression::FunctionCall(_, _) => todo!("Function calls"),
        }
    }

    pub fn track_integer_expression(
        &self,
        expr: IntegerExpressionId<'tag>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match self[expr].contents {
            IntegerExpression::Literal(_) => (),

            IntegerExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
            }
            IntegerExpression::ParameterReference(param) => {
                dependency_handler.handle_parameter_reference(param)
            }

            IntegerExpression::NetReference(_) | IntegerExpression::PortReference(_) => {
                todo!("digital")
            }

            IntegerExpression::FunctionCall(_, _) => todo!("Function calls"),

            IntegerExpression::StringEq(arg1, arg2) | IntegerExpression::StringNEq(arg1, arg2) => {
                self.track_string_expression(arg1, dependency_handler);
                self.track_string_expression(arg2, dependency_handler);
            }

            IntegerExpression::RealComparison(arg1, _, arg2) => {
                self.track_real_expression(arg1, dependency_handler);
                self.track_real_expression(arg2, dependency_handler);
            }

            IntegerExpression::RealCast(expr) => {
                self.track_real_expression(expr, dependency_handler);
            }

            IntegerExpression::UnaryOperator(_, expr) | IntegerExpression::Abs(expr) => {
                self.track_integer_expression(expr, dependency_handler)
            }

            IntegerExpression::Max(arg1, arg2)
            | IntegerExpression::Min(arg1, arg2)
            | IntegerExpression::BinaryOperator(arg1, _, arg2)
            | IntegerExpression::IntegerComparison(arg1, _, arg2) => {
                self.track_integer_expression(arg1, dependency_handler);
                self.track_integer_expression(arg2, dependency_handler);
            }

            IntegerExpression::Condition(cond, _, val1, _, val2) => {
                self.track_integer_expression(cond, dependency_handler);
                self.track_integer_expression(val1, dependency_handler);
                self.track_integer_expression(val2, dependency_handler);
            }
        }
    }

    pub fn track_string_expression(
        &self,
        expr: StringExpressionId<'tag>,
        dependency_handler: &mut impl ExtractionDependencyHandler<'tag>,
    ) {
        match self[expr].contents {
            StringExpression::Literal(val) => (),
            StringExpression::VariableReference(var) => {
                dependency_handler.handle_variable_reference(var);
            }
            StringExpression::Condition(cond, _, val1, _, val2) => {
                self.track_integer_expression(cond, dependency_handler);
                self.track_string_expression(val1, dependency_handler);
                self.track_string_expression(val2, dependency_handler);
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
