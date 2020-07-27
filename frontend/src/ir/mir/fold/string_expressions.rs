/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ir::ids::{IntegerExpressionId, ParameterId, StringExpressionId, VariableId};
use crate::ir::mir::StringExpression;
use crate::mir::Mir;
use crate::StringLiteral;

pub fn walk_string_expression<V: StringExprFold>(fold: &mut V, expr: StringExpressionId) -> V::T {
    let mir = fold.mir();
    match mir[expr].contents {
        StringExpression::Literal(val) => fold.fold_literal(val),
        StringExpression::VariableReference(var) => fold.fold_variable_reference(var),
        StringExpression::ParameterReference(param) => fold.fold_parameter_reference(param),
        StringExpression::Condition(cond, true_expr, false_expr) => {
            fold.fold_condition(cond, true_expr, false_expr)
        }
        StringExpression::SimParam(name) => fold.fold_string_expr(name),
    }
}

pub trait StringExprFold: Sized {
    type T;
    fn mir(&self) -> &Mir;

    #[inline]
    fn fold_string_expr(&mut self, expr: StringExpressionId) -> Self::T {
        walk_string_expression(self, expr)
    }

    fn fold_literal(&mut self, val: StringLiteral) -> Self::T;

    fn fold_condition(
        &mut self,
        cond: IntegerExpressionId,
        true_expr: StringExpressionId,
        false_expr: StringExpressionId,
    ) -> Self::T;

    fn fold_variable_reference(&mut self, var: VariableId) -> Self::T;

    fn fold_parameter_reference(&mut self, param: ParameterId) -> Self::T;

    fn fold_sim_parameter(&mut self, name: StringExpressionId) -> Self::T;
}
