/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::fold::integer_expressions::IntegerExprFold;
use crate::fold::real_expressions::RealExprFold;
use crate::fold::string_expressions::StringExprFold;
use crate::ExpressionId;

pub mod integer_expressions;
pub mod real_expressions;
pub mod string_expressions;

pub fn walk_expr<V: ExpressionFold>(fold: &mut V, expr: ExpressionId) -> <V as ExpressionFold>::T {
    match expr {
        ExpressionId::Real(expr) => fold.fold_real_expr(expr),
        ExpressionId::Integer(expr) => fold.fold_integer_expr(expr),
        ExpressionId::String(expr) => fold.fold_string_expr(expr),
    }
}

pub trait ExpressionFold:
    RealExprFold<T = <Self as ExpressionFold>::T>
    + IntegerExprFold<T = <Self as ExpressionFold>::T>
    + StringExprFold<T = <Self as ExpressionFold>::T>
{
    type T;
    fn fold_expr(&mut self, expr: ExpressionId) -> <Self as ExpressionFold>::T {
        walk_expr(self, expr)
    }
}
