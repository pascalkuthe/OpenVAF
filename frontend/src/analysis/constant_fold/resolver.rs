//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! Abstraction over resolution of variable and parameter values during constant folding
use crate::analysis::constant_fold::propagation::PropagatedConstants;
use crate::data_structures::{HybridBitSet, SparseBitSetMatrix};
use crate::ir::{ParameterId, StatementId, VariableId};
use crate::{HashMap, StringLiteral};
use log::debug;
use std::borrow::Borrow;

/// This trait abstracts over the resolution of variables/parameter values during constant folding
/// Each method is reponsible to resolve values of Variables/Parameters of a certain type
/// Implimentations may assume that a method is only called for the right type.
/// So real_variable_value can assume that `var` refers to a variable of real type for example
///
/// if a variable or parameter can't be resolved the method shall return `None`
///
pub trait ConstResolver {
    fn real_variable_value(&mut self, var: VariableId) -> Option<f64>;
    fn int_variable_value(&mut self, var: VariableId) -> Option<i64>;
    fn str_variable_value(&mut self, var: VariableId) -> Option<StringLiteral>;
    fn real_parameter_value(&mut self, param: ParameterId) -> Option<f64>;
    fn int_parameter_value(&mut self, param: ParameterId) -> Option<i64>;
    fn str_parameter_value(&mut self, param: ParameterId) -> Option<StringLiteral>;
}

/// An implimentation of `ConstResolver` that can be used when no dependencies can be resolved
/// This is useful when folding constant statements or when other analysis already concluded that there are no dependencies
pub struct NoConstResolution;

#[allow(clippy::inline_always)]
impl ConstResolver for NoConstResolution {
    #[inline(always)]
    fn real_variable_value(&mut self, _var: VariableId) -> Option<f64> {
        None
    }

    #[inline(always)]
    fn int_variable_value(&mut self, _var: VariableId) -> Option<i64> {
        None
    }

    #[inline(always)]
    fn str_variable_value(&mut self, _var: VariableId) -> Option<StringLiteral> {
        None
    }

    #[inline(always)]
    fn real_parameter_value(&mut self, _param: ParameterId) -> Option<f64> {
        None
    }

    #[inline(always)]
    fn int_parameter_value(&mut self, _param: ParameterId) -> Option<i64> {
        None
    }

    #[inline(always)]
    fn str_parameter_value(&mut self, _param: ParameterId) -> Option<StringLiteral> {
        None
    }
}

/// An implimentation of `ConstResolver` used during [constant propagation][const_propagation]
/// One instance is intended to be used for folding a single statement (or terminator condition)
///
/// The algorithm is based upon the result of [reaching definitions analysis][reaching_analysis].
///
/// When a reference to a variable is encountered all reaching assignments are checked.
/// (reaching assignments are the intersection of
/// `variables_assignments` to that variable and `reaching_definitions`)
/// If they have all been constant folded (present in `known_values`) to the same value
/// the reference can be folded to this value.
///
/// Furthermore the assigments to this variable are removed from `reaching_definitions`
/// (the expression does not longer depend on this variable it has been inlined)
/// so further analysis steps don't have to recompute the reaching definitions.
///
/// This requires a cache of variables that have already been folded as modifying `reaching_defintions`
/// as described above means that a second constant fold for the same variable would fail.
///
///
///
///
/// [reaching_analysis]: crate::analysis::data_flow::reaching_definitions
/// [const_propagation]: crate::analysis::constant_fold::propagation
#[derive(Debug)]
pub struct ConstantPropagator<'lt, K: Borrow<PropagatedConstants>> {
    pub known_values: K,

    ///Statements that can affect the statement this is being folded
    /// (by writing to a variable that is read here)
    /// Calculated using [reaching definitions analysis](crate::analysis::data_flow::reaching_definitions)
    reaching_definitions: &'lt mut HybridBitSet<StatementId>,

    /// Map from a Variable to all Statements that write to it
    /// See [reaching definitions analysis](crate::analysis::data_flow::reaching_definitions)
    variables_assignments: &'lt SparseBitSetMatrix<VariableId, StatementId>,

    /// real variables that have already been folded and don't need to be recomputed
    real_cache: HashMap<VariableId, f64>,

    /// integer variables that have already been folded and don't need to be recomputed
    int_cache: HashMap<VariableId, i64>,

    /// string variables that have already been folded and don't need to be recomputed
    str_cache: HashMap<VariableId, StringLiteral>,
}

impl<'lt, K: Borrow<PropagatedConstants>> ConstantPropagator<'lt, K> {
    pub fn new(
        known_values: K,
        dependencies: &'lt mut HybridBitSet<StatementId>,
        variables_assignments: &'lt SparseBitSetMatrix<VariableId, StatementId>,
    ) -> Self {
        Self {
            known_values,
            reaching_definitions: dependencies,
            variables_assignments,
            real_cache: HashMap::default(),
            int_cache: HashMap::default(),
            str_cache: HashMap::default(),
        }
    }
}

impl<'lt, K: Borrow<PropagatedConstants>> ConstResolver for ConstantPropagator<'lt, K> {
    #[inline]
    fn real_variable_value(&mut self, var: VariableId) -> Option<f64> {
        if let Some(&cached) = self.real_cache.get(&var) {
            return Some(cached);
        }

        let mut definitions = self
            .reaching_definitions
            .intersection(self.variables_assignments[var].as_ref()?)
            .map(|id| self.known_values.borrow().real_definitions.get(&id));

        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }

        if let Some(assignments) = &self.variables_assignments[var] {
            self.reaching_definitions.difference_with(assignments);
        }

        debug!("real var {} was resolved successfully", var);
        self.real_cache.insert(var, value);

        Some(value)
    }

    #[inline]
    fn int_variable_value(&mut self, var: VariableId) -> Option<i64> {
        if let Some(&cached) = self.int_cache.get(&var) {
            return Some(cached);
        }

        let mut definitions = self
            .reaching_definitions
            .intersection(self.variables_assignments[var].as_ref()?)
            .map(|id| self.known_values.borrow().integer_definitions.get(&id));

        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }

        self.int_cache.insert(var, value);

        if let Some(assignments) = &self.variables_assignments[var] {
            self.reaching_definitions.difference_with(assignments);
        }

        debug!("int var {} was resolved successfully", var);

        Some(value)
    }

    #[inline]
    fn str_variable_value(&mut self, var: VariableId) -> Option<StringLiteral> {
        if let Some(&cached) = self.str_cache.get(&var) {
            return Some(cached);
        }

        let mut definitions = self
            .reaching_definitions
            .intersection(self.variables_assignments[var].as_ref()?)
            .map(|id| self.known_values.borrow().string_definitions.get(&id));

        let value = *definitions.next().flatten()?;
        if definitions.any(|x| Some(&value) != x) {
            return None;
        }
        self.str_cache.insert(var, value);

        if let Some(assignments) = &self.variables_assignments[var] {
            self.reaching_definitions.difference_with(assignments);
        }

        debug!("str var {} was resolved successfully", var);

        Some(value)
    }

    #[inline]
    fn real_parameter_value(&mut self, param: ParameterId) -> Option<f64> {
        self.known_values
            .borrow()
            .real_parameters
            .get(&param)
            .copied()
    }

    #[inline]
    fn int_parameter_value(&mut self, param: ParameterId) -> Option<i64> {
        self.known_values
            .borrow()
            .int_parameters
            .get(&param)
            .copied()
    }

    #[inline]
    fn str_parameter_value(&mut self, param: ParameterId) -> Option<StringLiteral> {
        self.known_values
            .borrow()
            .string_parameters
            .get(&param)
            .copied()
    }
}
