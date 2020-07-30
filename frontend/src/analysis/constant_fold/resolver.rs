//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! Abstraction over resolution of variable and parameter values during constant folding
use crate::analysis::constant_fold::lattice::DiamondLattice::NotAConstant;
use crate::analysis::constant_fold::lattice::ProductLattice;
use crate::analysis::constant_fold::propagation::GlobalConstants;
use crate::analysis::constant_fold::DiamondLattice;
use crate::ir::ids::{ParameterId, VariableId};
use crate::StringLiteral;

/// This trait abstracts over the resolution of variables/parameter values during constant folding
/// Each method is reponsible to resolve values of Variables/Parameters of a certain type
/// Implimentations may assume that a method is only called for the right type.
/// So real_variable_value can assume that `var` refers to a variable of real type for example

pub trait ConstResolver {
    fn real_variable_value(&mut self, var: VariableId) -> DiamondLattice<f64>;
    fn int_variable_value(&mut self, var: VariableId) -> DiamondLattice<i64>;
    fn str_variable_value(&mut self, var: VariableId) -> DiamondLattice<StringLiteral>;
    fn real_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<f64>;
    fn int_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<i64>;
    fn str_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<StringLiteral>;
}

/// An implimentation of `ConstResolver` that can be used when no dependencies can be resolved
/// This is useful when folding constant statements or when other analysis already concluded that there are no dependencies
pub struct NoConstResolution;

#[allow(clippy::inline_always)]
impl ConstResolver for NoConstResolution {
    #[inline(always)]
    fn real_variable_value(&mut self, _var: VariableId) -> DiamondLattice<f64> {
        NotAConstant
    }

    #[inline(always)]
    fn int_variable_value(&mut self, _var: VariableId) -> DiamondLattice<i64> {
        NotAConstant
    }

    #[inline(always)]
    fn str_variable_value(&mut self, _var: VariableId) -> DiamondLattice<StringLiteral> {
        NotAConstant
    }

    #[inline(always)]
    fn real_parameter_value(&mut self, _param: ParameterId) -> DiamondLattice<f64> {
        NotAConstant
    }

    #[inline(always)]
    fn int_parameter_value(&mut self, _param: ParameterId) -> DiamondLattice<i64> {
        NotAConstant
    }

    #[inline(always)]
    fn str_parameter_value(&mut self, _param: ParameterId) -> DiamondLattice<StringLiteral> {
        NotAConstant
    }
}

pub struct LocalConstantPropagator<'lt>(pub &'lt ProductLattice, pub &'lt GlobalConstants);

impl<'lt> ConstResolver for LocalConstantPropagator<'lt> {
    fn real_variable_value(&mut self, var: VariableId) -> DiamondLattice<f64> {
        self.0[var].into()
    }

    fn int_variable_value(&mut self, var: VariableId) -> DiamondLattice<i64> {
        self.0[var].into()
    }

    fn str_variable_value(&mut self, var: VariableId) -> DiamondLattice<StringLiteral> {
        self.0[var].into()
    }

    fn real_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<f64> {
        self.1
            .real_parameters
            .get(&param)
            .copied()
            .map_or(DiamondLattice::NotAConstant, DiamondLattice::Val)
    }

    fn int_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<i64> {
        self.1
            .int_parameters
            .get(&param)
            .copied()
            .map_or(DiamondLattice::NotAConstant, DiamondLattice::Val)
    }

    fn str_parameter_value(&mut self, param: ParameterId) -> DiamondLattice<StringLiteral> {
        self.1
            .string_parameters
            .get(&param)
            .copied()
            .map_or(DiamondLattice::NotAConstant, DiamondLattice::Val)
    }
}
