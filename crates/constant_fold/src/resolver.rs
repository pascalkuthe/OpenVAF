//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

//! Abstraction over resolution of variable and parameter values during constant folding
use crate::lattice::DiamondLattice::NotAConstant;
use crate::DiamondLattice;
use crate::StringLiteral;
use openvaf_ir::ids::{ParameterId, VariableId};

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
