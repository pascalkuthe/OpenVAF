/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::error::Error::NotAllowedInContext;
use crate::Fold;
use derive_more::Display;
use hir::functions::{Function, HirFunction};
use hir::{
    AllowedOperation, AllowedOperations, Hir, ALLOWED_OPS_ANALOG_FUNCTION_BEHAVIOUR,
    ALLOWED_OPS_CONDITIONAL_ANALOG_BEHAVIOUR, ALLOWED_OPS_CONST_EXPRESSION,
    ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
};
use ir::ids::CallArg;
use session::sourcemap::Span;
use session::symbols::Symbol;
use std::mem;

#[derive(Copy, Clone, Eq, PartialEq, Display, Debug)]
pub enum AccessLocation {
    #[display(fmt = "constant expressions")]
    Constants,

    #[display(fmt = "analog functions")]
    AnalogFunction,
    #[display(fmt = "conditional analog behaviour")]
    ConditionalAnalogBehavior,
    #[display(fmt = "analog behaviour")]
    AnalogBehavior,

    #[display(fmt = "this argument of the function {}", "0")]
    FunctionArg(Symbol),
    #[display(fmt = "here")]
    Attribute,
}

#[derive(Copy, Clone, Debug)]
pub struct VerilogAState {
    allowed: AllowedOperations,
    location: AccessLocation,
}

impl VerilogAState {
    pub fn new_compiletime_const() -> Self {
        Self { allowed: AllowedOperations::empty(), location: AccessLocation::Constants }
    }

    pub fn new_runtime_const() -> Self {
        Self { allowed: ALLOWED_OPS_CONST_EXPRESSION, location: AccessLocation::Constants }
    }

    pub fn new_analog_block() -> Self {
        Self {
            allowed: ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
            location: AccessLocation::AnalogBehavior,
        }
    }

    pub fn new_attribute(allowed: AllowedOperations) -> Self {
        Self { allowed, location: AccessLocation::Attribute }
    }

    fn enter(&mut self, allowed: AllowedOperations, location: AccessLocation) -> Self {
        mem::replace(self, Self { allowed, location })
    }

    pub fn enter_function_decl(&mut self) -> Self {
        self.enter(ALLOWED_OPS_ANALOG_FUNCTION_BEHAVIOUR, AccessLocation::AnalogFunction)
    }

    pub fn enter_function_arg(&mut self, arg: CallArg, function: &Function, hir: &Hir) -> Self {
        if let Some(allowed) = function.allowed_arg_operation(arg) {
            self.enter(allowed, AccessLocation::FunctionArg(function.symbol(hir)))
        } else {
            *self
        }
    }

    pub fn enter_conditional_behaviour(&mut self) -> Self {
        let locations = if matches!(
            self.location,
            AccessLocation::AnalogFunction | AccessLocation::Constants | AccessLocation::Attribute
        ) {
            self.location
        } else {
            AccessLocation::ConditionalAnalogBehavior
        };

        let allowed = self.allowed & ALLOWED_OPS_CONDITIONAL_ANALOG_BEHAVIOUR;
        self.enter(allowed, locations)
    }

    pub fn test_allowed<F: Fn(Symbol) -> AllowedOperations>(
        &self,
        dst: &mut Fold<F>,
        op: AllowedOperation,
        span: Span,
    ) -> bool {
        if self.allowed.contains(op) {
            true
        } else {
            dst.error(NotAllowedInContext { op, span, loc: self.location, allowed: self.allowed });
            false
        }
    }
}
