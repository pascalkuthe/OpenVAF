/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
*/

use crate::derivatives::error::Error;
use crate::diagnostic::MultiDiagnostic;
use crate::ir::ids::{BranchId, NetId, ParameterId, VariableId};
use crate::ir::mir::{Variable, VariableType};
use crate::ir::{Attributes, Node};
use crate::mir::Mir;
use crate::symbol::Ident;
use crate::HashMap;
use std::mem::take;

pub mod demanded;
pub mod error;
pub mod expression_derivatives;
pub mod lints;

pub(super) type PartialDerivativeMap = HashMap<Unknown, VariableId>;

pub(super) type DerivativeMap = HashMap<VariableId, PartialDerivativeMap>;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Unknown {
    Parameter(ParameterId),
    NodePotential(NetId),
    Flow(BranchId),
    Temperature,
    Time,
}

pub struct AutoDiff<'lt> {
    pub errors: MultiDiagnostic<Error>,
    pub mir: &'lt mut Mir,
}

impl<'lt> AutoDiff<'lt> {
    pub fn new(mir: &'lt mut Mir) -> Self {
        Self {
            mir,
            errors: MultiDiagnostic(Vec::with_capacity(8)),
        }
    }
}

impl Mir {
    pub fn derivatives(&self) -> &DerivativeMap {
        &self.derivatives
    }

    pub fn derivative_origins(&self) -> &HashMap<VariableId, VariableId> {
        &self.derivative_origins
    }

    pub fn derivative_var(&mut self, var: VariableId, derive_by: Unknown) -> VariableId {
        // we appease the borrow checker
        let mut derivatives = take(&mut self.derivatives);

        let res = *derivatives
            .entry(var)
            .or_insert_with(|| HashMap::with_capacity(2))
            .entry(derive_by)
            .or_insert_with(|| self.declare_partial_derivative_variable(var, derive_by));

        self.derivatives = derivatives;
        res
    }

    pub fn declare_partial_derivative_variable(
        &mut self,
        variable: VariableId,
        derive_by: Unknown,
    ) -> VariableId {
        let derive_by = match derive_by {
            Unknown::Parameter(parameter) => self[parameter].contents.ident.to_string(),
            Unknown::NodePotential(net) => format!("potential({})", self[net].contents.ident),
            Unknown::Flow(branch) => format!("flow({})", self[branch].contents.ident),
            Unknown::Temperature => "Temp".to_string(),
            Unknown::Time => "t".to_string(),
        };

        let name = Ident::from_str(
            format!(
                "\u{2202}{}/\u{2202}{}",
                self[variable].contents.ident, derive_by
            )
            .as_str(),
        );

        let res = self.variables.push(Node {
            attributes: Attributes::EMPTY,
            span: self[variable].span,
            contents: Variable {
                ident: name,
                variable_type: VariableType::Real(None),
            },
        });

        debug_assert!(&self[self[res].attributes.as_range()].is_empty());

        let origin = if let Some(&origin) = self.derivative_origins.get(&variable) {
            origin
        } else {
            variable
        };

        self.derivative_origins.insert(res, origin);

        res
    }
}
