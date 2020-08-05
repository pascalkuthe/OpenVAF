//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of frontend, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use std::fmt::{Display, Formatter};

use thiserror::Error;

use openvaf_diagnostics::{AnnotationType, DiagnosticSlice, LibraryDiagnostic, Text};

use openvaf_session::sourcemap::Span;

pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UndefinedDerivative {
    Modulus,
    Noise,
    BitWiseOp,
    LogicOp,
    Comparison,
}

impl Display for UndefinedDerivative {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Modulus => f.write_str("modulus ('%')"),
            Self::Noise => f.write_str("noise filters"),
            Self::BitWiseOp => f.write_str("bit wise operations (such as |, &, ~)"),
            Self::LogicOp => f.write_str("logical operations (such as ||, &&, ~)"),
            Self::Comparison => f.write_str("comparison operations (such as ==, >, !=)"),
        }
    }
}

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("The derivative of {0} is not defined")]
    DerivativeNotDefined(UndefinedDerivative, Span),

    #[error("Symbolic derivatives of numeric (time) derivatives can not be calculated")]
    PartialDerivativeOfTimeDerivative(Span),

    #[error("Derivatives can only be calulated for numeric variables")]
    OnlyNumericExpressionsCanBeDerived(Span),
}

impl LibraryDiagnostic for Error {
    #[inline(always)]
    fn annotation_type(&self) -> Option<AnnotationType> {
        Some(AnnotationType::Error)
    }

    fn slices(&self) -> Vec<DiagnosticSlice> {
        match self {
            Self::DerivativeNotDefined(_, span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Derivative of this expression is not defined"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::PartialDerivativeOfTimeDerivative(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Can not calculate symbolic derivative"),
                    span.data(),
                )],
                fold: false,
            }],

            Self::OnlyNumericExpressionsCanBeDerived(span) => vec![DiagnosticSlice {
                slice_span: span.data(),
                messages: vec![(
                    AnnotationType::Error,
                    Text::const_str("Strings can not be differentiated"),
                    span.data(),
                )],
                fold: false,
            }],
        }
    }
}
