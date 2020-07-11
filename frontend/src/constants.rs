//! This module manages the physical constants that VARF uses to compile certain functions (such as `$vt`)
use crate::diagnostic::{AnnotationType, DiagnosticSlice, FooterItem, Text};
use crate::lints::{builtin, Lint, LintDiagnostic, Linter};
use crate::{Span, GLOBALS};
use std::error::Error;
use std::fmt::{Display, Formatter};

/// Represents a physical constant whose default value was used
/// This enum is used for the [`standard_nature_constants`](StandardNatureConstant) lint
#[derive(Debug, Copy, Clone)]
pub enum Constant {
    Q,
    Kb,
}

impl Constant {
    pub fn default_value(&self) -> f64 {
        match self {
            Self::Q => defaults::Q,
            Self::Kb => defaults::KB,
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Q => f.write_str("electron charge (q)"),
            Self::Kb => f.write_str("boltzmann constant (kb)"),
        }
    }
}

/// OpenVAF uses NIST2010 physical constants by default
pub mod defaults {
    pub const KB: f64 = 1.3806488e-23;
    pub const Q: f64 = 1.602176565e-19;
}

/// The set of physical constants that OpenVAF uses
/// if a constant is set to `None` the [default](defaults) constants are used
///
/// The pysical constants are stored as a thread local in `GLOBALS`
#[derive(Copy, Clone, Debug, Default)]
pub struct Constants {
    /// The Boltzmann constant
    pub kb: Option<f64>,

    /// The electron charge
    pub q: Option<f64>,
    /// A hint that hints to users to set the constant
    pub disclaimer: Option<&'static str>,
}

impl Constants {
    /// Sets the constants to be used by OpenVAF
    /// This function can only be called **once**
    /// # Returns
    /// * Err(constants) if the value was already set
    /// * Ok(()) if the constants were sucessfuly set
    pub fn set(constants: Self) -> Result<(), Self> {
        GLOBALS.with(|globals| globals.constants.set(constants))
    }

    fn with<T>(f: impl FnOnce(&Constants) -> T) -> T {
        GLOBALS.with(|globals| f(globals.constants.get_or_init(Default::default)))
    }

    #[inline]
    pub fn kb(span: Span) -> f64 {
        Self::with(|constants| match constants.kb {
            Some(val) => val,
            None => constants.default(span, Constant::Kb),
        })
    }

    #[inline]
    pub fn q(span: Span) -> f64 {
        Self::with(|constants| match constants.q {
            Some(val) => val,
            None => constants.default(span, Constant::Q),
        })
    }

    /// Dispatches a lint that the default value is being used and returns
    /// the [default](defaults) value for `constant`
    fn default(&self, span: Span, constant: Constant) -> f64 {
        Linter::dispatch_early(Box::new(StandardNatureConstant {
            span,
            disclaimer: self.disclaimer,
            constant,
        }));
        constant.default_value()
    }
}

/// A lint that informs the user that [default](defaults) physical constant is being used
#[derive(Clone, Debug)]
pub struct StandardNatureConstant {
    pub span: Span,
    pub disclaimer: Option<&'static str>,
    pub constant: Constant,
}

impl Error for StandardNatureConstant {}

impl Display for StandardNatureConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Default physical constants (NIST2010) were used for {}",
            self.constant
        ))
    }
}

impl LintDiagnostic for StandardNatureConstant {
    fn lint(&self) -> Lint {
        builtin::standard_nature_constants
    }

    fn slices(&self, main_annotation_type: AnnotationType) -> Vec<DiagnosticSlice> {
        vec![DiagnosticSlice {
            slice_span: self.span.data(),
            messages: vec![(
                main_annotation_type,
                Text::owned(format!("Default {} was used", self.constant)),
                self.span.data(),
            )],
            fold: false,
        }]
    }

    fn footer(&self) -> Vec<FooterItem> {
        if let Some(disclaimer) = self.disclaimer {
            vec![FooterItem {
                id: None,
                label: Text::const_str(disclaimer),
                annotation_type: AnnotationType::Info,
            }]
        } else {
            Vec::new()
        }
    }
}
