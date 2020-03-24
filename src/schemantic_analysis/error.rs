use crate::ir::{ParameterId, VariableId};

pub type Error<'hir> = crate::error::Error<Type<'hir>>;
//pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<'hir, T = ()> = std::result::Result<T, Error<'hir>>;

#[derive(Clone, Debug)]
pub enum Type<'hir> {
    ExpectedReal,
    ExpectedInteger,
    ExpectedIntegerParameter(ParameterId<'hir>),
    ExpectedIntegerVariable(VariableId<'hir>),
    ExpectedIntegerOperands,
    ExpectedNumericParameter(ParameterId<'hir>),
    ParameterDefinedAfterConstantReference(ParameterId<'hir>),
    NotABoolean(i64),
}
