use crate::Span;

pub type Error = crate::error::Error<Type>;
//pub(crate) type Warning = crate::error::Error<WarningType>;
pub type Result<T = ()> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Type {
    NotFound,
    NotAScope { declaration: Span },
}
