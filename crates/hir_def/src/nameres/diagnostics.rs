use syntax::name::Name;

use super::{ResolvedPath, ScopeDefItem};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathResolveError {
    NotFound { name: Name },
    NotFoundIn { name: Name, scope: Name },
    ExpectedScope { name: Name, found: ScopeDefItem },
    ExpectedItemKind { name: Name, expected: &'static str, found: ResolvedPath },
    ExpectedNatureAttributeIdent { found: Box<[Name]> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DefDiagnostic {
    AlreadyDeclard { old: ScopeDefItem, new: ScopeDefItem, name: Name },
    ResolveError(PathResolveError),
}
