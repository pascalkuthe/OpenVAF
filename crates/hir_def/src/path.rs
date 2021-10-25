use syntax::ast::{self, PathSegmentKind};

use crate::{name::AsName, Name};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Path {
    pub is_root_path: bool,
    pub segments: Vec<Name>,
}

impl Path {
    pub fn new_ident(ident: Name) -> Path {
        Path { is_root_path: false, segments: vec![ident] }
    }

    pub fn resolve(syntax: ast::Path) -> Option<Path> {
        let prefix =
            if let Some(qual) = syntax.qualifier() { Some(Path::resolve(qual)?) } else { None };

        let segment = syntax.segment()?;

        match (prefix, segment.kind) {
            (Some(_), PathSegmentKind::Root) => None,
            (None, PathSegmentKind::Root) => Some(Path { is_root_path: true, segments: vec![] }),
            (None, PathSegmentKind::Name) => {
                Some(Path { is_root_path: false, segments: vec![segment.as_name()] })
            }

            (Some(mut prefix), PathSegmentKind::Name) => {
                prefix.segments.push(segment.as_name());
                Some(prefix)
            }
        }
    }
}
