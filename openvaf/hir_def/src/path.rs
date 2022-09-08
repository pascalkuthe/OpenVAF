use stdx::{impl_debug, pretty};
use syntax::ast::{self, PathSegmentKind};
use syntax::name::{AsIdent, AsName, Name};

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Path {
    pub is_root_path: bool,
    pub segments: Vec<Name>,
}

impl Path {
    pub fn new_ident(ident: Name) -> Path {
        Path { is_root_path: false, segments: vec![ident] }
    }

    pub fn resolve(syntax: ast::Path) -> Option<Path> {
        let prefix = if let Some(qual) = syntax.qualifier() { Path::resolve(qual) } else { None };

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

impl AsIdent for Path {
    fn as_ident(&self) -> Option<Name> {
        match self.segments.as_slice() {
            [name] if !self.is_root_path => Some(name.clone()),
            _ => None,
        }
    }
}

impl_debug!(match Path{
    Path{is_root_path: false, segments} => "{}", pretty::List::path(segments.as_slice());
    Path{is_root_path: true, segments} => "$root.{}", pretty::List::path(segments.as_slice());
});
