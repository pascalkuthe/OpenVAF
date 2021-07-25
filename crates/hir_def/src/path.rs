use syntax::{
    ast::{self, PathSegmentKind},
    AstNode, SyntaxNodePtr,
};

use crate::{name::AsName, Name};

pub struct Path {
    pub is_root_path: bool,
    pub segments: Vec<Name>,
}

impl Path {
    pub fn resolve(syntax: ast::Path, mut err: impl FnMut(MalformedPathError)) -> Option<Path> {
        fn resolve_impl(
            syntax: &ast::Path,
            err: &mut impl FnMut(MalformedPathError),
        ) -> Option<Path> {
            let prefix = if let Some(qual) = syntax.qualifier() {
                Some(resolve_impl(&qual, err)?)
            } else {
                None
            };

            let segment = syntax.segment()?;

            match (prefix, segment.kind) {
                (Some(_), PathSegmentKind::Root) => {
                    err(MalformedPathError::IllegalRoot(SyntaxNodePtr::new_token(&segment.syntax)));
                    None
                }
                (None, PathSegmentKind::Root) => {
                    Some(Path { is_root_path: true, segments: vec![] })
                }
                (None, PathSegmentKind::Name) => {
                    Some(Path { is_root_path: false, segments: vec![segment.as_name()] })
                }

                (Some(mut prefix), PathSegmentKind::Name) => {
                    prefix.segments.push(segment.as_name());
                    Some(prefix)
                }
            }
        }

        let res = resolve_impl(&syntax, &mut err)?;
        if res.segments.len() != 0 {
            Some(res)
        } else {
            err(MalformedPathError::RootOnlyPath(SyntaxNodePtr::new(&syntax.syntax())));
            None
        }
    }
}

pub enum MalformedPathError {
    IllegalRoot(SyntaxNodePtr),
    RootOnlyPath(SyntaxNodePtr),
}
