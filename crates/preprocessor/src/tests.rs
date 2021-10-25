use std::{cell::RefCell, str::from_utf8, sync::Arc};

use data_structures::iter::Itertools;
use vfs::{FileId, Vfs, VfsPath};

use crate::{
    preprocess, FileReadError, Preprocess, SourceProvider,
    TokenKind::{
        Comma, Comment, Div, Identifier, Modulus, Mul, ParenClose, ParenOpen, Plus, WhiteSpace,
    },
};

struct TestSourceProvider {
    vfs: RefCell<Vfs>,
    include_dirs: Arc<[VfsPath]>,
}

impl TestSourceProvider {
    pub fn new(mut include_dirs: Vec<VfsPath>) -> Self {
        let mut vfs = Vfs::default();
        vfs.insert_std_lib();
        include_dirs.push(VfsPath::new_virtual_path("/std".to_owned()));
        Self { vfs: RefCell::new(vfs), include_dirs: Arc::from(include_dirs) }
    }
}

impl SourceProvider for TestSourceProvider {
    fn include_dirs(&self, _root_file: FileId) -> Arc<[VfsPath]> {
        self.include_dirs.clone()
    }

    fn macro_flags(&self, _file_root: FileId) -> Arc<[std::sync::Arc<str>]> {
        Arc::new([])
    }

    fn file_text(&self, file: FileId) -> Result<Arc<str>, crate::FileReadError> {
        let vfs = self.vfs.borrow();
        let contents = vfs.file_contents(file).ok_or(FileReadError::NotFound)?;
        let contents = from_utf8(contents).map_err(|_| FileReadError::InvalidTextFormat)?;
        Ok(Arc::from(contents))
    }

    fn file_path(&self, file: FileId) -> VfsPath {
        self.vfs.borrow().file_path(file)
    }

    fn file_id(&self, path: VfsPath) -> FileId {
        self.vfs.borrow_mut().ensure_file_id(path)
    }
}

pub(super) const SRC: &str = r#"
`define test5(x,y) (x)+(y)
`ifdef test1 ERROR
`endif
`define test2
`ifdef test2 OK1
`endif,
    `ifdef test2 OK2,`define test3 OK3\
OK3L
    `endif
`ifdef test4 ERROR
`else
`define test7(x,y,z) \
/* foo */ \
x*(y%z)\
/* bar */
SMS__
`endif
`ifndef test4
`define test4 OK4
                                            `endif
`test3

,

`ifndef test4
ERROR
`else
`test4
`endif
`test5(Sum1,Sum2)
`define test6(x,y,z) `test5(`test7(x,y,z),f(x/y)*z)
`test6(a,b,c)
"#;

const EXPANDED_SRC: &str = r#"

OK1
,
    OK2,

SMS__



OK3\
OK3L

,


OK4

(Sum1)+(Sum2)
(\
/* foo */ \
a*(b%c)\
/* bar */)+(f(a/b)*c)
"#;

#[test]
pub fn macro_expansion() {
    let sources = TestSourceProvider::new(vec![]);
    let file = sources.vfs.borrow_mut().add_virt_file("/macro_expansion_test.va", SRC);

    let Preprocess { ts, diagnostics, sm } = preprocess(&sources, file);
    assert_eq!(diagnostics.as_slice(), &[]);
    assert_eq!(
        ts.iter().map(|t| t.kind).collect_vec(),
        [
            WhiteSpace, WhiteSpace, Identifier, WhiteSpace, Comma, WhiteSpace, WhiteSpace,
            Identifier, Comma, WhiteSpace, WhiteSpace, Identifier, WhiteSpace, WhiteSpace,
            WhiteSpace, WhiteSpace, Identifier, WhiteSpace, Identifier, WhiteSpace, WhiteSpace,
            Comma, WhiteSpace, WhiteSpace, WhiteSpace, Identifier, WhiteSpace, WhiteSpace,
            ParenOpen, Identifier, ParenClose, Plus, ParenOpen, Identifier, ParenClose, WhiteSpace,
            ParenOpen, WhiteSpace, Comment, WhiteSpace, WhiteSpace, Identifier, Mul, ParenOpen,
            Identifier, Modulus, Identifier, ParenClose, WhiteSpace, Comment, ParenClose, Plus,
            ParenOpen, Identifier, ParenOpen, Identifier, Div, Identifier, ParenClose, Mul,
            Identifier, ParenClose, WhiteSpace,
        ]
    );

    let full_expansion: String = ts.iter().map(|t| &SRC[t.span.to_file_span(&sm).range]).collect();

    assert_eq!(full_expansion, EXPANDED_SRC);
}
