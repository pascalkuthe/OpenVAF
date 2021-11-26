use std::{cell::RefCell, path::PathBuf, str::from_utf8, sync::Arc};

use expect_test::expect_file;
use vfs::{FileId, Vfs, VfsPath};

use crate::{preprocess, FileReadError, Preprocess, SourceProvider};

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

fn check_prepocessor(sources: TestSourceProvider, root_file: FileId, test_name: &'static str) {
    let Preprocess { ts, diagnostics, sm } = preprocess(&sources, root_file);
    assert_eq!(diagnostics.as_slice(), &[]);
    let actual_tokens: String = ts.iter().map(|token| format!("{:?}\n", token.kind,)).collect();
    let expected = PathBuf::from(".").join("test_data").join(format!("{}.tokens", test_name));
    expect_file![expected].assert_eq(&actual_tokens);


    let vfs = sources.vfs.borrow();
    let actual_content: String = ts
        .iter()
        .map(|token| {
            let filespan = token.span.to_file_span(&sm);
            let src = from_utf8(vfs.file_contents(filespan.file).unwrap()).unwrap();
            &src[filespan.range]
        })
        .collect();

    let expected = PathBuf::from(".").join("test_data").join(format!("{}_expanded.va", test_name));
    expect_file![expected].assert_eq(&actual_content);
}

fn check_prepocessor_single_file(src: &str, test_name: &'static str) {
    let sources = TestSourceProvider::new(vec![]);
    let file = sources.vfs.borrow_mut().add_virt_file("/macro_expansion_test.va", src);
    check_prepocessor(sources, file, test_name)
}

#[test]
pub fn smoke_test() {
    const SRC: &str = r#"
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

    check_prepocessor_single_file(SRC, "smoke_test")
}

#[test]
fn whitespaces() {
    check_prepocessor_single_file(
        r#"
        `define FOO BAR
        // foo
        `define TEST `FOO\
        BAR
        "#,
        "whitespaces",
    )
}

#[test]
fn condition_enabled() {
    check_prepocessor_single_file(
        r#"
`ifdef DISABLE_STROBE
	`define STROBE(X)
	`define STROBE2(X,Y)
`else
	`define STROBE(X) $strobe(X)
	`define STROBE2(X,Y) $strobe(X,Y)
`endif

`STROBE(foo)
`STROBE2(bar,test)

        "#,
        "condition_enabled",
    )
}

#[test]
fn condition_disabled() {
    check_prepocessor_single_file(
        r#"
`define DISABLE_STROBE
`ifdef DISABLE_STROBE
	`define STROBE(X)
	`define STROBE2(X,Y)
`else
	`define STROBE(X) $strobe(X)
	`define STROBE2(X,Y) $strobe(X,Y)
`endif

`STROBE(foo)
`STROBE2(bar,test)

        "#,
        "condition_disacled",
    )
}

#[test]
fn source_map_triple_replacement(){
check_prepocessor_single_file(
        r#"
`include "constants.va"

`define y_fv(fv,y);

`define expLin(result, x);

// foo
"#,
        "source_map_triple_replacement",
    )
}
