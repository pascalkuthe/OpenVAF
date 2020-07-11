use open_vaf::diagnostic::UserResult;
use open_vaf::parser::tokenstream::TokenStream;
use open_vaf::preprocessor::{preprocess_user_facing, std_path};
use open_vaf::sourcemap::FileId;
use open_vaf::SourceMap;
use std::fmt::{Debug, Display, Formatter};
use std::path::PathBuf;
use std::sync::Arc;

pub const TEST_EXPANSION_HINT: &'static str =
    "This error occured inside a compiler directive. If you have trouble understanding why/where this error occurs use the 'ExpansionPrinter'";

pub struct PrettyError(Box<dyn Display>);

impl<I: Display + 'static> From<I> for PrettyError {
    fn from(val: I) -> Self {
        Self(Box::new(val))
    }
}

impl Debug for PrettyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub fn preprocess_test(
    mut path_to_tests: PathBuf,
    sm: SourceMap,
    main_file: FileId,
) -> UserResult<(TokenStream, Arc<SourceMap>)> {
    path_to_tests.push("std");
    let mut constants = path_to_tests.clone();
    constants.push("constants.vams");
    let mut disciplines = path_to_tests;
    disciplines.push("disciplines.vams");

    preprocess_user_facing(
        sm,
        TEST_EXPANSION_HINT,
        main_file,
        std_path(constants, disciplines),
    )
}
