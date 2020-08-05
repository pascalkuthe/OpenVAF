use crate::sourcemap::SourceMap;
use crate::with_sourcemap;
use openvaf_data_structures::index_vec::define_index_type;

define_index_type! {
            pub struct StringLiteral = u16;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<StringLiteral {}>";

            IMPL_RAW_CONVERSIONS = true;

            // Checks are done when literals are added
            DISABLE_MAX_INDEX_CHECK = ! cfg!(debug_assertions);
}

impl StringLiteral {
    pub const DUMMY: Self = Self::from_raw_unchecked(0);

    #[must_use]
    pub fn raw_contents(self, sm: &SourceMap) -> &str {
        sm.literals[self].src(sm)
    }

    #[must_use]
    pub fn with_raw_contents<T>(self, f: impl FnOnce(&str) -> T) -> T {
        with_sourcemap(|sm| f(self.raw_contents(sm)))
    }

    /// Returns the contents of the `StringLiteral` with escape character such as "\\" replaced by the correspoding character such as "\"
    /// Expansive call considerately
    #[must_use]
    pub fn unescaped_contents(self) -> String {
        with_sourcemap(|sm| unesacpe_string(self.raw_contents(sm)))
    }
}

pub fn unesacpe_string(raw: &str) -> String {
    raw.replace(r"\n", "\n")
        .replace(r"\\", "\\")
        .replace(r"\t", "\t")
        .replace(r#"\""#, "\"")
}
