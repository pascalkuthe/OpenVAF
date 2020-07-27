use crate::sourcemap::span::SpanData;
use crate::sourcemap::{FileId, Location, SourceMap};
use crate::{HashMap, GLOBALS};
use index_vec::{define_index_type, IndexVec};

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

    /// Creates a string literal from the text locate at `span`
    #[must_use]
    pub fn create(span: &SpanData, sm: &SourceMap) -> Self {
        with_string_literal_interner(|interner| interner.intern(span, sm))
    }

    #[must_use]
    pub fn raw_contents(self, sm: &SourceMap) -> &str {
        with_string_literal_interner(|interner| interner.contents(self, sm))
    }

    /// Returns the contents of the `StringLiteral` with escape character such as "\\" replaced by the correspoding character such as "\"
    /// Expansive call considerately
    #[must_use]
    pub fn unescaped_contents(self, sm: &SourceMap) -> String {
        with_string_literal_interner(|interner| interner.unescaped_contents(self, sm))
    }
}

#[inline]
fn with_string_literal_interner<T, F: FnOnce(&mut StringLiteralInterner) -> T>(f: F) -> T {
    GLOBALS.with(|globals| f(&mut *globals.string_literal_interner.lock()))
}

#[derive(Clone, Debug)]
pub struct StringLiteralInterner {
    interned_literals: HashMap<Location, StringLiteral>,
    literals: IndexVec<StringLiteral, Location>,
}

impl StringLiteralInterner {
    #[allow(clippy::reversed_empty_ranges)]
    pub fn new() -> Self {
        Self {
            interned_literals: vec![(
                Location {
                    file: FileId::from_raw_unchecked(0),
                    range: 0..0,
                },
                StringLiteral::DUMMY,
            )]
            .into_iter()
            .collect(),
            literals: Default::default(),
        }
    }
    pub fn intern(&mut self, span: &SpanData, sm: &SourceMap) -> StringLiteral {
        if self.literals.len() >= StringLiteral::MAX_INDEX {
            panic!(
                "frontend currently supports at most {} literals",
                StringLiteral::MAX_INDEX
            )
        }
        let location = sm.lookup_span(&span);
        if let Some(&interned) = self.interned_literals.get(&location) {
            return interned;
        }
        let index = self.literals.push(location.clone());
        self.interned_literals.insert(location, index);
        index
    }

    pub fn location(&self, literal: StringLiteral) -> &Location {
        &self.literals[literal]
    }

    pub fn contents<'sm>(&self, literal: StringLiteral, sm: &'sm SourceMap) -> &'sm str {
        self.location(literal).src(sm)
    }

    /// Expansive call considerately
    pub fn unescaped_contents(&self, literal: StringLiteral, sm: &SourceMap) -> String {
        unesacpe_string(self.contents(literal, sm))
    }
}

impl Default for StringLiteralInterner {
    fn default() -> Self {
        Self::new()
    }
}

pub fn unesacpe_string(raw: &str) -> String {
    raw.replace(r"\n", "\n")
        .replace(r"\\", "\\")
        .replace(r"\t", "\t")
        .replace(r#"\""#, "\"")
}
