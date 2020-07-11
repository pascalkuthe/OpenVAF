use crate::sourcemap::span::DUMMY_SP;
use crate::sourcemap::Span;
use crate::HashMap;
use crate::GLOBALS;
use core::option::Option::Some;
use index_vec::{define_index_type, IndexVec};

define_index_type! {
            pub struct SyntaxContext = u32;

            DISPLAY_FORMAT = "{}";

            DEBUG_FORMAT = "<SyntaxContext {}>";

            IMPL_RAW_CONVERSIONS = true;
}

impl SyntaxContext {
    pub const ROOT: Self = Self::from_raw_unchecked(0);
    // FOR USAGE IN SPANS
    pub const ROOT_U16: u16 = 0;

    #[must_use]
    pub fn create(call_span: Span) -> Self {
        with_syntax_interner(|interner| interner.intern(call_span))
    }

    /// # Returns
    /// * `None` for the root file
    /// * The [`Span`](crate::sourcemap::Span) of the compiler directive that created this `SyntaxContext`
    ///
    /// # Note
    /// This function reads the syntax context interner from thread local storage
    /// as such calling this from a different thread may yield a different result (or panic)

    #[inline(always)]
    #[must_use]
    pub fn call_site(self) -> Option<Span> {
        with_syntax_interner(|interner| interner.get(self))
    }

    /// Finds the smallest `SyntaxContext` that contains both `self` and `other`
    /// # Note
    /// This is a fairly efficient algorithm but still requires a building a HashMap
    /// so avoid calling this if not necessary.
    ///
    /// Furthremore this algorithm does not special case self==other
    /// This is a special case that tends to be of interest to the caller and and as such
    /// is checked there. Even if this is not of interest to you this is a fairly common case
    /// so you should special case it for better performance

    #[must_use]
    pub fn lowest_common_parent(self, other: Self) -> (SyntaxContext, Option<Span>, Option<Span>) {
        // self.index() is used as a capacity because it is the lowest upper bound we know
        let mut ancestors = HashMap::with_capacity(self.index());
        ancestors.insert(self, None);
        let mut current = self;
        with_syntax_interner(move |interner| {
            while let Some(call_site) = interner.get(current) {
                current = call_site.data().ctxt;
                ancestors.insert(current, Some(call_site));
            }

            current = other;
            let mut current_call_span = None;

            loop {
                if let Some(call_span) = ancestors.get(&current).copied() {
                    return (current, call_span, current_call_span);
                }

                let span = interner
                    .get(current)
                    .expect("CTXT paths dont intersect at root");
                current = span.data().ctxt;
                current_call_span = Some(span);
            }
        })
    }
}

pub struct SyntaxContextInterner {
    contexts: HashMap<Span, SyntaxContext>,
    contexts_data: IndexVec<SyntaxContext, Span>,
}

impl SyntaxContextInterner {
    pub fn new() -> Self {
        let mut contexts_data = IndexVec::with_capacity(512);
        // <SyntaxContext 0> is always the root span. As such
        contexts_data.push(DUMMY_SP);
        Self {
            contexts: HashMap::with_capacity(512),
            contexts_data,
        }
    }

    fn intern(&mut self, call_span: Span) -> SyntaxContext {
        if let Some(&index) = self.contexts.get(&call_span) {
            return index;
        }

        let index = self.contexts_data.push(call_span);
        self.contexts.insert(call_span, index);
        index
    }

    // Inline always because SyntaxContex::ROOT is often a special case anyway
    #[inline(always)]
    fn get(&self, index: SyntaxContext) -> Option<Span> {
        if index == SyntaxContext::ROOT {
            None
        } else {
            Some(self.contexts_data[index])
        }
    }
}

impl Default for SyntaxContextInterner {
    fn default() -> Self {
        Self::new()
    }
}

// If an interner exists, return it. Otherwise, prepare a fresh one.
#[inline(always)]
fn with_syntax_interner<T, F: FnOnce(&mut SyntaxContextInterner) -> T>(f: F) -> T {
    GLOBALS.with(|globals| f(&mut *globals.syntax_context_interner.lock()))
}
