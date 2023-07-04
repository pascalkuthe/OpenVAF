use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

#[derive(Clone, PartialEq, Hash, Eq)]
pub struct List<C> {
    pub data: C,
    pub separator: &'static str,
    pub final_separator: &'static str,
    pub prefix: &'static str,
    pub postfix: &'static str,
    pub break_after: u32,
    pub first_break_after: u32,
}

impl<C> List<C> {
    pub fn new(contents: C) -> Self {
        Self {
            data: contents,
            separator: ", ",
            final_separator: " or ",
            prefix: "",
            postfix: "",
            break_after: 10,
            first_break_after: 5,
        }
    }

    pub fn surround(mut self, prefix: &'static str) -> Self {
        self.prefix = prefix;
        self.postfix = prefix;
        self
    }

    pub fn with_separator(mut self, separator: &'static str) -> Self {
        self.separator = separator;
        self
    }

    pub fn with_break_after(mut self, break_after: u32) -> Self {
        self.break_after = break_after;
        self
    }

    pub fn with_first_break_after(mut self, break_after: u32) -> Self {
        self.first_break_after = break_after;
        self
    }

    pub fn with_final_separator(mut self, final_separator: &'static str) -> Self {
        self.final_separator = final_separator;
        self
    }

    pub fn path(contents: C) -> Self {
        Self::new(contents).with_separator(".").with_final_separator(".")
    }
}

impl<C: Debug> Debug for List<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data, f)
    }
}

impl<X: Display, T: Deref<Target = [X]>> Display for List<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.data.deref() {
            [] => f.write_str(" "),
            [x] => write!(f, "{}{}{}", self.prefix, x, self.postfix),
            [ref body @ .., second_last, last] => {
                let mut i = 0;
                let mut break_after = self.first_break_after;
                for x in body {
                    if i == break_after {
                        i = 1;
                        break_after = self.break_after;
                        writeln!(f)?;
                    } else {
                        i += 1;
                    }
                    write!(f, "{}{}{}{}", self.prefix, x, self.postfix, self.separator)?;
                }
                write!(
                    f,
                    "{}{}{}{}{}{}{}",
                    self.prefix,
                    second_last,
                    self.postfix,
                    self.final_separator,
                    self.prefix,
                    last,
                    self.postfix
                )
            }
        }
    }
}
