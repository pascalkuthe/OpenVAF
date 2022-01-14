use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

#[derive(Clone, PartialEq, Hash, Eq)]
pub struct List<C> {
    pub data: C,
    pub seperator: &'static str,
    pub final_seperator: &'static str,
    pub prefix: &'static str,
    pub postfix: &'static str,
}

impl<C> List<C> {
    pub fn new(contents: C) -> Self {
        Self { data: contents, seperator: ", ", final_seperator: " or ", prefix: "", postfix: "" }
    }

    pub fn with_seperator(contents: C, seperator: &'static str) -> Self {
        Self { data: contents, seperator, final_seperator: " or ", prefix: "", postfix: "" }
    }

    pub fn with_final_seperator(contents: C, final_seperator: &'static str) -> Self {
        Self { data: contents, seperator: ", ", final_seperator, prefix: "", postfix: "" }
    }

    pub fn path(contents: C) -> Self {
        Self { data: contents, seperator: ".", final_seperator: ".", prefix: "", postfix: "" }
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
                for x in body {
                    write!(f, "{}{}{}{}", self.prefix, x, self.postfix, self.seperator)?;
                }
                write!(
                    f,
                    "{}{}{}{}{}{}{}",
                    self.prefix,
                    second_last,
                    self.postfix,
                    self.final_seperator,
                    self.prefix,
                    last,
                    self.postfix
                )
            }
        }
    }
}
