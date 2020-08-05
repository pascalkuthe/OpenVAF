use crate::lints::{generate_lints, Lints};
use crate::symbols::{generate_symbols, Symbols};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result};

mod lints;
mod symbols;

/// A type used to greedily parse another type until the input is empty.
struct List<T>(Vec<T>);

impl<T: Parse> Parse for List<T> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut res = Vec::with_capacity(16);
        while !input.is_empty() {
            res.push(input.parse()?);
        }
        Ok(Self(res))
    }
}

#[proc_macro]
pub fn lints(body: TokenStream) -> TokenStream {
    let lints = parse_macro_input!(body as Lints);
    generate_lints(lints).into()
}

#[proc_macro]
pub fn symbols(body: TokenStream) -> TokenStream {
    let symbols = parse_macro_input!(body as Symbols);
    generate_symbols(symbols).into()
}
