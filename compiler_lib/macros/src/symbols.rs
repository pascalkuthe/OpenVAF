/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashSet;
use syn::parse::{Parse, ParseBuffer, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{Ident, LitStr, Token};

pub struct Symbol {
    name: Ident,
    value: Option<LitStr>,
}
pub(crate) struct Symbols(Punctuated<Symbol, Token![,]>);
impl Parse for Symbols {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        Ok(Self(input.parse_terminated(Symbol::parse)?))
    }
}

impl Parse for Symbol {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.parse()?;
        let value = match input.parse::<Token![:]>() {
            Ok(_) => Some(input.parse()?),
            Err(_) => None,
        };

        Ok(Symbol { name, value })
    }
}

#[allow(clippy::needless_pass_by_value)]
pub(crate) fn generate_symbols(symbols: Symbols) -> TokenStream {
    let mut items = quote! {};
    let mut prefill_stream = quote! {};
    let mut keys = HashSet::<String>::new();

    let mut check_dup = |str: &str| {
        if !keys.insert(str.to_string()) {
            panic!("Symbol `{}` is duplicated", str);
        }
    };
    if symbols.0.len() > u32::MAX as usize {
        panic!(
            "At most {} symbols are allowed but {} were specified",
            u32::MAX,
            symbols.0.len()
        );
    }
    // Generate the listed symbols.
    for (counter, symbol) in symbols.0.iter().enumerate() {
        #[allow(clippy::cast_possible_truncation)]
        let counter = counter as u32;

        let name = &symbol.name;
        let value = match &symbol.value {
            Some(value) => value.value(),
            None => name.to_string(),
        };
        check_dup(&value);
        prefill_stream.extend(quote! {
            #value,
        });
        items.extend(quote! {
            #[allow(non_upper_case_globals)]
            pub const #name: Symbol = Symbol::from_raw_unchecked(#counter);
        });
    }

    let tt = quote! {
        pub mod keywords{
            #![allow(clippy::wildcard_imports)]
            use super::*;

            #items
        }

        impl Interner {
            pub fn fresh() -> Self {
                Interner::prefill(&[
                    #prefill_stream
                ])
            }
        }
    };

    // To see the generated code generated, uncomment this line, recompile, and
    // run the resulting output through `rustfmt`.
    //eprintln!("{}", tt);

    tt
}
