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

mod idents {
    syn::custom_keyword!(Keywords);
    syn::custom_keyword!(Symbols);
    syn::custom_keyword!(SystemFunctions);
}

pub struct Symbol {
    name: Ident,
    value: Option<LitStr>,
}

pub(crate) struct Symbols {
    keywords: Punctuated<Symbol, Token![,]>,
    symbols: Punctuated<Symbol, Token![,]>,
    system_functions: Punctuated<Symbol, Token![,]>,
}

impl Parse for Symbols {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        input.parse::<idents::Keywords>()?;
        let content;
        syn::braced!(content in input);
        let keywords = Punctuated::parse_terminated(&content)?;

        input.parse::<idents::Symbols>()?;
        let content;
        syn::braced!(content in input);
        let symbols = Punctuated::parse_terminated(&content)?;

        input.parse::<idents::SystemFunctions>()?;
        let content;
        syn::braced!(content in input);
        let system_functions = Punctuated::parse_terminated(&content)?;

        Ok(Self { keywords, symbols, system_functions })
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
    let mut keywords_stream = quote! {};
    let mut symbols_stream = quote! {};
    let mut systemfunctions_stream = quote! {};

    let mut prefill_stream = quote! {};
    let mut keys = HashSet::<String>::new();

    let mut check_dup = |str: &str| {
        if !keys.insert(str.to_string()) {
            panic!("Symbol `{}` is duplicated", str);
        }
    };
    if symbols.keywords.len() + symbols.symbols.len() + symbols.system_functions.len()
        > u32::MAX as usize
    {
        panic!(
            "At most {} symbols are allowed but {} were specified",
            u32::MAX,
            symbols.keywords.len() + symbols.symbols.len() + symbols.system_functions.len()
        );
    }

    #[allow(clippy::cast_possible_truncation)]
    let keyword_cnt = symbols.keywords.len() as u32;

    #[allow(clippy::cast_possible_truncation)]
    let sym_cnt = symbols.symbols.len() as u32;

    #[allow(clippy::cast_possible_truncation)]
    let sys_fun_cnt = symbols.system_functions.len() as u32;

    // Generate the listed symbols.
    for (counter, symbol) in symbols.keywords.iter().enumerate() {
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
        keywords_stream.extend(quote! {
            #[allow(non_upper_case_globals)]
            pub const #name: Symbol = Symbol::from_raw_unchecked(#counter);
        });
    }

    for (counter, symbol) in symbols.symbols.iter().enumerate() {
        #[allow(clippy::cast_possible_truncation)]
        let counter = counter as u32 + keyword_cnt;

        let name = &symbol.name;
        let value = match &symbol.value {
            Some(value) => value.value(),
            None => name.to_string(),
        };
        check_dup(&value);
        prefill_stream.extend(quote! {
            #value,
        });
        symbols_stream.extend(quote! {
            #[allow(non_upper_case_globals)]
            pub const #name: Symbol = Symbol::from_raw_unchecked(#counter);
        });
    }

    for (counter, symbol) in symbols.system_functions.iter().enumerate() {
        #[allow(clippy::cast_possible_truncation)]
        let counter = counter as u32 + keyword_cnt + sym_cnt;

        let name = &symbol.name;
        let value = match &symbol.value {
            Some(value) => format!("${}", value.value()),
            None => format!("${}", name.to_string()),
        };
        check_dup(&value);
        prefill_stream.extend(quote! {
            #value,
        });
        systemfunctions_stream.extend(quote! {
            #[allow(non_upper_case_globals)]
            pub const #name: Symbol = Symbol::from_raw_unchecked(#counter);
        });
    }

    let tt = quote! {
        mod generated_kws{
            #![allow(clippy::wildcard_imports)]
            use super::*;

            #keywords_stream
        }

        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        mod kw_generated {
            use super::Symbol;
            pub (super) const START: u32 = 0;
            pub (super) const END: u32 = #keyword_cnt;
            #keywords_stream
        }

        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        mod sym_generated {
            pub (super) const START: u32 = #keyword_cnt;
            pub (super) const END: u32 = #keyword_cnt + #sym_cnt;
            use super::Symbol;
            #symbols_stream
        }

        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        mod sysfun_generated {
            pub (super) const START: u32 = #keyword_cnt + #sym_cnt;
            pub (super) const END: u32 = #keyword_cnt  + #sym_cnt + #sys_fun_cnt;
            use super::Symbol;
            #systemfunctions_stream

        }

        impl Interner {
            pub fn fresh() -> Self {
                Interner::prefill(&[#prefill_stream])
            }
        }
    };

    // To see the generated code generated, uncomment this line, recompile, and
    // run the resulting output through `rustfmt`.
    //eprintln!("{}", tt);

    tt
}
