/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::enum_derives::{generate_cfg_function, SingleItemEnum};
use crate::lints::{generate_lints, Lints};
use crate::symbols::{generate_symbols, Symbols};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Error, Meta, NestedMeta, Result};

mod enum_derives;
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

#[proc_macro_derive(CfgFunctions, attributes(cfg_inputs))]
pub fn cfg_functions(body: TokenStream) -> TokenStream {
    let input = parse_macro_input!(body as DeriveInput);
    let attr = input.attrs.iter().find(|x|x.path.is_ident("cfg_inputs")).expect("cfg_inputs attribute is missing! Use it to specify the CfgInput that belongs to these CfgFunctions");
    let (enum_decl, input_name) = match attr.parse_meta().and_then(|input_name| {
        let input_name = if let Meta::List(input_name) = input_name {
            if input_name.nested.len() == 1{
                if let NestedMeta::Meta(Meta::Path(name)) = input_name.nested.first().unwrap(){
                    Some(name.to_owned())
                }else {
                    None
                }
            }else{
                None
            }
        } else {
            None
        };
        if let Some(input_name) = input_name{
            Ok((SingleItemEnum::new(input, "CfgFunctions")?, input_name))
        }else{
            Err(Error::new(
                input_name.span(),
                "Invalid syntax. Expected the name of the input in the following format: #[cfg_input(DefaultInput)]",
            ))
        }

    }) {
        Ok(res) => res,
        Err(err) => return err.to_compile_error().into(),
    };
    generate_cfg_function(enum_decl, input_name).into()
}

// #[proc_macro_attribute]
// pub fn enum_dispatch(attr: TokenStream, item: TokenStream) -> TokenStream {
//     let enum_def = syn::ItemEnum::from(parse_macro_input!(item as EnumDispatchItem));
//
//     let mut expanded = proc_macro2::TokenStream::from(expanded);
//     // If the attributes are non-empty, the new block should be "linked" to the listed definitions.
//     // Those definitions may or may not have been cached yet.
//     // If one is not cached yet, the link will be pushed into the cache, and impl generation will
//     // be deferred until the missing definition is encountered.
//     // For now, we assume it is already cached.
//     if !attr.is_empty() {
//         let item = parse_macro_input!(attr as EnumDispatchArgList);
//
//         item
//             .arg_list
//             .into_iter()
//             .for_each(|p| {
//                 if p.leading_colon.is_some() || p.segments.len() != 1 {
//                     panic!("Paths in `#[enum_dispatch(...)]` are not supported.");
//                 }
//                 let syn::PathSegment {
//                     ident: attr_name,
//                     arguments: attr_generics
//                 } = p.segments.last().unwrap();
//                 let attr_generics = match attr_generics.clone() {
//                     syn::PathArguments::None => vec![],
//                     syn::PathArguments::AngleBracketed(args) => {
//                         assert!(args.colon2_token.is_none());
//                         args.args.iter().map(|generic_arg| {
//                             match generic_arg {
//                                 syn::GenericArgument::Type(syn::Type::Path(t)) if t.qself.is_none() => Some(t.path.get_ident().expect("Generic binding paths in #[enum_dispatch(...)] are not supported").clone()),
//                                 syn::GenericArgument::Type(syn::Type::Infer(_)) => None,
//                                 syn::GenericArgument::Type(_) => panic!("Generics in #[enum_dispatch(...)] must be identifiers"),
//                                 syn::GenericArgument::Lifetime(_) => panic!("Lifetime generics in #[enum_dispatch(...)] are not supported"),
//                                 syn::GenericArgument::Binding(_) => panic!("Generic equality constraints in #[enum_dispatch(...)] are not supported"),
//                                 syn::GenericArgument::Constraint(_) => panic!("Generic trait constraints in #[enum_dispatch(...)] are not supported"),
//                                 syn::GenericArgument::Const(_) => panic!("Const expression generics in #[enum_dispatch(...)] are not supported"),
//                             }
//                         }).collect::<Vec<_>>()
//                     }
//                     syn::PathArguments::Parenthesized(_) => panic!("Expected angle bracketed generic arguments, found parenthesized arguments"),
//                 };
//                 match &new_block {
//                     attributed_parser::ParsedItem::Trait(traitdef) => {
//                         cache::defer_link((attr_name, attr_generics.len()), (&traitdef.ident, traitdef.generics.type_params().count()))
//                     }
//                     attributed_parser::ParsedItem::EnumDispatch(enumdef) => {
//                         cache::defer_link((attr_name, attr_generics.len()), (&enumdef.ident, enumdef.generics.type_params().count()))
//                     }
//                 }
//             });
//     };
//     // It would be much simpler to just always retrieve all definitions from the cache. However,
//     // span information is not stored in the cache. Saving the newly retrieved definition prevents
//     // *all* of the span information from being lost.
//     match new_block {
//         attributed_parser::ParsedItem::Trait(traitdef) => {
//             let additional_enums =
//                 cache::fulfilled_by_trait(&traitdef.ident, traitdef.generics.type_params().count());
//             for enumdef in additional_enums {
//                 expanded.append_all(add_enum_impls(enumdef, traitdef.clone()));
//             }
//         }
//         attributed_parser::ParsedItem::EnumDispatch(enumdef) => {
//             let additional_traits =
//                 cache::fulfilled_by_enum(&enumdef.ident, enumdef.generics.type_params().count());
//             for traitdef in additional_traits {
//                 expanded.append_all(add_enum_impls(enumdef.clone(), traitdef));
//             }
//         }
//     }
//     expanded.into()
// }
