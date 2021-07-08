/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Error, Path};

pub struct SingleItemEnum {
    ident: Ident,
    variants: Vec<Ident>,
}

impl SingleItemEnum {
    pub fn new(input: DeriveInput, trait_name: &'static str) -> syn::Result<Self> {
        if let Data::Enum(base) = input.data {
            for variant in base.variants.iter() {
                if variant.fields.len() != 1 {
                    return Err(Error::new(
                        variant.fields.span(),
                        format_args!(
                            "{} can only be derived for enums where all variants have a single field",
                            trait_name
                        ),
                    ));
                }
            }
            Ok(Self {
                variants: base.variants.iter().map(|var| var.ident.clone()).collect(),
                ident: input.ident,
            })
        } else {
            panic!("{} can only be derived for enums", trait_name)
        }
    }
}

pub fn generate_cfg_function(enum_def: SingleItemEnum, input: Path) -> TokenStream {
    let SingleItemEnum { ident, variants } = enum_def;
    quote! {
        impl CfgFunctionEnum for #ident{
            type I = #input;
        }

        impl CfgFunction for #ident{
            fn fconst_fold(&self, call: &[FlatSet<ConstVal>]) -> FlatSet<ConstVal>{
                match *self{
                    #(Self::#variants(ref __enum_inner) => CfgFunction::<Self>::fconst_fold(__enum_inner,call),)*
                }
            }

            fn fderivative<X: CfgFunctions>(
                &self,
                args: &IndexSlice<CallArg, [COperand<Self>]>,
                ad: &mut RValueAutoDiff<Self, X>,
                span: Span,
            ) -> Option<RValue<Self>>{
                 match *self{
                    #(Self::#variants(ref __enum_inner) => CfgFunction::<Self>::fderivative::<X>(__enum_inner,args,ad,span),)*
                }
            }
        }
    }
}
