use crate::List;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::export::Span;
use syn::parse::{Parse, ParseBuffer, Result};
use syn::spanned::Spanned;
use syn::token::Colon;
use syn::{
    Error, Expr, ExprLit, ExprStruct, FieldValue, Ident, Lit, LitStr, Member, Token, Visibility,
};

pub(crate) type Lints = List<Lint>;

pub struct Lint {
    pub visibility: Visibility,
    pub name: Ident,
    pub declaration: ExprStruct,
}

impl Parse for Lint {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        let visibility = input.parse()?;
        input.parse::<Token![const]>()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let mut declaration: ExprStruct = input.parse()?;
        input.parse::<Token![;]>()?;

        if !declaration.path.is_ident("LintData") {
            return Err(Error::new(declaration.path.span(), "Expected 'LintData'"));
        }

        for field in &declaration.fields {
            if let Member::Named(name) = &field.member {
                if name == "display_id" {
                    return Err(Error::new(
                        name.span(),
                        "`display_id` will be automatically defined by the lints! macro",
                    ));
                }
            }
        }
        declaration.fields.push(FieldValue {
            member: Member::Named(Ident::new("display_id", Span::call_site())),
            expr: Expr::Lit(ExprLit {
                attrs: Vec::default(),
                lit: Lit::Str(LitStr::new(&name.to_string(), name.span())),
            }),
            attrs: Vec::default(),
            colon_token: Some(Colon {
                spans: [Span::call_site()],
            }),
        });

        Ok(Self {
            visibility,
            name,
            declaration,
        })
    }
}

pub(crate) fn generate_lints(lints: Lints) -> TokenStream {
    let mut items = TokenStream::new();
    let mut name_lookup_init = TokenStream::new();
    let mut init = TokenStream::new();
    let count = lints.0.len();
    if count > u16::MAX as usize {
        panic!(
            "At most {} lints are allowed but {} builtin lints were declared",
            u16::MAX,
            count
        )
    }
    for (
        i,
        Lint {
            visibility,
            name,
            declaration,
        },
    ) in lints.0.into_iter().enumerate()
    {
        let data_name = format_ident!("{}_data", name);
        quote!(
            #[allow(non_upper_case_globals)]
            #visibility static #data_name: &LintData = &#declaration;
        )
        .to_tokens(&mut items);

        let i = i as u16;

        quote! (
                #[allow(non_upper_case_globals)]
                #visibility const #name: Lint = Lint::from_raw_unchecked(#i);
        )
        .to_tokens(&mut items);

        let name_str = name.to_string();

        quote! (
               names.insert(#name_str,builtin::#name);
        )
        .to_tokens(&mut name_lookup_init);

        quote! (
               builtin::#data_name,
        )
        .to_tokens(&mut init);
    }

    let count = Literal::usize_suffixed(count as usize);

    let res = quote! (
        pub mod builtin{
            use super::*;
            use super::LintLevel::*;
            #items
        }

        #[allow(clippy::default_trait_access)]
        impl LintRegistry{
            pub fn new()->Self{
                let mut names = HashMap::with_capacity(#count);
                #name_lookup_init
                Self{
                    names,
                    lints: index_vec![#init],
                    .. LintRegistry::default()
                }
            }
        }

    );
    // To see the generated code generated, uncomment this line, recompile, and
    // run the resulting output through `rustfmt`.
    //eprintln!("{}", res);
    res
}
