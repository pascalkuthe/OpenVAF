use std::fs::{read_dir, read_to_string, DirEntry};
use std::mem::swap;

use ahash::RandomState;
use indexmap::IndexMap;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use sourcegen::{add_preamble, ensure_file_contents, project_root, reformat, to_lower_snake_case};
use stdx::SKIP_HOST_TESTS;

#[test]
fn gen_osdi_structs() {
    if SKIP_HOST_TESTS{
        return ;
    }

    let header_dir = project_root().join("crates").join("osdi").join("header");
    let headers: Vec<_> = read_dir(header_dir)
        .unwrap()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            Header::new(entry)
        })
        .collect();

    let src_dir = project_root().join("crates").join("osdi").join("src").join("metadata");

    for header in &headers {
        let res = HeaderParser { header, res: ParseResults::default(), off: 0 }.run();
        let tys = gen_tys(&res.tys);
        let consts = gen_defines(&res.defines);
        let file_header = "use mir_llvm::CodegenCx;\n";
        let version_str = format!("{}_{}", header.version_major, header.version_minor);
        let stdlib = format!("pub const STDLIB_BITCODE: &[u8] = include_bytes!(concat!(env!(\"OUT_DIR\"), \"/stdlib_{version_str}.bc\"));");
        let file_string = format!("{file_header}\n{stdlib}\n{consts}\n\n{tys}");
        let file_string = add_preamble("gen_osdi_structs", reformat(file_string));
        let file_name = format!("osdi_{}_{}.rs", header.version_major, header.version_minor);

        ensure_file_contents(&src_dir.join(file_name), &file_string);
    }
}

struct Header {
    version_minor: u32,
    version_major: u32,
    src: String,
    // path: PathBuf,
}

impl Header {
    fn new(entry: DirEntry) -> Option<Self> {
        if entry.file_type().map_or(true, |it| !it.is_file()) {
            return None;
        }
        let name = entry.file_name().to_str()?.to_owned();
        if &name[0..5] != "osdi_" {
            return None;
        }

        let name = &name[5..];
        let (version_major, name) = name.split_once('_').unwrap();
        let version_minor = name.split_once(".h").unwrap().0;

        let version_major = version_major.parse().unwrap();
        let version_minor = version_minor.parse().unwrap();

        let path = entry.path();
        let src = read_to_string(&path).unwrap();
        Some(Header { version_minor, version_major, src })
    }
}

#[derive(Default)]
struct ParseResults<'a> {
    tys: IndexMap<&'a str, OsdiStruct<'a>, RandomState>,
    defines: Vec<(&'a str, &'a str)>,
}

struct HeaderParser<'a> {
    header: &'a Header,
    off: usize,
    res: ParseResults<'a>,
}

impl<'a> HeaderParser<'a> {
    fn src(&self) -> &'a str {
        &self.header.src[self.off..]
    }

    fn trim(&mut self) {
        let src = self.src();
        let (off, _) =
            src.char_indices().find(|(_, c)| !c.is_whitespace()).unwrap_or((src.len(), '\0'));
        self.off += off;
    }

    fn eat(&mut self, kw: &str) -> bool {
        self.trim();
        if &self.src()[..kw.len()] == kw {
            self.off += kw.len();
            true
        } else {
            false
        }
    }

    fn eat_ident(&mut self) -> Option<&'a str> {
        self.trim();
        let src = self.src();
        let off = src.find(|c| !is_ident_char(c)).unwrap_or(src.len());
        if off != 0 {
            self.off += off;
            Some(&src[..off])
        } else {
            None
        }
    }

    fn run(mut self) -> ParseResults<'a> {
        loop {
            let typedef_pos = self.src().find("typedef");
            let define_pos = self.src().find("#define");
            if let Some(pos) = typedef_pos {
                if define_pos.map_or(true, |define_pos| pos < define_pos) {
                    self.off += pos;
                    assert!(self.eat("typedef"));
                    if self.eat("struct") {
                        self.parse_struct(false);
                    } else if self.eat("union") {
                        self.parse_struct(true);
                    }
                    continue;
                }
            }

            if let Some(pos) = define_pos {
                self.off += pos;
                assert!(self.eat("#define"));
                self.parse_define();
                continue;
            }

            if typedef_pos.is_none() {
                break;
            }
        }

        self.res
    }

    fn parse_define(&mut self) {
        let ident = self.eat_ident().unwrap();
        let end = self.src().find('\n').unwrap_or_else(|| self.src().len());
        let val = self.src()[..end].trim();
        self.res.defines.push((ident, val));
    }

    fn parse_ty(&mut self) -> Ty<'a> {
        let base_ty = match self.eat_ident().unwrap() {
            "double" => BaseTy::F64,
            "int" | "int32_t" => BaseTy::I32,
            "uint32_t" => BaseTy::U32,
            "size_t" => BaseTy::Usize,
            "char" => BaseTy::Str,
            "void" => BaseTy::Void,
            "bool" => BaseTy::Bool,
            name => BaseTy::Struct(name),
        };

        let mut indirection = 0;
        while self.eat("*") {
            indirection += 1;
        }
        Ty { indirection, base: base_ty, func_args: None }
    }

    fn parse_struct(&mut self, is_union: bool) {
        let ident = self.eat_ident().unwrap();
        assert!(self.eat("{"));

        let mut fields = Vec::new();
        loop {
            if self.eat("}") {
                break;
            }

            let mut ty = self.parse_ty();

            let is_func_ptr = self.eat("(") && self.eat("*");
            let field_ident = self.eat_ident().unwrap();
            if is_func_ptr {
                assert!(self.eat(")"));
                assert!(self.eat("("));
                let mut args = Vec::new();
                while !self.eat(")") {
                    args.push(self.parse_ty());
                    self.eat_ident();
                    self.eat(",");
                }
                ty.func_args = Some(args);
            }

            self.eat(";");
            fields.push((field_ident, ty));
        }

        self.res.tys.insert(
            ident,
            OsdiStruct { ident, llvm_ty_ident: to_lower_snake_case(ident), fields, is_union },
        );
    }
}

fn is_ident_char(c: char) -> bool {
    matches!(c, '_'| 'a'..='z' | 'A' ..='Z' | '0' ..='9')
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum BaseTy<'a> {
    F64,
    I32,
    U32,
    Usize,
    Str,
    Bool,
    Void,
    Struct(&'a str),
}

impl ToTokens for BaseTy<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = match self {
            BaseTy::F64 => "f64",
            BaseTy::I32 => "i32",
            BaseTy::U32 => "u32",
            BaseTy::Usize => "usize",
            BaseTy::Bool => "bool",
            BaseTy::Str => {
                quote!(String).to_tokens(tokens);
                return;
            }
            BaseTy::Void => "c_void",
            BaseTy::Struct(name) => name,
        };

        tokens.append(Ident::new(ident, Span::call_site()));
    }
}

struct Ty<'a> {
    base: BaseTy<'a>,
    indirection: u32,
    func_args: Option<Vec<Ty<'a>>>,
}

struct BaseTyInterpolater<'b, 'a> {
    indirection: u32,
    base: BaseTy<'a>,
    lut: &'b IndexMap<&'a str, OsdiStruct<'a>, RandomState>,
}

impl ToTokens for BaseTyInterpolater<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.indirection == 0 {
            self.base.to_tokens(tokens);
            if let BaseTy::Struct(ty) = self.base {
                let ty = &self.lut[ty];
                let has_ll = ty.fields.iter().any(|(_, ty)| ty.func_args.is_some());
                if has_ll {
                    quote!(<'ll>).to_tokens(tokens)
                }
            }
        } else {
            let next = BaseTyInterpolater { indirection: self.indirection - 1, ..*self };
            quote!(Vec<#next>).to_tokens(tokens)
        }
    }
}

struct TyInterpolater<'b, 'a> {
    ty: &'b Ty<'a>,
    lut: &'b IndexMap<&'a str, OsdiStruct<'a>, RandomState>,
}
impl ToTokens for TyInterpolater<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.ty.func_args.is_some() || self.ty.base == BaseTy::Void {
            quote!(&'ll llvm::Value).to_tokens(tokens);
            return;
        }

        let mut indirection = self.ty.indirection;
        if self.ty.base == BaseTy::Str {
            indirection -= 1;
        }

        BaseTyInterpolater { indirection, base: self.ty.base, lut: self.lut }.to_tokens(tokens);
    }
}

struct OsdiStruct<'a> {
    is_union: bool,
    ident: &'a str,
    llvm_ty_ident: String,
    fields: Vec<(&'a str, Ty<'a>)>,
}

struct OsdiStructInterp<'a, 'b> {
    info: &'b OsdiStruct<'a>,
    lut: &'b IndexMap<&'a str, OsdiStruct<'a>, RandomState>,
}

struct LLVMTyInterp<'a, 'b> {
    ty: &'b Ty<'a>,
    lut: &'b IndexMap<&'a str, OsdiStruct<'a>, RandomState>,
}

impl ToTokens for LLVMTyInterp<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut indirection = self.ty.indirection;
        let mut ty = match self.ty.base {
            BaseTy::F64 => quote!(ctx.ty_real()),
            BaseTy::I32 | BaseTy::U32 => quote!(ctx.ty_int()),
            BaseTy::Usize => quote!(ctx.ty_isize()),
            BaseTy::Str => {
                indirection -= 1;
                quote!(ctx.ty_str())
            }
            BaseTy::Bool => quote!(ctx.ty_c_bool()),
            BaseTy::Void if indirection == 0 => quote!(ctx.ty_void()),
            BaseTy::Void => {
                indirection -= 1;
                quote!(ctx.ty_void_ptr())
            }
            BaseTy::Struct(ty) => {
                let ty = &self.lut[ty].llvm_ty_ident;
                let ty = Ident::new(ty, Span::call_site());
                quote!(self.#ty.unwrap())
            }
        };
        for _ in 0..indirection {
            ty = quote!(ctx.ptr_ty(#ty));
        }

        if let Some(args) = &self.ty.func_args {
            let args = args.iter().map(|ty| LLVMTyInterp { ty, lut: self.lut });
            quote!(ctx.ptr_ty(ctx.ty_func(&[#(#args),*], #ty))).to_tokens(tokens)
        } else {
            ty.to_tokens(tokens)
        }
    }
}

struct LLVMValInterp<'a, 'b> {
    ty: &'b Ty<'a>,
    name: &'a str,
    pos: u32,
    lut: &'b IndexMap<&'a str, OsdiStruct<'a>, RandomState>,
}

impl ToTokens for LLVMValInterp<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = Ident::new(self.name, Span::call_site());
        let src = quote!(self.#ident);
        if self.ty.func_args.is_some() || self.ty.base == BaseTy::Void {
            src.to_tokens(tokens);
            return;
        }
        let mut indirection = self.ty.indirection;
        if self.ty.base == BaseTy::Str {
            indirection -= 1;
        }

        if indirection != 0 {
            let base_ty = match self.ty.base {
                BaseTy::F64 => quote!(ctx.ty_real()),
                BaseTy::I32 | BaseTy::U32 => quote!(ctx.ty_int()),
                BaseTy::Usize => quote!(ctx.ty_isize()),
                BaseTy::Str => {
                    quote!(ctx.ty_str())
                }
                BaseTy::Bool => quote!(ctx.ty_c_bool()),
                BaseTy::Void => unreachable!(),
                BaseTy::Struct(ty) => {
                    let ty = &self.lut[ty].llvm_ty_ident;
                    let ty = Ident::new(ty, Span::call_site());
                    quote!(tys.#ty)
                }
            };
            let pos = self.pos;
            let ident = format_ident!("arr_{pos}");
            quote!(ctx.const_arr_ptr(#base_ty, &#ident)).to_tokens(tokens);
            return;
        }

        let val = match self.ty.base {
            BaseTy::F64 => quote!(ctx.const_real(#src)),
            BaseTy::I32 => quote!(ctx.const_int(#src)),
            BaseTy::U32 => quote!(ctx.const_unsigned_int(#src)),
            BaseTy::Usize => quote!(ctx.const_usize(#src)),
            BaseTy::Str => {
                quote!(ctx.const_str_uninterned(&#src))
            }
            BaseTy::Bool => quote!(ctx.const_c_bool(#src)),
            BaseTy::Void => unreachable!(),
            BaseTy::Struct(_) => {
                quote!(#src.to_ll_val(ctx, tys))
            }
        };

        val.to_tokens(tokens)
    }
}

struct LLVMValPreInterp<'a, 'b> {
    ty: &'b Ty<'a>,
    name: &'a str,
    pos: u32,
}

impl ToTokens for LLVMValPreInterp<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = Ident::new(self.name, Span::call_site());
        let src = quote!(self.#ident);
        let mut indirection = self.ty.indirection;
        if self.ty.base == BaseTy::Str {
            indirection -= 1;
        }

        if indirection == 0 || self.ty.func_args.is_some() {
            return;
        };

        let calc_src = quote!(it);

        let val = match self.ty.base {
            BaseTy::F64 => quote!(ctx.const_real(*#calc_src)),
            BaseTy::I32 => quote!(ctx.const_int(*#calc_src)),
            BaseTy::U32 => quote!(ctx.const_unsigned_int(*#calc_src)),
            BaseTy::Usize => quote!(ctx.const_usize(*#calc_src)),
            BaseTy::Str => {
                quote!(ctx.const_str_uninterned(&*#calc_src))
            }
            BaseTy::Bool => quote!(ctx.const_c_bool(*#calc_src)),
            BaseTy::Void if indirection == 1 => {
                return;
            }
            BaseTy::Void => unreachable!(),
            BaseTy::Struct(_) => {
                quote!(#calc_src.to_ll_val(ctx, tys))
            }
        };

        assert!(indirection <= 1);
        let pos = self.pos;
        let ident = format_ident!("arr_{pos}");
        quote! {
            let #ident: Vec<_> = #src.iter().map(|it| #val).collect();
        }
        .to_tokens(tokens)
    }
}

impl ToTokens for OsdiStructInterp<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let llvm_ty_ident = Ident::new(&self.info.llvm_ty_ident, Span::call_site());

        let OsdiStruct { ident, fields, .. } = self.info;
        if !matches!(
            *ident,
            "OsdiInitError"
                | "OsdiSimParas"
                | "OsdiInitInfo"
                | "OsdiInitErrorPayload"
                | "OsdiSimInfo"
        ) {
            assert!(!self.info.is_union, "union code generation is not implemented (yet)");
            let ident = Ident::new(ident, Span::call_site());
            let field_names = fields.iter().map(|(name, _)| Ident::new(name, Span::call_site()));
            let field_tys = fields.iter().map(|(_, ty)| TyInterpolater { ty, lut: self.lut });
            let field_ll_arrays = fields
                .iter()
                .enumerate()
                .map(|(pos, (name, ty))| LLVMValPreInterp { ty, name, pos: pos as u32 });
            let field_ll_vals = fields.iter().enumerate().map(|(pos, (name, ty))| LLVMValInterp {
                ty,
                name,
                pos: pos as u32,
                lut: self.lut,
            });
            let has_ll =
                fields.iter().any(|(_, ty)| ty.func_args.is_some() || ty.base == BaseTy::Void);

            let mut lt = quote!();
            let mut func_lt = quote!(<'ll>);
            if has_ll {
                swap(&mut lt, &mut func_lt);
            }

            quote! {
                pub struct #ident #lt{
                    #(pub #field_names: #field_tys),*
                }

                impl #lt #ident #lt{
                    pub fn to_ll_val #func_lt (&self, ctx: &CodegenCx<'_,'ll>, tys: &'ll OsdiTys) -> &'ll llvm::Value{
                        #(#field_ll_arrays)*
                        let fields = [#(#field_ll_vals),*];
                        let ty = tys.#llvm_ty_ident;
                        ctx.const_struct(ty, &fields)
                    }
                }
            }
            .to_tokens(tokens);
        }

        let field_ll_tys =
            self.info.fields.iter().map(|(_, ty)| LLVMTyInterp { ty, lut: self.lut });
        let ident = self.info.ident;
        if self.info.is_union {
            let field_ll_tys2 = field_ll_tys.clone();
            quote! {
                impl OsdiTyBuilder<'_, '_, '_>{
                    fn #llvm_ty_ident(&mut self){
                        let ctx = &*self.ctx;
                        unsafe{
                            let align = [#(llvm::LLVMABIAlignmentOfType(self.target_data, #field_ll_tys)),*].into_iter().max().unwrap();
                            let mut size = [#(llvm::LLVMABISizeOfType(self.target_data, #field_ll_tys2)),*].into_iter().max().unwrap() as u32;
                            size = (size + align - 1) / align;
                            let elem = ctx.ty_aint(align*8);
                            let ty = ctx.ty_array(elem, size);
                            self.#llvm_ty_ident = Some(ty);
                        }
                    }
                }
            }
            .to_tokens(tokens);
        } else {
            quote! {
                impl OsdiTyBuilder<'_, '_, '_>{
                    fn #llvm_ty_ident(&mut self){
                        let ctx = &*self.ctx;
                        let fields = [#(#field_ll_tys),*];
                        let ty = ctx.struct_ty(#ident, &fields);
                        self.#llvm_ty_ident = Some(ty);
                    }
                }
            }
            .to_tokens(tokens);
        }
    }
}

fn gen_tys<'a>(tys: &IndexMap<&'a str, OsdiStruct<'a>, RandomState>) -> String {
    let fields = tys.values().map(|it| Ident::new(&it.llvm_ty_ident, Span::call_site()));
    let fields2 = fields.clone();
    let fields3 = fields.clone();
    let fields4 = fields.clone();
    let fields5 = fields.clone();
    let structs = tys.values().map(|it| OsdiStructInterp { info: it, lut: tys });
    quote!(
        #(#structs)*

        #[derive(Clone)]
        pub struct OsdiTys<'ll>{
            #(pub #fields : &'ll llvm::Type),*
        }

        impl<'ll> OsdiTys<'ll>{
            pub fn new(ctx: &CodegenCx<'_, 'll>, target_data: &llvm::TargetData) -> Self{
                let mut builder = OsdiTyBuilder{
                    ctx,
                    target_data,
                    #(#fields4: None),*
                };
                #(builder.#fields5();)*
                builder.finish()
            }

        }


        struct OsdiTyBuilder<'a, 'b, 'll>{
            ctx: &'a CodegenCx<'b, 'll>,
            target_data: &'a llvm::TargetData,
            #(#fields2 : Option<&'ll llvm::Type>),*
        }

        impl<'ll> OsdiTyBuilder<'_, '_, 'll>{
            fn finish(self) -> OsdiTys<'ll>{
                OsdiTys{
                    #(#fields3: self.#fields3.unwrap()),*
                }
            }
        }
    )
    .to_string()
}

fn gen_defines(defines: &[(&str, &str)]) -> String {
    defines.iter().map(|(ident, val)| format!("pub const {ident}: u32 = {val};")).collect()
}
