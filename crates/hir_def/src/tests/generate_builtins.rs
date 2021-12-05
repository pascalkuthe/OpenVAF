use indexmap::IndexSet;
use quote::{format_ident, quote};
use sourcegen::{add_preamble, ensure_file_contents, project_root, reformat, to_upper_snake_case};
use stdx::iter::multiunzip;

const BUILTINS: [&str; 135] = [
    "analysis",
    "acos",
    "acosh",
    "ac_stim",
    "asin",
    "asinh",
    "atan",
    "atan2",
    "atanh",
    "cos",
    "cosh",
    "ddt",
    "ddx",
    "exp",
    "flicker_noise",
    "floor",
    "flow",
    "potential",
    "hypot",
    "idt",
    "idtmod",
    "laplace_nd",
    "laplace_np",
    "laplace_zd",
    "laplace_zp",
    "limexp",
    "ln",
    "log",
    "max",
    "min",
    "noise_table",
    "noise_table_log",
    "pow",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
    "last_crossing",
    "slew",
    "white_noise",
    // "transition",
    "absdelay",
    "zi_nd",
    "zi_np",
    "zi_zd",
    "zi_zp",
    "$display",
    "$strobe",
    "$write",
    "$monitor",
    // "$monitoron",
    // "$monitoroff",
    "$debug",
    "$fclose",
    "$fopen",
    "$fdisplay",
    "$fwrite",
    "$fstrobe",
    "$fmonitor",
    "$fgets",
    "$fscanf",
    "$swrite",
    "$sformat",
    "$sscanf",
    "$rewind",
    "$fseek",
    "$ftell",
    "$fflush",
    "$ferror",
    "$feof",
    "$fdebug",
    "$finish",
    "$stop",
    "$fatal",
    "$warning",
    "$error",
    "$info",
    "$abstime",
    // "$bitstoreal",
    // "$realtobits",
    "$dist_chi_square",
    "$dist_exponential",
    "$dist_poisson",
    "$dist_uniform",
    "$dist_erlang",
    "$dist_normal",
    "$dist_t",
    "$random",
    "$arandom",
    "$rdist_chi_square",
    "$rdist_exponential",
    "$rdist_poisson",
    "$rdist_uniform",
    "$rdist_erlang",
    "$rdist_normal",
    "$rdist_t",
    "$clog2",
    "$ln",
    "$log10",
    "$exp",
    "$sqrt",
    "$pow",
    "$floor",
    "$ceil",
    "$sin",
    "$cos",
    "$tan",
    "$asin",
    "$acos",
    "$atan",
    "$atan2",
    "$hypot",
    "$sinh",
    "$cosh",
    "$tanh",
    "$asinh",
    "$acosh",
    "$atanh",
    "$temperature",
    "$vt",
    "$simparam",
    "$simprobe",
    "$discontinuity",
    "$limit",
    "$bound_step",
    "$mfactor",
    "$xposition",
    "$yposition",
    "$angle",
    "$hflip",
    "$vflip",
    "$param_given",
    "$port_connected",
    "$analog_node_alias",
    "$analog_port_alias",
    // "$table_model",
    "$test$plusargs",
    "$value$plusargs",
    "$simparam$str",
    "abs",
];

#[test]
fn generate_builtins() {
    let iter = BUILTINS.map(|builtin| {
        let is_sysfun = builtin.starts_with('$');

        let ident = if is_sysfun { builtin[1..].replace('$', "_") } else { builtin.to_owned() };
        let variant = ident.replace('$', "_");
        let ident = format_ident!("{}", ident);
        let prefix = if is_sysfun { format_ident!("sysfun") } else { format_ident!("kw") };
        (prefix, ident, variant)
    });

    let (kw_types, kws, variants): (Vec<_>, Vec<_>, Vec<_>) = multiunzip(iter);

    let unique_variants: IndexSet<_, ahash::RandomState> = variants.iter().cloned().collect();
    let constants =
        unique_variants.iter().map(|variant| format_ident!("{}", to_upper_snake_case(variant)));
    let unique_variants = unique_variants.iter().map(|variant| format_ident!("{}", variant));
    let indicies = (0..unique_variants.len()).map(|i| i as u8);

    let variants = variants.iter().map(|var| format_ident!("{}", var));

    let hir_def = quote! {
        #[derive(Eq,PartialEq,Copy,Clone, Hash,Debug)]
        #[allow(nonstandard_style,unreachable_pub)]
        #[repr(u8)]
        pub enum BuiltIn{
            #(#unique_variants = #indicies),*
        }

        pub fn insert_builtin_scope(dst: &mut AHashMap<Name, ScopeDefItem>){
            #(dst.insert(#kw_types::#kws,BuiltIn::#variants.into());)*
        }
    };

    let header = "use ahash::AHashMap;
        use syntax::name::{kw, sysfun, Name};

        use crate::nameres::ScopeDefItem;
    ";

    let hir_def = format!("{}\n{}", header, hir_def);

    let hir_def = add_preamble("generate_builtins", reformat(hir_def));
    let file = project_root().join("crates").join("hir_def").join("src").join("builtin.rs");
    ensure_file_contents(&file, &hir_def);

    let const_cnt = constants.len();
    let hir_ty = quote! {
        const BUILTIN_INFO: [BuiltinInfo; #const_cnt] = [#(#constants),*];

        pub(crate) fn bultin_info(builtin: BuiltIn) -> BuiltinInfo{
            BUILTIN_INFO[builtin as u8 as usize]
        }
    };

    let header = "use hir_def::BuiltIn;

        use crate::builtin::*;
    ";

    let hir_ty = format!("{}\n{}", header, hir_ty);
    let hir_ty = add_preamble("generate_builtins", reformat(hir_ty));

    let file = project_root()
        .join("crates")
        .join("hir_ty")
        .join("src")
        .join("builtin")
        .join("generated.rs");
    ensure_file_contents(&file, &hir_ty);
}
