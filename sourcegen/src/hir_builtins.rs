use crate::{add_preamble, ensure_file_contents, project_root, reformat, to_upper_snake_case};
use indexmap::IndexSet;
use quote::{format_ident, quote};
use stdx::iter::multiunzip;
use stdx::SKIP_HOST_TESTS;

const ANALOG_OPERATORS: [&str; 17] = [
    "absdelay",
    "ddt",
    "idt",
    "idtmod",
    "ddx",
    "zi_nd",
    "zi_np",
    "zi_zd",
    "zi_zp",
    "laplace_nd",
    "laplace_np",
    "laplace_zd",
    "laplace_zp",
    "limexp",
    "last_crossing",
    "slew",
    "transition",
];

const UNSUPPORTED: [&str; 50] = [
    "simprobe",
    "analog_node_alias",
    "analog_port_alias",
    "test_plusargs",
    "value_plusargs",
    "zi_nd",
    "zi_np",
    "zi_zd",
    "zi_zp",
    "laplace_nd",
    "laplace_np",
    "laplace_zd",
    "laplace_zp",
    "last_crossing",
    "slew",
    "transition",
    "fclose",
    "fopen",
    "fdisplay",
    "fwrite",
    "fstrobe",
    "fmonitor",
    "fgets",
    "fscanf",
    "swrite",
    "sformat",
    "sscanf",
    "rewind",
    "fseek",
    "ftell",
    "fflush",
    "ferror",
    "feof",
    "fdebug",
    "dist_chi_square",
    "dist_exponential",
    "dist_poisson",
    "dist_uniform",
    "dist_erlang",
    "dist_normal",
    "dist_t",
    "random",
    "arandom",
    "rdist_chi_square",
    "rdist_exponential",
    "rdist_poisson",
    "rdist_uniform",
    "rdist_erlang",
    "rdist_normal",
    "rdist_t",
];

const ANALOG_OPERATORS_SYSFUN: [&str; 1] = ["$limit"];

const ANALYSIS_FUNS: [&str; 6] =
    ["analysis", "ac_stim", "noise_table", "noise_table_log", "white_noise", "flicker_noise"];

const BUILTINS: [&str; 25] = [
    "abs",
    "acos",
    "acosh",
    "asin",
    "asinh",
    "atan",
    "atan2",
    "atanh",
    "cos",
    "cosh",
    "exp",
    "floor",
    "flow",
    "potential",
    "hypot",
    "ln",
    "log",
    "max",
    "min",
    "pow",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
];

const PARAM_SYSFUNS: [&str; 6] = ["mfactor", "xposition", "yposition", "angle", "hflip", "vflip"];

const SYSFUNS: [&str; 81] = [
    "$display",
    "$strobe",
    "$write",
    "$monitor",
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
    "$simparam$str",
    "$simprobe",
    "$discontinuity",
    "$param_given",
    "$port_connected",
    "$analog_node_alias",
    "$analog_port_alias",
    // "$table_model",
    "$test$plusargs",
    "$value$plusargs",
    "$bound_step",
];

#[test]
fn generate_builtins() {
    if SKIP_HOST_TESTS {
        return;
    }
    let iter = BUILTINS
        .into_iter()
        .chain(SYSFUNS)
        .chain(ANALYSIS_FUNS)
        .chain(ANALOG_OPERATORS_SYSFUN)
        .chain(ANALOG_OPERATORS)
        .map(|builtin| {
            let is_sysfun = builtin.starts_with('$');

            let variant =
                if is_sysfun { builtin[1..].replace('$', "_") } else { builtin.to_owned() };
            let ident = format_ident!("{}", variant);
            let prefix = if is_sysfun { format_ident!("sysfun") } else { format_ident!("kw") };
            (prefix, ident, variant)
        });

    let (kw_types, kws, variants): (Vec<_>, Vec<_>, Vec<_>) = multiunzip(iter);

    let unique_variants: IndexSet<_, ahash::RandomState> = variants.iter().cloned().collect();
    let constants =
        unique_variants.iter().map(|variant| format_ident!("{}", to_upper_snake_case(variant)));
    let unique_variants = unique_variants.iter().map(|variant| format_ident!("{}", variant));
    let indicies = (0..unique_variants.len()).map(|i| i as u8);

    let analysis_funs = ANALYSIS_FUNS.into_iter().map(|op| format_ident!("{}", op));
    let analog_operators = ANALOG_OPERATORS.into_iter().map(|op| format_ident!("{}", op));
    let unsupported = UNSUPPORTED.into_iter().map(|op| format_ident!("{}", op));
    let analog_operators_sysfun =
        ANALOG_OPERATORS_SYSFUN.into_iter().map(|op| format_ident!("{}", &op[1..]));

    let variants = variants.iter().map(|var| format_ident!("{}", var));
    let params = PARAM_SYSFUNS.map(|var| format_ident!("{}", var));

    let hir_def = quote! {
        #[derive(Eq,PartialEq,Copy,Clone, Hash,Debug)]
        #[allow(nonstandard_style,unreachable_pub)]
        #[repr(u8)]
        pub enum BuiltIn{
            #(#unique_variants = #indicies),*
        }


        #[derive(Eq,PartialEq,Copy,Clone, Hash,Debug)]
        #[allow(nonstandard_style,unreachable_pub)]
        pub enum ParamSysFun{
            #(#params),*
        }

        impl BuiltIn{
            #[allow(clippy::match_like_matches_macro)]
            pub fn is_analog_operator(self)->bool{
                match self{
                    #(BuiltIn::#analog_operators)|* =>true,
                    _ => false
                }
            }

            #[allow(clippy::match_like_matches_macro)]
            pub fn is_analog_operator_sysfun(self)->bool{
                match self{
                    #(BuiltIn::#analog_operators_sysfun)|* =>true,
                    _ => false
                }
            }

            #[allow(clippy::match_like_matches_macro)]
            pub fn is_unsupported(self)->bool{
                match self{
                    #(BuiltIn::#unsupported)|* =>true,
                    _ => false
                }
            }

            #[allow(clippy::match_like_matches_macro)]
            pub fn is_analysis_var(self)->bool{
                match self{
                    #(BuiltIn::#analysis_funs)|* =>true,
                    _ => false
                }
            }
        }

        pub fn insert_builtin_scope(dst: &mut IndexMap<Name, ScopeDefItem, RandomState>){
            #(dst.insert(#kw_types::#kws,BuiltIn::#variants.into());)*
        }


        pub fn insert_modulle_builtin_scope(dst: &mut IndexMap<Name, ScopeDefItem, RandomState>){
            #(dst.insert(sysfun::#params,ParamSysFun::#params.into());)*
        }
    };

    let header = "use ahash::RandomState;
        use indexmap::IndexMap;
        use syntax::name::{kw, sysfun, Name};

        use crate::nameres::ScopeDefItem;
    ";

    let hir_def = format!("{}\n{}", header, hir_def);

    let hir_def = add_preamble("generate_builtins", reformat(hir_def));
    let file = project_root().join("openvaf").join("hir_def").join("src").join("builtin.rs");
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
        .join("openvaf")
        .join("hir_ty")
        .join("src")
        .join("builtin")
        .join("generated.rs");
    ensure_file_contents(&file, &hir_ty);
}
