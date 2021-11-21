use hir_def::Type;

mod generated;

use crate::requirements::TypeRequirement;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct BuiltinInfo {
    pub signatures: &'static [Signature],
    pub min_args: usize,
    pub max_args: Option<usize>,
    pub has_side_effects: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Signature {
    args: &'static [TypeRequirement],
    return_ty: Type,
}

pub use  generated::bultin_info;

impl BuiltinInfo {
    const fn new(signatures: &'static [Signature], has_side_effects: bool) -> BuiltinInfo {
        let mut min_args = None;
        let mut max_args = None;
        let mut i = 0;
        while i < signatures.len() {
            let arg_cnt = signatures[i].args.len();
            match min_args {
                None => max_args = Some(arg_cnt),
                Some(old_min) if arg_cnt < old_min => min_args = Some(arg_cnt),
                _ => (),
            }
            match max_args {
                None => max_args = Some(arg_cnt),
                Some(old_max) if arg_cnt > old_max => max_args = Some(arg_cnt),
                _ => (),
            }
            i += 1;
        }
        let min_args = match min_args {
            Some(val) => val,
            None => 0,
        };
        BuiltinInfo { signatures, min_args, max_args, has_side_effects }
    }

    const fn impure_fn(signatures: &'static [Signature]) -> BuiltinInfo {
        BuiltinInfo::new(signatures, true)
    }

    const fn pure_fn(signatures: &'static [Signature]) -> BuiltinInfo {
        BuiltinInfo::new(signatures, false)
    }

    const fn specical_cased_pure(min_args: usize, max_args: Option<usize>) -> BuiltinInfo {
        BuiltinInfo { signatures: &[], min_args, max_args, has_side_effects: false }
    }

    const fn specical_cased_impure(min_args: usize, max_args: Option<usize>) -> BuiltinInfo {
        BuiltinInfo { signatures: &[], min_args, max_args, has_side_effects: true }
    }
}

use Type::*;
use TypeRequirement::*;

macro_rules! bultins {
    {
        const fn $name: ident= {
            $(fn($($args: expr),*) -> $ty: ident;)*
        }
        $($rem: tt)*
    } => {
        const $name: BuiltinInfo = BuiltinInfo::pure_fn(
            &[$(Signature{
                args: &[$($args),*],
                return_ty: Type::$ty,
            }),*]
        );
        bultins!($($rem)*);
    };

    {
        fn $name: ident = {
            $(fn($($args: expr),*) -> $ty: ident;)*
        }
        $($rem: tt)*
    } => {
        const $name: BuiltinInfo = BuiltinInfo::impure_fn(
            &[$(Signature{
                args: &[$($args),*],
                return_ty: Type::$ty,
            }),*]
        );
        bultins!($($rem)*);
    };

    {} => {};
}

bultins! {

    const fn ANALYSIS = {
        fn(Val(String)) -> Bool;
    }
    const fn AC_STIM = {
        fn() -> Real;
        fn(Val(String)) -> Real;
        fn(Val(String),Val(Real)) -> Real;
        fn(Val(String),Val(Real),Val(Real)) -> Real;
    }

    const fn REAL_MATH_1 = {
        fn(Val(Real)) -> Real;
    }

    const fn REAL_MATH_2 = {
        fn(Val(Real),Val(Real)) -> Real;
    }

    const fn REAL_INFO = {
        fn() -> Real;
    }


    const fn VT = {
        fn() -> Real;
        fn(Val(Real)) -> Real;
    }

    const fn FLICKER_NOISE = {
        fn(Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Val(Real),Literal(String)) -> Real;
    }

    const fn WHITE_NOISE = {
        fn(Val(Real)) -> Real;
        fn(Val(Real),Literal(String)) -> Real;
    }


    const fn NOISE_TABLE = {
        fn(ArrayAnyLength{ty: Real}) -> Real;
        fn(Val(String)) -> Real;
        fn(ArrayAnyLength{ty: Real},Literal(String)) -> Real;
        fn(Val(String),Literal(String)) -> Real;
    }

    const fn DDT = {
        fn(Val(Real)) -> Real;
        fn(Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Nature) -> Real;
    }

    const fn IDT = {
        fn(Val(Real)) -> Real;
        fn(Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Val(Real),Nature) -> Real;
    }

    const fn IDTMOD = {
        fn(Val(Real)) -> Real;
        fn(Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Val(Real),Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Val(Real),Val(Real),Nature) -> Real;
    }

    // all laplace fitlers have the same signature
    const fn LAPLACE_FILTER = {
        fn(Val(Real),ArrayAnyLength{ty: Real},ArrayAnyLength{ty: Real}) -> Real;
        fn(Val(Real),ArrayAnyLength{ty: Real},ArrayAnyLength{ty: Real}, Val(Real)) -> Real;
        fn(Val(Real),ArrayAnyLength{ty: Real},ArrayAnyLength{ty: Real}, Nature) -> Real;
    }

    // all zi filters have the same signature
    const fn ZI_FILTER = {
        fn(Val(Real),ArrayAnyLength{ty: Real},ArrayAnyLength{ty: Real}, Val(Real)) -> Real;
        fn(Val(Real),ArrayAnyLength{ty: Real},ArrayAnyLength{ty: Real}, Val(Real), Val(Real)) -> Real;
        fn(Val(Real),ArrayAnyLength{ty: Real},ArrayAnyLength{ty: Real}, Val(Real), Val(Real), Val(Real)) -> Real;
    }

    const fn ABSDELAY = {
        fn(Val(Real), Val(Real)) -> Real;
        fn(Val(Real), Val(Real), Val(Real)) -> Real;
    }

    const fn SLEW = {
        fn(Val(Real)) -> Real;
        fn(Val(Real),Val(Real)) -> Real;
        fn(Val(Real),Val(Real),Val(Real)) -> Real;
    }


    const fn LAST_CROSSING = {
        fn(Val(Real)) -> Real;
        fn(Val(Real),Val(Integer)) -> Real;
    }

    fn BASIC_IO = {
        fn(Val(Integer)) -> Integer;
    }

    fn FOPEN = {
        fn(Val(String)) -> Integer;
        fn(Val(String), Val(String)) -> Integer;
    }

    fn FGETS = {
        fn(Var(String),Val(Integer)) -> Integer;
    }

    fn FSEEK = {
        fn(Val(Integer),Val(Integer),Val(Integer)) -> Integer;
    }

    fn FFLUSH = {
        fn() -> Integer;
        fn(Val(Integer)) -> Integer;
    }

    const fn FERROR = {
        fn(Val(Integer),Var(String)) -> Integer;
    }

    fn FINISH = {
        fn() -> Void;
        fn(Val(Integer)) -> Void;
    }

    const fn SIMPARAM = {
        fn(Literal(String)) -> Real;
        fn(Literal(String),Val(Real)) -> Real;
    }

    const fn SIMPARAM_STR = {
        fn(Literal(String)) -> Real;
    }

    const fn RANDOM = {
        fn() -> Integer;
        fn(Var(Integer)) -> Integer;
    }


    const fn ARANDOM = {
        fn() -> Integer;
        fn(Var(Integer)) -> Integer;
        fn(Var(Integer),Literal(String)) -> Integer;
        fn(Param(Integer)) -> Integer;
        fn(Param(Integer),Literal(String)) -> Integer;
    }


    const fn RDIST_1_ARG = {
        fn(Param(Integer),Val(Real)) -> Real;
        fn(Var(Integer),Val(Real)) -> Real;
        fn(Param(Integer),Val(Real),Literal(String)) -> Real;
        fn(Var(Integer),Val(Real),Literal(String)) -> Real;
    }

    const fn RDIST_2_ARG = {
        fn(Param(Integer),Val(Real),Val(Real)) -> Real;
        fn(Var(Integer),Val(Real),Val(Real)) -> Real;
        fn(Param(Integer),Val(Real),Val(Real),Literal(String)) -> Real;
        fn(Var(Integer),Val(Real),Val(Real),Literal(String)) -> Real;
    }


    const fn DIST_1_ARG = {
        fn(Param(Integer),Val(Integer)) -> Integer;
        fn(Var(Integer),Val(Integer)) -> Integer;
        fn(Param(Integer),Val(Integer),Literal(String)) -> Integer;
        fn(Var(Integer),Val(Integer),Literal(String)) -> Integer;
    }

    const fn DIST_2_ARG = {
        fn(Param(Integer),Val(Integer),Val(Integer)) -> Integer;
        fn(Var(Integer),Val(Integer),Val(Integer)) -> Integer;
        fn(Param(Integer),Val(Integer),Val(Integer),Literal(String)) ->Integer;
        fn(Var(Integer),Val(Integer),Val(Integer),Literal(String)) -> Integer;
    }

    const fn SIMPROBE = {
        fn(Val(String),Val(String))->Real;
        fn(Val(String),Val(String),Val(Real))->Real;
    }

    const fn TEST_PLUSARGS = {
        fn(Val(String))->Bool;
    }

    const fn VALUE_PLUSARGS = {
        fn(Val(String),Val(String))->Bool;
    }


    fn ANALOG_NODE_ALIAS = {
        fn(Net,Val(String)) -> Integer;
    }

    const fn PARAM_GIVEN = {
        fn(AnyParam) -> Bool;
    }

    const fn PORT_CONNECTED = {
        fn(Port) -> Bool;
    }

    fn DISCONTINUITY = {
        fn() -> Void;
        fn(Val(Integer)) -> Void;
    }

    fn BOUND_STEP = {
        fn(Val(Real)) -> Void;
    }
}

// TODO TABLE_MODEL

const MAX: BuiltinInfo = BuiltinInfo::specical_cased_pure(2, Some(2));
const DDX: BuiltinInfo = BuiltinInfo::specical_cased_pure(2, Some(2));
const FLOW: BuiltinInfo = BuiltinInfo::specical_cased_pure(1, Some(2));
const POT: BuiltinInfo = BuiltinInfo::specical_cased_pure(1, Some(2));
const DISPLAY_FUN: BuiltinInfo = BuiltinInfo::specical_cased_impure(0, None);
const FDISPLAY_FUN: BuiltinInfo = BuiltinInfo::specical_cased_impure(1, None);
const SFORMAT: BuiltinInfo = BuiltinInfo::specical_cased_impure(2, None);
const LIMIT: BuiltinInfo = BuiltinInfo::specical_cased_impure(2, None);
const FATAL: BuiltinInfo = BuiltinInfo::specical_cased_impure(1, None);

macro_rules! copied_builtins {
    {$($name: ident = $val: ident)*}=> {
        $(const $name: BuiltinInfo = $val;)*
    };
}

copied_builtins! {
    ACOS = REAL_MATH_1
    ACOSH = REAL_MATH_1
    ASIN = REAL_MATH_1
    ASINH = REAL_MATH_1
    ATAN = REAL_MATH_1
    ATANH = REAL_MATH_1
    COS = REAL_MATH_1
    COSH = REAL_MATH_1
    EXP = REAL_MATH_1
    FLOOR = REAL_MATH_1
    LN = REAL_MATH_1
    LOG = REAL_MATH_1
    CLOG2 = REAL_MATH_2
    LOG10 = REAL_MATH_1
    CEIL = REAL_MATH_1
    LIMEXP = REAL_MATH_1
    SIN = REAL_MATH_1
    SINH = REAL_MATH_1
    SQRT = REAL_MATH_1
    TAN = REAL_MATH_1
    TANH = REAL_MATH_1

    ATAN2 = REAL_MATH_2
    HYPOT = REAL_MATH_2
    POW = REAL_MATH_2

    NOISE_TABLE_LOG = NOISE_TABLE

    LAPLACE_ND = LAPLACE_FILTER
    LAPLACE_NP = LAPLACE_FILTER
    LAPLACE_ZD = LAPLACE_FILTER
    LAPLACE_ZP = LAPLACE_FILTER


    ZI_ND = ZI_FILTER
    ZI_NP = ZI_FILTER
    ZI_ZD = ZI_FILTER
    ZI_ZP = ZI_FILTER

    // Types are special cased
    MIN = MAX

    DISPLAY = DISPLAY_FUN
    STROBE = DISPLAY_FUN
    MONITOR = DISPLAY_FUN
    WRITE = DISPLAY_FUN
    DEBUG = DISPLAY_FUN
    ERROR = DISPLAY_FUN
    WARNING = DISPLAY_FUN
    INFO = DISPLAY_FUN

    FDISPLAY = FDISPLAY_FUN
    FSTROBE = FDISPLAY_FUN
    FMONITOR = FDISPLAY_FUN
    FWRITE = FDISPLAY_FUN
    FDEBUG = FDISPLAY_FUN
    SSCANF = FDISPLAY_FUN
    FSCANF = FDISPLAY_FUN
    SWRITE = FDISPLAY_FUN

    REWIND = BASIC_IO
    FEOF = BASIC_IO
    FCLOSE = BASIC_IO
    FTELL = BASIC_IO

    STOP = FINISH

    ABSTIME = REAL_INFO
    TEMPERATURE = REAL_INFO
    MFACTOR = REAL_INFO
    XPOSITION = REAL_INFO
    YPOSITION = REAL_INFO
    ANGLE = REAL_INFO
    HFLIP = REAL_INFO
    VFLIP = REAL_INFO

    RDIST_CHI_SQUARE = RDIST_1_ARG
    RDIST_EXPONENTIAL = RDIST_1_ARG
    RDIST_POISSON = RDIST_1_ARG
    RDIST_T = RDIST_1_ARG

    RDIST_UNIFORM = RDIST_2_ARG
    RDIST_ERLANG = RDIST_2_ARG
    RDIST_NORMAL = RDIST_2_ARG

    DIST_CHI_SQUARE = DIST_1_ARG
    DIST_EXPONENTIAL = DIST_1_ARG
    DIST_POISSON = DIST_1_ARG
    DIST_T = DIST_1_ARG

    DIST_UNIFORM = DIST_2_ARG
    DIST_ERLANG = DIST_2_ARG
    DIST_NORMAL = DIST_2_ARG

    ANALOG_PORT_ALIAS = ANALOG_NODE_ALIAS
}
