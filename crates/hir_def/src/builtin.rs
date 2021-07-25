use crate::allowed_options::{AllowedOps, ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR};
use crate::name::{kw, sysfun, Name};
use crate::Type;
// use data_structures::iter::zip;

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinFunctionInfo {
    pub name: &'static str,
    pub min_arg_cnt: u8,
    pub max_arg_cnt: Option<u8>,
    pub has_side_effects: bool,
    pub allowed_operations: &'static [AllowedOps],
    return_ty: InferReturnType,
}

impl BuiltinFunctionInfo {
    const fn with_name(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }

    pub fn infer_return_type(&self, args: &[Type]) -> Option<Type> {
        let ty = match self.return_ty {
            InferReturnType::Numeric1 => args[0].clone(),
            InferReturnType::Numeric2 => match args {
                [Type::Real, Type::Real]
                | [Type::Real, Type::Integer]
                | [Type::Integer, Type::Real]
                | [Type::Real, Type::Bool]
                | [Type::Bool, Type::Real] => Type::Real,

                [Type::Integer, Type::Integer]
                | [Type::Integer, Type::Bool]
                | [Type::Bool, Type::Integer]
                | [Type::Bool, Type::Bool] => Type::Integer,
                _ => Type::Err,
            },
            InferReturnType::Const(ref ty) => ty.clone(),
            InferReturnType::NoReturn => return None,
        };
        Some(ty)
    }

    // pub fn check_args(&self, args: &[Type]) {
    //     match self.arg_types {
    //         ArgTypes::Numeric => {
    //             for (arg_i, ty) in args.iter().enumerate() {
    //                 if !ty.is_numeric() {
    //                     todo!("Error")
    //                 }
    //             }
    //         }
    //         ArgTypes::Print => (), // TODO parse and check this for the common case of string literal format args
    //         ArgTypes::NoiseTable => {
    //             match args[0] {
    //                 Type::String => (),
    //                 Type::Array { ty, .. } if ty.is_numeric() => (),
    //                 _ => todo!("ERROR"),
    //             }

    //             if !matches!(args.get(1), Some(Type::String) | None) {
    //                 todo!("ERROR")
    //             }
    //         }
    //         ArgTypes::Explicit(expted_types) => {
    //             for (arg_i, (ty, expected_ty)) in zip(args, expted_types).enumerate() {
    //                 if !ty.is_convertable_to(expected_ty) {
    //                     todo!("Error")
    //                 }
    //             }
    //         }
    //         ArgTypes::Identifier => (),
    //         ArgTypes::LimFun => todo!(),
    //     }
    // }

    // Verilog-A is weakly typed and automatically converts types when neccessary (mostly INT ->
    // REAL). Since builtin functions may be varidaic over their function args a special funciton
    // is needed here
    // pub fn args_need_cast(&self, arg_i: usize, args: &[Type]) -> bool {
    //     match self.arg_types {
    //         ArgTypes::Numeric => self.infer_return_type(args) != Some(args[arg_i]),
    //         ArgTypes::NoiseTable if arg_i == 0 => match args[0] {
    //             Type::String => false,
    //             Type::Array { ty, .. } if &*ty == &Type::Real => false,
    //             _ => true,
    //         },
    //         ArgTypes::Explicit(expected_ty) => expected_ty[arg_i] != args[arg_i],
    //         _ => false,
    //     }
    // }
}

#[derive(Debug, Clone, PartialEq)]
enum InferReturnType {
    Numeric1,
    Numeric2,
    Const(Type),
    NoReturn,
}

const NUMERIC: &[Type] = &[Type::Real, Type::Integer];
const REAL: &[Type] = &[Type::Real];
const INTEGER: &[Type] = &[Type::Integer];

#[derive(Debug, Clone, PartialEq)]
enum ArgTypes {
    Identifier,
    Numeric,
    Print,
    NoiseTable,
    LimFun,
    Explicit(&'static [Type]),
}

const MATH1_NUMERIC: BuiltinFunctionInfo = BuiltinFunctionInfo {
    name: "",
    min_arg_cnt: 1,
    max_arg_cnt: Some(1),
    has_side_effects: false,
    allowed_operations: &[ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR],
    return_ty: InferReturnType::Numeric1,
    // arg_types: ArgTypes::Numeric,
};

const MATH1_REAL: BuiltinFunctionInfo = BuiltinFunctionInfo {
    name: "",
    min_arg_cnt: 1,
    max_arg_cnt: Some(1),
    has_side_effects: false,
    allowed_operations: &[ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR],
    return_ty: InferReturnType::Const(Type::Real),
    // arg_types: ArgTypes::Numeric,
};

const MATH2_REAL: BuiltinFunctionInfo = BuiltinFunctionInfo {
    name: "",
    min_arg_cnt: 2,
    max_arg_cnt: Some(2),
    has_side_effects: false,
    allowed_operations: &[
        ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
        ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
    ],
    return_ty: InferReturnType::Const(Type::Real),
    // arg_types: ArgTypes::Numeric,
};

const MATH2_NUMERIC: BuiltinFunctionInfo = BuiltinFunctionInfo {
    name: "",
    min_arg_cnt: 2,
    max_arg_cnt: Some(2),
    has_side_effects: false,
    allowed_operations: &[
        ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
        ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
    ],
    return_ty: InferReturnType::Const(Type::Real),
    // arg_types: ArgTypes::Numeric,
};

const PRINT: BuiltinFunctionInfo = BuiltinFunctionInfo {
    name: "",
    min_arg_cnt: 1,
    max_arg_cnt: None,
    has_side_effects: true,
    allowed_operations: &[ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR],
    return_ty: InferReturnType::NoReturn,
    // arg_types: ArgTypes::Print,
};

mod info {
    use crate::allowed_options::Allowed;

    use super::*;
    use kw::raw as kw;
    use sysfun::raw as sysfun;

    // Mathmatical functions
    pub const ABS: BuiltinFunctionInfo = MATH1_NUMERIC.with_name(kw::abs);
    pub const MIN: BuiltinFunctionInfo = MATH2_NUMERIC.with_name(kw::min);
    pub const MAX: BuiltinFunctionInfo = MATH2_NUMERIC.with_name(kw::max);
    pub const HYPOT: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::hypot);
    pub const POW: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::pow);
    pub const SQRT: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::sqrt);
    pub const LIMEXP: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::limexp);
    pub const EXP: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::exp);
    pub const LN: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::ln);
    pub const LOG: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::log);
    pub const CLOG2: BuiltinFunctionInfo = MATH1_REAL.with_name(sysfun::clog2);
    pub const FLOOR: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::floor);
    pub const CEIL: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::ceil);
    pub const SIN: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::sin);
    pub const COS: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::cos);
    pub const TAN: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::tan);
    pub const ARC_SIN: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::asin);
    pub const ARC_COS: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::acos);
    pub const ARC_TAN: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::atan);
    pub const ARC_TAN_2: BuiltinFunctionInfo = MATH2_REAL.with_name(kw::atan2);
    pub const SIN_H: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::sinh);
    pub const COS_H: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::cosh);
    pub const TAN_H: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::tanh);
    pub const ARC_SIN_H: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::asinh);
    pub const ARC_COS_H: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::acosh);
    pub const ARC_TAN_H: BuiltinFunctionInfo = MATH1_REAL.with_name(kw::atanh);

    pub const STROBE: BuiltinFunctionInfo = PRINT.with_name(sysfun::strobe);
    pub const DISPLAY: BuiltinFunctionInfo = PRINT.with_name(sysfun::display);
    pub const WRITE: BuiltinFunctionInfo = PRINT.with_name(sysfun::write);
    pub const DEBUG: BuiltinFunctionInfo = PRINT.with_name(sysfun::debug);
    pub const INFO: BuiltinFunctionInfo = PRINT.with_name(sysfun::info);
    pub const WARN: BuiltinFunctionInfo = PRINT.with_name(sysfun::warning);
    pub const ERROR: BuiltinFunctionInfo = PRINT.with_name(sysfun::error);
    pub const FATAL: BuiltinFunctionInfo = PRINT.with_name(sysfun::fatal);

    pub const WHITE_NOISE: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: kw::white_noise,
        min_arg_cnt: 1,
        max_arg_cnt: Some(2),
        has_side_effects: false,
        allowed_operations: &[ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR, AllowedOps::empty()],
        return_ty: InferReturnType::Const(Type::Real),
        // arg_types: ArgTypes::Explicit(&[Type::Real, Type::String]),
    };
    pub const FLICKR_NOISE: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: kw::flicker_noise,
        min_arg_cnt: 2,
        max_arg_cnt: Some(3),
        has_side_effects: false,
        allowed_operations: &[
            ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
            ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR,
            AllowedOps::empty(),
        ],
        return_ty: InferReturnType::Const(Type::Real),
        // arg_types: ArgTypes::Explicit(&[Type::Real, Type::Real, Type::String]),
    };
    pub const NOISE_TABLE: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: kw::noise_table,
        min_arg_cnt: 1,
        max_arg_cnt: Some(2),
        has_side_effects: false,
        allowed_operations: &[ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR, AllowedOps::empty()],
        return_ty: InferReturnType::Const(Type::Real),
        // arg_types: ArgTypes::NoiseTable,
    };
    pub const NOISE_TABLE_LOG: BuiltinFunctionInfo = NOISE_TABLE.with_name(kw::noise_table_log);

    pub const TEMPERATURE: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: sysfun::temperature,
        min_arg_cnt: 0,
        max_arg_cnt: Some(0),
        has_side_effects: false,
        allowed_operations: &[],
        return_ty: InferReturnType::Const(Type::Real),
        // arg_types: ArgTypes::Explicit(&[]),
    };

    pub const VT: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: sysfun::vt,
        min_arg_cnt: 0,
        max_arg_cnt: Some(1),
        has_side_effects: false,
        allowed_operations: &[ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR],
        return_ty: InferReturnType::Const(Type::Real),
        // arg_types: ArgTypes::Explicit(&[Type::Real]),
    };

    pub const SIMPARAM: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: sysfun::simparam,
        min_arg_cnt: 1,
        max_arg_cnt: Some(2),
        has_side_effects: false,
        allowed_operations: &[AllowedOps::empty(), ALLOWED_OPS_UNCONDITIONAL_ANALOG_BEHAVIOUR],
        return_ty: InferReturnType::Const(Type::Real),
        // arg_types: ArgTypes::Explicit(&[Type::String, Type::Real]),
    };

    pub const SIMPARAM_STR: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: sysfun::simparam,
        min_arg_cnt: 1,
        max_arg_cnt: Some(1),
        has_side_effects: false,
        allowed_operations: &[AllowedOps::empty()],
        return_ty: InferReturnType::Const(Type::String),
        // arg_types: ArgTypes::Explicit(&[Type::String]),
    };

    pub const PORT_CONNECTED: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: sysfun::port_connected,
        min_arg_cnt: 1,
        max_arg_cnt: Some(1),
        has_side_effects: false,
        allowed_operations: &[AllowedOps::new(&[Allowed::PortReferences])],
        return_ty: InferReturnType::Const(Type::Bool),
        // arg_types: ArgTypes::Identifier,
    };

    pub const PARAM_GIVEN: BuiltinFunctionInfo = BuiltinFunctionInfo {
        name: sysfun::param_given,
        min_arg_cnt: 1,
        max_arg_cnt: Some(1),
        has_side_effects: false,
        allowed_operations: &[AllowedOps::new(&[Allowed::ParameterReferences])],
        return_ty: InferReturnType::Const(Type::Bool),
        // arg_types: ArgTypes::Identifier,
    };
}

macro_rules! intrinsics {
    ($($names: ident),*) => {
        const BUILTIN_CNT: usize = [$(stringify!($names)),*].len();
        const BUILTIN_INFO: &'static [BuiltinFunctionInfo;BUILTIN_CNT] = &[$(info::$names),*];
        pub (crate) const BUILTIN_FUNCTIONS: &'static [BuiltinFunction] = &[$(BuiltinFunction::$names),*];


        #[doc(hidden)]
        mod intrinsic_idx{
            use super::*;
            intrinsics!(@constants BUILTIN_CNT as u8, $($names),*);
        }

        #[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
        #[repr(u8)]
        #[allow(non_camel_case_types)]
        pub enum BuiltinFunction{
            $($names=intrinsic_idx::$names),*
        }

    };
    (@constants $len: expr, $first_name: ident$(,$names: ident)+) => {
        pub const $first_name: u8 = {
            const LEN: usize = [$(stringify!($names)),*].len() + 1;
            $len - LEN as u8
        };

        intrinsics!(@constants $len, $($names),*);
    };

    (@constants $len: expr, $first_name: ident) => {
        pub const $first_name: u8 = $len - 1;
    };
}

intrinsics![
    ABS,
    SQRT,
    LIMEXP,
    EXP,
    LN,
    LOG,
    CLOG2,
    FLOOR,
    CEIL,
    SIN,
    COS,
    TAN,
    ARC_SIN,
    ARC_COS,
    ARC_TAN,
    ARC_TAN_2,
    SIN_H,
    COS_H,
    TAN_H,
    ARC_COS_H,
    ARC_SIN_H,
    STROBE,
    DISPLAY,
    WRITE,
    DEBUG,
    INFO,
    WARN,
    ERROR,
    FATAL,
    WHITE_NOISE,
    FLICKR_NOISE,
    NOISE_TABLE,
    NOISE_TABLE_LOG,
    TEMPERATURE,
    VT,
    SIMPARAM,
    SIMPARAM_STR,
    PORT_CONNECTED,
    PARAM_GIVEN
];

impl BuiltinFunction {
    pub fn info(self) -> &'static BuiltinFunctionInfo {
        &BUILTIN_INFO[self as u8 as usize]
    }
}
