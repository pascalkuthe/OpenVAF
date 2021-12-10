use std::str::FromStr;

use stdx::impl_debug;
use Op::*;

use crate::Callback;

#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub enum Op {
    // unary op
    Copy,

    IntBitNegate,
    BoolBitNegate,

    RealArtihNeg,
    IntArithNeg,

    // casts (also unary)
    RealToInt,
    IntToReal,
    BoolToReal,
    BoolToInt,
    IntToBool,

    ArrRealToInt,
    ArrIntToReal,
    ArrBoolToReal,
    ArrBoolToInt,
    ArrIntToBool,

    RealToComplex,
    RealComponent,
    ImagComponent,

    // Bin Op
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntRem,

    IntShl,
    IntShr,
    IntAShl,
    IntAShr,
    IntXor,
    IntNXor,
    IntAnd,
    IntOr,

    RealAdd,
    RealSub,
    RealMul,
    RealDiv,
    RealRem,

    CmplxPlus,
    CmplxMinus,
    CmplxMul,
    CmplxDiv,

    IntLessThen,
    IntGreaterThen,
    RealLessThen,
    RealGreaterThen,
    IntLessEqual,
    IntGreaterEqual,
    RealLessEqual,
    RealGreaterEqual,

    IntEq,
    RealEq,
    StringEq,
    BoolEq,

    IntNeq,
    RealNeq,
    StringNeq,
    BoolNeq,

    // VarArgs
    // CreateRealArray,
    // CreateIntArray,
    // CreateStringArray,
    // CreateComplexArray,

    // ConcatRealArray,
    // ConactIntArray,
    // ConactStringArray,
    // ConactComplexArray,
    Sqrt,
    Exp,
    LimExp,
    Ln,
    Log,
    Clog2,
    Floor,
    Ceil,

    Sin,
    Cos,
    Tan,
    Hypot,

    ArcSin,
    ArcCos,
    ArcTan,
    ArcTan2,

    SinH,
    CosH,
    TanH,

    ArcSinH,
    ArcCosH,
    ArcTanH,

    RealMin,
    RealMax,
    RealAbs,
    RealPow,

    IntAbs,
    IntPow,
    IntMin,
    IntMax,

    Call(Callback),
}

impl_debug! {
    match Op{

        // unary op
        Copy                 => "copy";

        IntBitNegate        => "i32~";
        BoolBitNegate       => "bool~";

        RealArtihNeg        => "f64-";
        IntArithNeg         => "i32-";

        RealToInt           => "cast_f64_i32";
        IntToReal           => "cast_i32_f64";
        BoolToInt           => "cast_bool_i32";
        IntToBool           => "cast_i32_bool";
        BoolToReal          => "cast_bool_real";


        ArrRealToInt           => "arr_cast_f64_i32";
        ArrIntToReal           => "arr_cast_i32_f64";
        ArrBoolToInt           => "arr_cast_bool_i32";
        ArrIntToBool           => "arr_cast_i32_bool";
        ArrBoolToReal          => "arr_cast_bool_real";





        RealToComplex       => "cast_f64_c64";
        RealComponent       => "c_real";
        ImagComponent       => "c_imag";

        // Bin Op
        IntAdd              => "i32.+";
        IntSub              => "i32.-";
        IntMul              => "i32.*";
        IntDiv              => "i32./";
        IntRem              => "i32.%";

        IntShl              => "i32.<<";
        IntShr              => "i32.>>";
        IntAShl             => "i32.<<<";
        IntAShr             => "i32.>>>";
        IntXor              => "i32.^";
        IntNXor             => "i32.=";
        IntAnd              => "i32.&";
        IntOr               => "i32.|";

        RealAdd             => "f64.+";
        RealSub             => "f64.-";
        RealMul             => "f64.*";
        RealDiv             => "f64./";
        RealRem             => "f64.%";

        CmplxPlus           => "c64.+";
        CmplxMinus          => "c64.-";
        CmplxMul            => "c64.*";
        CmplxDiv            => "c64./";

        IntLessThen         => "i32.<";
        IntGreaterThen      => "i32.>";
        RealLessThen        => "f64.<";
        RealGreaterThen     => "f64.>";
        IntLessEqual        => "i32.<=";
        IntGreaterEqual     => "i32.>=";
        RealLessEqual       => "f64.<=";
        RealGreaterEqual    => "f64.>=";


        IntEq               => "i32.==";
        RealEq              => "f64.==";
        StringEq            => "str.==";
        BoolEq              => "bool.==";

        IntNeq               => "i32.!=";
        RealNeq              => "f64.!=";
        StringNeq            => "str.!=";
        BoolNeq              => "bool.!=";


        // // VarArgs
        // CreateRealArray     => "f64.arr";
        // CreateIntArray      => "i32.arr";
        // CreateStringArray   => "str.arr";
        // CreateComplexArray  => "c64.arr";
        // ConcatRealArray     => "f64.arr_concat";
        // ConactIntArray      => "i32.arr_concat";
        // ConactStringArray   => "str.arr_concat";
        // ConactComplexArray  => "c64.arr_concat";

        Sqrt                => "sqrt";
        Exp                 => "exp";
        LimExp              => "limexp";
        Ln                  => "ln";
        Log                 => "log";
        Clog2               => "clog2";
        Floor               => "flor";
        Ceil                => "ceil";

        Sin                 => "sin";
        Cos                 => "cos";
        Tan                 => "tan";
        Hypot               => "hypot";

        ArcSin              => "asin";
        ArcCos              => "acos";
        ArcTan              => "atan";
        ArcTan2             => "atan2";

        SinH                => "sinh";
        CosH                => "cosh";
        TanH                => "tanh";

        ArcSinH             => "asinh";
        ArcCosH             => "acosh";
        ArcTanH             => "atanh";

        RealMin             => "f64.min";
        RealMax             => "f64.max";
        RealAbs             => "f64.abs";
        RealPow             => "f64.pow";

        IntAbs              => "i32.abs";
        IntPow              => "i32.pow";
        IntMin              => "i32.min";
        IntMax              => "i32.max";

        Call(c)             => "{:?}",c;

    }

}

impl FromStr for Op {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        let res = match s {
            "copy" => Copy,

            "i32~" => IntBitNegate,
            "bool~" => BoolBitNegate,

            "f64-" => RealArtihNeg,
            "i32-" => IntArithNeg,

            "cast_f64_i32" => RealToInt,
            "cast_i32_f64" => IntToReal,
            "cast_bool_i32" => BoolToInt,
            "cast_i32_bool" => IntToBool,
            "cast_bool_real" => BoolToReal,

            "arr_cast_f64_i32" => ArrRealToInt,
            "arr_cast_i32_f64" => ArrIntToReal,
            "arr_cast_bool_i32" => ArrBoolToInt,
            "arr_cast_i32_bool" => ArrIntToBool,
            "arr_cast_bool_real" => ArrBoolToReal,

            "cast_f64_c64" => RealToComplex,
            "c_real" => RealComponent,
            "c_imag" => ImagComponent,

            // Bin Op
            "i32.+" => IntAdd,
            "i32.-" => IntSub,
            "i32.*" => IntMul,
            "i32./" => IntDiv,
            "i32.%" => IntRem,

            "i32.<<" => IntShl,
            "i32.>>" => IntShr,
            "i32.<<<" => IntAShl,
            "i32.>>>" => IntAShr,
            "i32.^" => IntXor,
            "i32.=" => IntNXor,
            "i32.&" => IntAnd,
            "i32.|" => IntOr,

            "f64.+" => RealAdd,
            "f64.-" => RealSub,
            "f64.*" => RealMul,
            "f64./" => RealDiv,
            "f64.%" => RealRem,

            "c64.+" => CmplxPlus,
            "c64.-" => CmplxMinus,
            "c64.*" => CmplxMul,
            "c64./" => CmplxDiv,

            "i64.<" => IntLessThen,
            "i64.>" => IntGreaterThen,
            "f64.<" => RealLessThen,
            "f64.>" => RealGreaterThen,
            "i32.<=" => IntLessEqual,
            "i32.>=" => IntGreaterEqual,
            "f64.<=" => RealLessEqual,
            "f64.>=" => RealGreaterEqual,

            "i32.==" => IntEq,
            "f64.==" => RealEq,
            "str.==" => StringEq,
            "bool.==" => BoolEq,

            "i32.!=" => IntNeq,
            "f64.!=" => RealNeq,
            "str.!=" => StringNeq,
            "bool.!=" => BoolNeq,

            // // VarArgs
            // "f64.arr" => CreateRealArray,
            // "i32.arr" => CreateIntArray,
            // "str.arr" => CreateStringArray,
            // "c64.arr" => CreateComplexArray,

            // "f64.arr_concat" => ConcatRealArray,
            // "i32.arr_concat" => ConactIntArray,
            // "str.arr_concat" => ConactStringArray,
            // "c64.arr_concat" => ConactComplexArray,
            "sqrt" => Sqrt,
            "exp" => Exp,
            "limexp" => LimExp,
            "ln" => Ln,
            "log" => Log,
            "clog2" => Clog2,
            "flor" => Floor,
            "ceil" => Ceil,

            "sin" => Sin,
            "cos" => Cos,
            "tan" => Tan,
            "hypot" => Hypot,

            "asin" => ArcSin,
            "acos" => ArcCos,
            "atan" => ArcTan,
            "atan2" => ArcTan2,

            "sinh" => SinH,
            "cosh" => CosH,
            "tanh" => TanH,

            "asinh" => ArcSinH,
            "acosh" => ArcCosH,
            "atanh" => ArcTanH,

            "f64.min" => RealMin,
            "f64.max" => RealMax,
            "f64.abs" => RealAbs,
            "f64.pow" => RealPow,

            "i32.abs" => IntAbs,
            "i32.pow" => IntPow,
            "i32.min" => IntMin,
            "i32.max" => IntMax,

            _ => {
                if s.len() > 2 && &s[0..2] == "cb" {
                    let c = u32::from_str(&s[3..]).map_err(|e| e.to_string())?;
                    Call(Callback(c))
                } else {
                    return Err(format!("unkown op {}", s));
                }
            }
        };
        Ok(res)
    }
}
