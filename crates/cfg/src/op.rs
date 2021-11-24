use std::str::FromStr;

use crate::Callback;
use stdx::impl_debug;
use Op::*;

#[derive(Clone, PartialEq, Eq)]
pub enum Op {
    // unary op
    Mov,

    IntBitNegate,
    BoolBitNegate,

    RealArtihNeg,
    IntArithNeg,

    // casts (also unary)
    RealToInt,
    IntToReal,
    BoolToInt,
    IntToBool,

    RealToComplex,
    RealComponent,
    ImagComponent,

    // Bin Op
    IntPlus,
    IntMinus,
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

    RealPlus,
    RealMinus,
    RealMul,
    RealDiv,

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

    // ternery op
    // TODO seperate op for each type?
    Select,

    // VarArgs
    CreateRealArray,
    CreateIntArray,
    CreateStringArray,
    CreateComplexArray,

    Sqrt,
    Exp,
    LimExp,
    Ln,
    Log,
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
        Mov                 => "move";

        IntBitNegate        => "i32~";
        BoolBitNegate       => "bool~";

        RealArtihNeg        => "f64-";
        IntArithNeg         => "i32-";

        RealToInt           => "cast_f64_i32";
        IntToReal           => "cast_i32_f64";
        BoolToInt           => "cast_bool_i32";
        IntToBool           => "cast_i32_bool";

        RealToComplex       => "cast_f64_c64";
        RealComponent       => "c_real";
        ImagComponent       => "c_imag";

        // Bin Op
        IntPlus             => "i32.+";
        IntMinus            => "i32.-";
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

        RealPlus            => "f64.+";
        RealMinus           => "f64.-";
        RealMul             => "f64.*";
        RealDiv             => "f64./";

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

        // ternery op
        Select              => "select";

        // // VarArgs
        CreateRealArray     => "f64.arr";
        CreateIntArray      => "i32.arr";
        CreateStringArray   => "str.arr";
        CreateComplexArray  => "c64.arr";

        Sqrt                => "sqrt";
        Exp                 => "exp";
        LimExp              => "limexp";
        Ln                  => "ln";
        Log                 => "log";
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
            "move" => Mov,

            "i32~" => IntBitNegate,
            "bool~" => BoolBitNegate,

            "f64-" => RealArtihNeg,
            "i32-" => IntArithNeg,

            "cast_f64_i32" => RealToInt,
            "cast_i32_f64" => IntToReal,
            "cast_bool_i32" => BoolToInt,
            "cast_i32_bool" => IntToBool,

            "cast_f64_c64" => RealToComplex,
            "c_real" => RealComponent,
            "c_imag" => ImagComponent,

            // Bin Op
            "i32.+" => IntPlus,
            "i32.-" => IntMinus,
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

            "f64.+" => RealPlus,
            "f64.-" => RealMinus,
            "f64.*" => RealMul,
            "f64./" => RealDiv,

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

            // ternery op
            "select" => Select,

            // // VarArgs
            "f64.arr" => CreateRealArray,
            "i32.arr" => CreateIntArray,
            "str.arr" => CreateStringArray,
            "c64.arr" => CreateComplexArray,

            "sqrt" => Sqrt,
            "exp" => Exp,
            "limexp" => LimExp,
            "ln" => Ln,
            "log" => Log,
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
                if s.len()>2 && &s[0..2] == "cb" {
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
