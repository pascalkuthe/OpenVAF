use std::mem::size_of_val;

use mir::{Const, Function, Opcode, Value, FALSE, F_ONE, F_ZERO, ONE, TRUE, ZERO};

pub fn eval_binary(func: &mut Function, op: Opcode, lhs: Const, rhs: Const) -> Value {
    match (lhs, rhs) {
        (Const::Int(lhs), Const::Int(rhs)) => match op {
            Opcode::Iadd => func.dfg.iconst(lhs + rhs),
            Opcode::Isub => func.dfg.iconst(lhs - rhs),
            Opcode::Imul => func.dfg.iconst(lhs * rhs),
            Opcode::Idiv => func.dfg.iconst(lhs / rhs),
            Opcode::Irem => func.dfg.iconst(lhs % rhs),

            Opcode::Ishl => func.dfg.iconst(lhs << rhs),
            Opcode::Ishr => func.dfg.iconst(lhs >> rhs),
            Opcode::Ixor => func.dfg.iconst(lhs ^ rhs),
            Opcode::Iand => func.dfg.iconst(lhs & rhs),
            Opcode::Ior => func.dfg.iconst(lhs | rhs),

            Opcode::Ilt => (lhs < rhs).into(),
            Opcode::Igt => (lhs > rhs).into(),
            Opcode::Ige => (lhs >= rhs).into(),
            Opcode::Ile => (lhs <= rhs).into(),
            Opcode::Ieq => (lhs == rhs).into(),
            Opcode::Ine => (lhs != rhs).into(),

            _ => unreachable!("invalid int operation {}", op),
        },

        (Const::Float(lhs), Const::Float(rhs)) => {
            let lhs: f64 = lhs.into();
            let rhs: f64 = rhs.into();
            match op {
                Opcode::Fadd => func.dfg.f64const(lhs + rhs),
                Opcode::Fsub => func.dfg.f64const(lhs - rhs),
                Opcode::Fmul => func.dfg.f64const(lhs * rhs),
                Opcode::Fdiv => func.dfg.f64const(lhs / rhs),
                Opcode::Frem => func.dfg.f64const(lhs % rhs),

                Opcode::Flt => (lhs < rhs).into(),
                Opcode::Fgt => (lhs > rhs).into(),
                Opcode::Fge => (lhs >= rhs).into(),
                Opcode::Fle => (lhs <= rhs).into(),
                Opcode::Feq => (lhs == rhs).into(),
                Opcode::Fne => (lhs != rhs).into(),

                Opcode::Hypot => func.dfg.f64const(lhs.hypot(rhs)),
                Opcode::Atan2 => func.dfg.f64const(lhs.atan2(rhs)),
                Opcode::Pow => func.dfg.f64const(lhs.powf(rhs)),
                _ => unreachable!("invalid real operation  {}", op,),
            }
        }
        _ => match op {
            Opcode::Seq | Opcode::Beq => (lhs == rhs).into(),
            Opcode::Sne | Opcode::Bne => (lhs != rhs).into(),
            _ => unreachable!("invalid operation {} {:?} {:?}", op, lhs, rhs),
        },
    }
}

pub fn eval_unary(func: &mut Function, op: Opcode, val: Const) -> Option<Value> {
    if op == Opcode::OptBarrier {
        return None;
    }
    let val = match val {
        mir::Const::Float(val) => {
            let val: f64 = val.into();
            match op {
                Opcode::Sqrt => func.dfg.f64const(val.sqrt()),
                Opcode::Exp => func.dfg.f64const(val.exp()),
                Opcode::Ln => func.dfg.f64const(val.ln()),
                Opcode::Log => func.dfg.f64const(val.log10()),
                Opcode::Floor => func.dfg.f64const(val.floor()),
                Opcode::Ceil => func.dfg.f64const(val.ceil()),
                Opcode::Sin => func.dfg.f64const(val.sin()),
                Opcode::Cos => func.dfg.f64const(val.cos()),
                Opcode::Tan => func.dfg.f64const(val.tan()),
                Opcode::Asin => func.dfg.f64const(val.asin()),
                Opcode::Acos => func.dfg.f64const(val.acos()),
                Opcode::Atan => func.dfg.f64const(val.atan()),
                Opcode::Sinh => func.dfg.f64const(val.sinh()),
                Opcode::Cosh => func.dfg.f64const(val.cosh()),
                Opcode::Tanh => func.dfg.f64const(val.tanh()),
                Opcode::Asinh => func.dfg.f64const(val.asinh()),
                Opcode::Acosh => func.dfg.f64const(val.acosh()),
                Opcode::Atanh => func.dfg.f64const(val.atanh()),
                Opcode::FIcast => func.dfg.iconst(val.round() as i32),
                Opcode::FBcast => (val.abs() != 0.0).into(),
                Opcode::Fneg => func.dfg.f64const(-val),
                _ => unreachable!("invalid real operation {}", op),
            }
        }
        mir::Const::Int(val) => match op {
            Opcode::Inot => func.dfg.iconst(!val),
            Opcode::Ineg => func.dfg.iconst(-val),
            Opcode::IFcast => func.dfg.f64const(val as f64),
            Opcode::IBcast => (val != 0).into(),
            Opcode::Clog2 => {
                let val = 8 * size_of_val(&val) as i32 - val.leading_zeros() as i32;
                func.dfg.iconst(val)
            }
            _ => unreachable!("invalid int operation {}", op),
        },
        mir::Const::Str(_) => unreachable!(),
        mir::Const::Bool(true) => match op {
            Opcode::Bnot => FALSE,
            Opcode::BIcast => ONE,
            Opcode::BFcast => F_ONE,
            _ => unreachable!(),
        },
        mir::Const::Bool(false) => match op {
            Opcode::Bnot => TRUE,
            Opcode::BIcast => ZERO,
            Opcode::BFcast => F_ZERO,
            _ => unreachable!(),
        },
    };
    Some(val)
}
