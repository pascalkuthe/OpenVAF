use std::any::type_name;
use std::fmt::Display;
use std::hash::Hash;
use std::str::FromStr;

use ahash::AHashMap;
use lasso::{Rodeo, Spur};
use smallvec::SmallVec;
use typed_index_collections::TiVec;

// use crate::ty::Array;
use crate::{
    BasicBlock, BasicBlockData, CfgParam, Const, ControlFlowGraph, InstrDst, Instruction, Local,
    Op, Operand, Phi, Place, Terminator,
};

pub(crate) struct CfgParser {
    pub pos: usize,
    pub src: String,
    pub literals: lasso::Rodeo<Spur>,
}

impl CfgParser {
    pub fn new(src: &str) -> CfgParser {
        assert!(src.chars().all(|c| c.is_ascii()), "can only parse ascii cfg");
        CfgParser {
            src: src.replace(|c: char| c.is_ascii_whitespace(), ""),
            pos: 0,
            literals: lasso::Rodeo::new(),
        }
    }
    pub fn expect(&mut self, expect: &str) -> Result<(), String> {
        if self.eat(expect)? {
            Ok(())
        } else {
            Err(format!("expected {} here \n {}", expect, &self.src[self.pos..]))
        }
    }

    pub fn eat(&mut self, expect: &str) -> Result<bool, String> {
        if self.pos + expect.len() <= self.src.len() {
            let at = self.at(expect);
            if at {
                self.pos += expect.len()
            }
            Ok(at)
        } else {
            Err(format!("unexpected EOF! expected {}", expect))
        }
    }

    pub fn at(&self, expect: &str) -> bool {
        self.pos + expect.len() <= self.src.len()
            && &self.src[self.pos..self.pos + expect.len()] == expect
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T, String> {
        T::parse(self)
    }
}

pub(crate) trait Parse: Sized {
    fn parse(p: &mut CfgParser) -> Result<Self, String>;
}

impl<T: Parse> Parse for Vec<T> {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("[")?;
        let mut res: Vec<T> = Vec::new();
        while !p.eat("]")? {
            res.push(p.parse()?);
            if !p.at("]") {
                p.expect(",")?
            }
        }
        Ok(res)
    }
}

impl<K: Parse + Hash + Eq, V: Parse> Parse for AHashMap<K, V> {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("[")?;
        let mut res: AHashMap<K, V> = AHashMap::new();
        while !p.eat("]")? {
            p.expect("(")?;
            let key = p.parse()?;
            p.expect(",")?;
            let val = p.parse()?;
            p.expect(")")?;
            res.insert(key, val);
            if !p.at("]") {
                p.expect(",")?
            }
        }
        Ok(res)
    }
}

#[repr(transparent)]
pub(crate) struct ParseFromStr<T>(pub T);

impl<T> Parse for ParseFromStr<T>
where
    T: FromStr,
    T::Err: Display,
{
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        let pos = p.src[p.pos..]
            .find(|c| matches!(c, ';' | ',' | ']' | '[' | '}' | '{' | ':' | '(' | ')'))
            .ok_or_else(|| format!("unexpected EOF; expected {}", type_name::<T>()))?;
        let src = &p.src[p.pos..p.pos + pos];
        p.pos += pos;
        match T::from_str(src) {
            Ok(val) => Ok(ParseFromStr(val)),
            Err(err) => Err(format!(
                "failed to parse {} from {}: {}\n{}",
                type_name::<T>(),
                src,
                err,
                &p.src[p.pos..]
            )),
        }
    }
}

impl Parse for Operand {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        let res = if p.at("_") {
            Operand::Local(p.parse()?)
        } else if p.at("#") {
            Operand::CfgParam(p.parse()?)
        } else if p.at("p") {
            Operand::Place(p.parse()?)
        } else {
            Operand::Const(p.parse()?)
        };
        Ok(res)
    }
}

impl Parse for Local {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("_")?;
        Ok(Local(ParseFromStr::parse(p)?.0))
    }
}
impl Parse for BasicBlock {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("bb")?;
        Ok(BasicBlock(ParseFromStr::parse(p)?.0))
    }
}

impl Parse for CfgParam {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("#")?;
        Ok(CfgParam(ParseFromStr::parse(p)?.0))
    }
}

impl Parse for Place {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("p")?;
        Ok(Place(ParseFromStr::parse(p)?.0))
    }
}

impl Parse for Const {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        if p.eat("true") == Ok(true) {
            return Ok(Self::Bool(true));
        }

        if p.eat("false") == Ok(true) {
            return Ok(Self::Bool(false));
        }
        if p.eat("[]") == Ok(true) {
            return Ok(Self::Zst);
        }

        let src = &p.src[p.pos..];
        let ty = &src[..3];
        let is_arr = &src[3..5] == "[]";
        if is_arr {
            p.pos += 5
        } else {
            p.pos += 3
        }

        let res = match ty {
            // "f64" if is_arr => Const::RealArray(p.parse()?),
            "f64" => Const::Real(ParseFromStr::<f64>::parse(p)?.0),

            // "i32" if is_arr => Const::IntArray(p.parse()?),
            "i32" => Const::Int(ParseFromStr::<i32>::parse(p)?.0),

            // "c64" if is_arr => Const::ComplexArray(p.parse()?),
            // "c64" => Const::Complex(p.parse()?),

            // "str" if is_arr => Const::StringArray(p.parse()?),
            "str" => Const::String(p.parse()?),

            _ => return Err(format!("{:?} is not a valid const type", src)),
        };

        Ok(res)
    }
}

// impl Parse for Array<f64> {
//     fn parse(p: &mut CfgParser) -> Result<Self, String> {
//         let p: Vec<ParseFromStr<f64>> = Vec::parse(p)?;
//         Ok(Array::from_header_and_iter((), p.into_iter().map(|f| f.0)))
//     }
// }

impl Parse for Spur {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("\"")?;
        let pos = p.pos;
        while !p.eat("\"")? {
            if p.at("\\") {
                p.pos += 2;
            } else {
                p.pos += 1
            }
        }
        let src = p.src[pos..p.pos - 1]
            .replace("\\\n", "\n")
            .replace("\\\t", "\t")
            .replace("\\\\", "\\")
            .replace("\\\"", "\"");
        Ok(p.literals.get_or_intern(&src))
    }
}

// impl<T: Parse> Parse for Array<T> {
//     fn parse(p: &mut CfgParser) -> Result<Self, String> {
//         let res = Vec::parse(p)?;
//         Ok(Array::from_header_and_iter((), res.into_iter()))
//     }
// }

// impl Parse for Array<i32> {
//     fn parse(p: &mut CfgParser) -> Result<Self, String> {
//         let p: Vec<ParseFromStr<i32>> = Vec::parse(p)?;
//         Ok(Array::from_header_and_iter((), p.into_iter().map(|f| f.0)))
//     }
// }

impl Parse for Instruction {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        let dst = if &p.src[p.pos..][..3] == "let" {
            p.pos += 3;
            let dst = p.parse()?;
            p.expect(":=")?;
            dst
        } else {
            InstrDst::Ignore
        };

        let op: Op = ParseFromStr::parse(p)?.0;
        let args: Vec<_> = p.parse()?;
        p.expect(";")?;
        let src = if p.eat("//src") == Ok(true) {
            let src = ParseFromStr::parse(p)?.0;
            p.expect(";")?;
            src
        } else {
            0 // no src found optional as not actually required for the CFG
        };

        let res = Instruction { dst, op, args: SmallVec::from_vec(args), src };
        Ok(res)
    }
}

impl Parse for InstrDst {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        if p.at("_") {
            Ok(Self::Local(p.parse()?))
        } else if p.at("p") {
            Ok(Self::Place(p.parse()?))
        } else {
            Err("Invalid InstrDst expected 'p' (place) or '_' (local) ".to_owned())
        }
    }
}

impl Parse for Phi {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("phi")?;
        let dst = p.parse()?;
        p.expect(":=")?;
        let sources = p.parse()?;
        p.expect(";")?;
        Ok(Phi { dst, sources })
    }
}

impl Parse for BasicBlockData {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        let mut phis = TiVec::new();
        let mut instructions = TiVec::new();

        while p.at("phi") {
            let phi = p.parse()?;
            phis.push(phi);
        }

        let mut terminator = None;
        while !p.at("bb") && p.pos < p.src.len() {
            if p.at("goto") || p.at("if") || p.at("end") {
                terminator = Some(p.parse()?);
                break;
            }
            let instr = p.parse()?;
            instructions.push(instr);
        }

        Ok(BasicBlockData { phis, instructions, terminator })
    }
}

impl Parse for Terminator {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        if p.eat("goto")? {
            let bb = p.parse()?;
            p.expect(";")?;
            return Ok(Terminator::Goto(bb));
        }

        if p.eat("if")? {
            let condition = p.parse()?;
            p.expect("{")?;
            let true_block = p.parse()?;
            p.expect("}else{")?;
            let false_block = p.parse()?;
            p.expect("}")?;
            let loop_head = p.eat("(loop)")?;

            return Ok(Terminator::Split { condition, true_block, false_block, loop_head });
        }

        if p.eat("end")? {
            return Ok(Terminator::Ret);
        }

        Err(format!("unkown terminator: {}", &p.src[p.pos..]))
    }
}

impl Parse for ControlFlowGraph {
    fn parse(p: &mut CfgParser) -> Result<Self, String> {
        p.expect("{")?;

        let mut res = ControlFlowGraph::default();
        p.expect("next_local")?;
        res.next_local = p.parse()?;
        p.expect(";")?;

        p.expect("next_place")?;
        res.next_place = p.parse()?;
        p.expect(";")?;

        while !p.eat("}")? {
            let bb: BasicBlock = p.parse()?;
            assert_eq!(bb, res.blocks.next_key());
            p.expect(":")?;

            let bb = p.parse()?;
            res.blocks.push(bb);
        }
        Ok(res)
    }
}

impl ControlFlowGraph {
    pub fn parse(src: &str) -> Result<(ControlFlowGraph, Rodeo<Spur>), String> {
        let mut parser = CfgParser::new(src);
        Ok((parser.parse()?, parser.literals))
    }
}
