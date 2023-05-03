//! Converting MIR to text.
//!
//! The `write` module provides the `write_function` function which converts an IR `Function` to an
//! equivalent textual form. This textual form can be read back by the `mir-reader` crate.

use core::fmt::{self, Write};

use crate::entities::AnyEntity;
use crate::instructions::PhiNode;
use crate::{Block, Const, DataFlowGraph, Function, Inst, InstructionData, Value, ValueDef};

#[cfg(test)]
mod tests;

/// A `FuncWriter` used to decorate functions during printing.
pub trait FuncWriter {
    /// Write the basic block header for the current function.
    fn write_block_header(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        block: Block,
        indent: usize,
    ) -> fmt::Result;

    /// Write the given `inst` to `w`.
    fn write_instruction(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        inst: Inst,
        indent: usize,
    ) -> fmt::Result;

    /// Write the preamble to `w`. By default, this uses `write_entity_definition`.
    fn write_preamble(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        interner: &dyn lasso::Resolver,
    ) -> Result<bool, fmt::Error> {
        self.super_preamble(w, func, interner)
    }

    /// Default impl of `write_preamble`
    fn super_preamble(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        interner: &dyn lasso::Resolver,
    ) -> Result<bool, fmt::Error> {
        let mut any = false;

        // Write out all signatures before functions since function declarations can refer to
        // signatures.
        for (sig, sig_data) in func.dfg.signatures.iter_enumerated() {
            any = true;
            self.write_entity_definition(w, func, sig.into(), &sig_data)?;
        }

        for val in func.dfg.values() {
            match func.dfg.value_def(val) {
                ValueDef::Const(Const::Float(def)) if func.dfg.uses(val).next().is_some() => {
                    writeln!(w, "    {} = fconst {}", val, def)?
                }
                ValueDef::Const(Const::Int(def)) if func.dfg.uses(val).next().is_some() => {
                    writeln!(w, "    {} = iconst {}", val, def)?
                }
                ValueDef::Const(Const::Str(def)) if func.dfg.uses(val).next().is_some() => {
                    writeln!(w, "    {} = sconst {:?}", val, interner.resolve(&def))?
                }
                ValueDef::Const(Const::Bool(def)) if func.dfg.uses(val).next().is_some() => {
                    writeln!(w, "    // {} = bconst {}", val, def)?
                }
                _ => (),
            }
        }

        Ok(any)
    }

    /// Write an entity definition defined in the preamble to `w`.
    fn write_entity_definition(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        entity: AnyEntity,
        value: &dyn fmt::Display,
    ) -> fmt::Result {
        self.super_entity_definition(w, func, entity, value)
    }

    /// Default impl of `write_entity_definition`
    #[allow(unused_variables)]
    fn super_entity_definition(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        entity: AnyEntity,
        value: &dyn fmt::Display,
    ) -> fmt::Result {
        writeln!(w, "    {} = {}", entity, value)
    }
}

/// A `PlainWriter` that doesn't decorate the function.
pub struct PlainWriter;

impl FuncWriter for PlainWriter {
    fn write_instruction(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        inst: Inst,
        indent: usize,
    ) -> fmt::Result {
        write_instruction(w, func, inst, indent)
    }

    fn write_block_header(
        &mut self,
        w: &mut dyn Write,
        func: &Function,
        block: Block,
        indent: usize,
    ) -> fmt::Result {
        write_block_header(w, func, block, indent)
    }
}

/// Write `func` to `w` as equivalent text.
/// Use `isa` to emit ISA-dependent annotations.
pub fn write_function(
    w: &mut dyn Write,
    func: &Function,
    interner: &dyn lasso::Resolver,
) -> fmt::Result {
    decorate_function(&mut PlainWriter, w, func, interner)
}

/// Writes `func` to `w` as text.
/// write_function_plain is passed as 'closure' to print instructions as text.
/// pretty_function_error is passed as 'closure' to add error decoration.
pub fn decorate_function<FW: FuncWriter>(
    func_w: &mut FW,
    w: &mut dyn Write,
    func: &Function,
    interner: &dyn lasso::Resolver,
) -> fmt::Result {
    write!(w, "function ")?;
    write!(w, "%{}", func.name)?;
    write!(w, "(")?;

    let mut params: Vec<(usize, Value)> = func
        .dfg
        .values()
        .filter_map(|val| {
            if let ValueDef::Param(def) = func.dfg.value_def(val) {
                Some((def.into(), val))
            } else {
                None
            }
        })
        .collect();
    params.sort_by_key(|(pos, _)| *pos);
    let mut seen = false;
    for (_, val) in params {
        if seen {
            write!(w, ", ")?;
        } else {
            seen = true
        }
        write!(w, "{}", val)?;
    }
    writeln!(w, ") {{")?;
    let mut any = func_w.write_preamble(w, func, interner)?;
    for block in &func.layout {
        if any {
            writeln!(w)?;
        }
        decorate_block(func_w, w, func, block)?;
        any = true;
    }
    writeln!(w, "}}")
}

//----------------------------------------------------------------------
//
// Basic blocks

// fn write_arg(w: &mut dyn Write, arg: Value) -> fmt::Result {
//     write!(w, "{}", arg)
// }

/// Write out the basic block header, outdented:
///
///    block1:
///    block1(v1: i32):
///    block10(v4: f64, v5: b1):
///
pub fn write_block_header(
    w: &mut dyn Write,
    _func: &Function,
    block: Block,
    indent: usize,
) -> fmt::Result {
    // The `indent` is the instruction indentation. block headers are 4 spaces out from that.
    writeln!(w, "{1:0$}{2}:", indent - 4, "", block)
}

fn decorate_block<FW: FuncWriter>(
    func_w: &mut FW,
    w: &mut dyn Write,
    func: &Function,
    block: Block,
) -> fmt::Result {
    // Indent all instructions if any srclocs are present.
    let indent = if func.srclocs.iter().all(|loc| loc.is_default()) { 4 } else { 36 };

    func_w.write_block_header(w, func, block, indent)?;
    for inst in func.layout.block_insts(block) {
        func_w.write_instruction(w, func, inst, indent)?;
    }

    Ok(())
}

//----------------------------------------------------------------------
//
// Instructions

// /// Write out any aliases to the given target, including indirect aliases
// fn write_value_aliases(
//     w: &mut dyn Write,
//     aliases: &SecondaryMap<Value, Vec<Value>>,
//     target: Value,
//     indent: usize,
// ) -> fmt::Result {
//     let mut todo_stack = vec![target];
//     while let Some(target) = todo_stack.pop() {
//         for &a in &aliases[target] {
//             writeln!(w, "{1:0$}{2} -> {3}", indent, "", a, target)?;
//             todo_stack.push(a);
//         }
//     }

//     Ok(())
// }

fn write_instruction(w: &mut dyn Write, func: &Function, inst: Inst, indent: usize) -> fmt::Result {
    // Prefix containing source location, encoding, and value locations.
    let mut s = String::with_capacity(16);

    // Source location goes first.
    let srcloc = func.srclocs.get(inst).copied().unwrap_or_default();
    if !srcloc.is_default() {
        write!(s, "{} ", srcloc)?;
    }

    // Write out prefix and indent the instruction.
    write!(w, "{1:0$}", indent, s)?;

    // Write out the result values, if any.
    let mut has_results = false;
    for r in func.dfg.inst_results(inst) {
        if !has_results {
            has_results = true;
            write!(w, "{}", r)?;
        } else {
            write!(w, ", {}", r)?;
        }
    }
    if has_results {
        write!(w, " = ")?;
    }

    let opcode = func.dfg.insts[inst].opcode();

    write!(w, "{}", opcode)?;

    write_operands(w, &func.dfg, inst)?;
    writeln!(w)?;

    Ok(())
}

/// Write the operands of `inst` to `w` with a prepended space.
pub fn write_operands(w: &mut dyn Write, dfg: &DataFlowGraph, inst: Inst) -> fmt::Result {
    let pool = &dfg.insts.value_lists;
    match dfg.insts[inst].clone() {
        InstructionData::Unary { arg, .. } => write!(w, " {}", arg),
        InstructionData::Binary { args, .. } => write!(w, " {}, {}", args[0], args[1]),
        InstructionData::Jump { destination, .. } => {
            write!(w, " {}", destination)
        }
        InstructionData::Branch { then_dst, else_dst, cond, loop_entry, .. } => {
            let tag = if loop_entry { "[loop]" } else { "" };
            write!(w, " {}, {}{}, {}", cond, then_dst, tag, else_dst)
        }
        InstructionData::Call { func_ref, ref args, .. } => {
            write!(w, " {}({})", func_ref, DisplayValues(args.as_slice(pool)))
        }
        InstructionData::PhiNode(PhiNode { args, blocks }) => {
            let mut first = true;
            let args = args.as_slice(&dfg.insts.value_lists);
            for (block, i) in blocks.iter(&dfg.phi_forest) {
                if first {
                    first = false;
                } else {
                    write!(w, ",")?;
                }

                write!(w, " ")?;
                write!(w, "[")?;
                write!(w, "{}, {}", args[i as usize], block)?;
                write!(w, "]")?;
            }
            Ok(())
        }
    }
}

// /// Write block args using optional parentheses.
// fn write_block_args(w: &mut dyn Write, args: &[Value]) -> fmt::Result {
//     if args.is_empty() {
//         Ok(())
//     } else {
//         write!(w, "({})", DisplayValues(args))
//     }
// }

/// Displayable slice of values.
struct DisplayValues<'a>(&'a [Value]);

impl<'a> fmt::Display for DisplayValues<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, val) in self.0.iter().enumerate() {
            if i == 0 {
                write!(f, "{}", val)?;
            } else {
                write!(f, ", {}", val)?;
            }
        }
        Ok(())
    }
}

struct DisplayValuesWithDelimiter<'a>(&'a [Value], char);

impl<'a> fmt::Display for DisplayValuesWithDelimiter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, val) in self.0.iter().enumerate() {
            if i == 0 {
                write!(f, "{}", val)?;
            } else {
                write!(f, "{}{}", self.1, val)?;
            }
        }
        Ok(())
    }
}

/// A lasso resolver that always returns `"<DUMMY>"`.
/// Mainly useful for debugging
pub struct DummyResolver;

impl lasso::Resolver for DummyResolver {
    fn resolve<'a>(&'a self, _key: &lasso::Spur) -> &'a str {
        "<DUMMY>"
    }

    fn try_resolve<'a>(&'a self, _key: &lasso::Spur) -> Option<&'a str> {
        Some("<DUMMY>")
    }

    unsafe fn resolve_unchecked<'a>(&'a self, _key: &lasso::Spur) -> &'a str {
        "<DUMMY>"
    }

    fn contains_key(&self, _key: &lasso::Spur) -> bool {
        true
    }

    fn len(&self) -> usize {
        0
    }
}
