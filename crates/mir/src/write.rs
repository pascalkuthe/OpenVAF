//! Converting MIR to text.
//!
//! The `write` module provides the `write_function` function which converts an IR `Function` to an
//! equivalent textual form. This textual form can be read back by the `mir-reader` crate.

#[cfg(test)]
mod tests;

use core::fmt::{self, Write};
use cranelift_entity::SecondaryMap;

use crate::entities::AnyEntity;
use crate::{Block, DataFlowGraph, Function, Inst, Value};

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
        aliases: &SecondaryMap<Value, Vec<Value>>,
        inst: Inst,
        indent: usize,
        interner: &dyn lasso::Resolver,
    ) -> fmt::Result;

    /// Write the preamble to `w`. By default, this uses `write_entity_definition`.
    fn write_preamble(&mut self, w: &mut dyn Write, func: &Function) -> Result<bool, fmt::Error> {
        self.super_preamble(w, func)
    }

    /// Default impl of `write_preamble`
    fn super_preamble(&mut self, w: &mut dyn Write, func: &Function) -> Result<bool, fmt::Error> {
        let mut any = false;

        // Write out all signatures before functions since function declarations can refer to
        // signatures.
        for (sig, sig_data) in &func.dfg.signatures {
            any = true;
            self.write_entity_definition(w, func, sig.into(), &sig_data)?;
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
        aliases: &SecondaryMap<Value, Vec<Value>>,
        inst: Inst,
        indent: usize,
        interner: &dyn lasso::Resolver,
    ) -> fmt::Result {
        write_instruction(w, func, aliases, inst, indent, interner)
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

/// Create a reverse-alias map from a value to all aliases having that value as a direct target
fn alias_map(func: &Function) -> SecondaryMap<Value, Vec<Value>> {
    let mut aliases = SecondaryMap::<_, Vec<_>>::new();
    for v in func.dfg.values() {
        // VADFS returns the immediate target of an alias
        if let Some(k) = func.dfg.value_alias_dest_for_serialization(v) {
            aliases[k].push(v);
        }
    }
    aliases
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
    write_spec(w, func)?;
    writeln!(w, " {{")?;
    let aliases = alias_map(func);
    let mut any = func_w.write_preamble(w, func)?;
    for block in &func.layout {
        if any {
            writeln!(w)?;
        }
        decorate_block(func_w, w, func, &aliases, block, interner)?;
        any = true;
    }
    writeln!(w, "}}")
}

//----------------------------------------------------------------------
//
// Function spec.

fn write_spec(w: &mut dyn Write, func: &Function) -> fmt::Result {
    write!(w, "%{}", func.name)
}

//----------------------------------------------------------------------
//
// Basic blocks

fn write_arg(w: &mut dyn Write, arg: Value) -> fmt::Result {
    write!(w, "{}", arg)
}

/// Write out the basic block header, outdented:
///
///    block1:
///    block1(v1: i32):
///    block10(v4: f64, v5: b1):
///
pub fn write_block_header(
    w: &mut dyn Write,
    func: &Function,
    block: Block,
    indent: usize,
) -> fmt::Result {
    // The `indent` is the instruction indentation. block headers are 4 spaces out from that.
    write!(w, "{1:0$}{2}", indent - 4, "", block)?;

    let mut args = func.dfg.block_params(block).iter().cloned();
    match args.next() {
        None => return writeln!(w, ":"),
        Some(arg) => {
            write!(w, "(")?;
            write_arg(w, arg)?;
        }
    }
    // Remaining arguments.
    for arg in args {
        write!(w, ", ")?;
        write_arg(w, arg)?;
    }
    writeln!(w, "):")
}

fn decorate_block<FW: FuncWriter>(
    func_w: &mut FW,
    w: &mut dyn Write,
    func: &Function,
    aliases: &SecondaryMap<Value, Vec<Value>>,
    block: Block,
    interner: &dyn lasso::Resolver,
) -> fmt::Result {
    // Indent all instructions if any srclocs are present.
    let indent = if func.srclocs.is_empty() { 4 } else { 36 };

    func_w.write_block_header(w, func, block, indent)?;
    for a in func.dfg.block_params(block).iter().cloned() {
        write_value_aliases(w, aliases, a, indent)?;
    }

    for inst in func.layout.block_insts(block) {
        func_w.write_instruction(w, func, aliases, inst, indent, interner)?;
    }

    Ok(())
}

//----------------------------------------------------------------------
//
// Instructions

/// Write out any aliases to the given target, including indirect aliases
fn write_value_aliases(
    w: &mut dyn Write,
    aliases: &SecondaryMap<Value, Vec<Value>>,
    target: Value,
    indent: usize,
) -> fmt::Result {
    let mut todo_stack = vec![target];
    while let Some(target) = todo_stack.pop() {
        for &a in &aliases[target] {
            writeln!(w, "{1:0$}{2} -> {3}", indent, "", a, target)?;
            todo_stack.push(a);
        }
    }

    Ok(())
}

fn write_instruction(
    w: &mut dyn Write,
    func: &Function,
    aliases: &SecondaryMap<Value, Vec<Value>>,
    inst: Inst,
    indent: usize,
    interner: &dyn lasso::Resolver,
) -> fmt::Result {
    // Prefix containing source location, encoding, and value locations.
    let mut s = String::with_capacity(16);

    // Source location goes first.
    let srcloc = func.srclocs[inst];
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

    let opcode = func.dfg[inst].opcode();

    write!(w, "{}", opcode)?;

    write_operands(w, &func.dfg, inst, interner)?;
    writeln!(w)?;

    // Value aliases come out on lines after the instruction defining the referent.
    for r in func.dfg.inst_results(inst) {
        write_value_aliases(w, aliases, *r, indent)?;
    }
    Ok(())
}

/// Write the operands of `inst` to `w` with a prepended space.
pub fn write_operands(
    w: &mut dyn Write,
    dfg: &DataFlowGraph,
    inst: Inst,
    interner: &dyn lasso::Resolver,
) -> fmt::Result {
    let pool = &dfg.value_lists;
    use crate::instructions::InstructionData::*;
    match dfg[inst] {
        Unary { arg, .. } => write!(w, " {}", arg),
        UnaryInt { imm } => write!(w, " {}", imm),
        UnaryIeee64 { imm } => write!(w, " {}", imm),
        UnaryBool { imm } => write!(w, " {}", imm),
        UnaryStr { imm } => write!(w, " {:?}", interner.resolve(&imm)),
        Binary { args, .. } => write!(w, " {}, {}", args[0], args[1]),
        Jump { destination, ref args, .. } => {
            write!(w, " {}", destination)?;
            write_block_args(w, args.as_slice(pool))
        }
        Branch { destination, ref args, .. } => {
            let args = args.as_slice(pool);
            write!(w, " {}, {}", args[0], destination)?;
            write_block_args(w, &args[1..])
        }
        Call { func_ref, ref args, .. } => {
            write!(w, " {}({})", func_ref, DisplayValues(args.as_slice(pool)))
        }
    }
}

/// Write block args using optional parantheses.
fn write_block_args(w: &mut dyn Write, args: &[Value]) -> fmt::Result {
    if args.is_empty() {
        Ok(())
    } else {
        write!(w, "({})", DisplayValues(args))
    }
}

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
