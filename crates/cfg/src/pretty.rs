use std::fmt::{self, Write};

use lasso::MiniSpur;

use crate::{BasicBlockData, Const, ControlFlowGraph, Instruction, Operand, Terminator};

impl ControlFlowGraph {
    pub fn print(&self, lit: &lasso::Rodeo<MiniSpur>) -> String {
        let mut print =
            Printer { cfg: self, lit, buf: String::new(), indent_level: 0, needs_indent: true };
        print.print();
        print.buf
    }
}

macro_rules! wln {
    ($dst:expr) => {
        { let _ = writeln!($dst); }
    };
    ($dst:expr, $($arg:tt)*) => {
        { let _ = writeln!($dst, $($arg)*); }
    };
}

macro_rules! w {
    ($dst:expr, $($arg:tt)*) => {
        { let _ = write!($dst, $($arg)*); }
    };
}

struct Printer<'a> {
    cfg: &'a ControlFlowGraph,
    lit: &'a lasso::Rodeo<MiniSpur>,
    buf: String,
    indent_level: usize,
    needs_indent: bool,
}

impl<'a> Printer<'a> {
    fn indented(&mut self, f: impl FnOnce(&mut Self)) {
        self.indent_level += 1;
        wln!(self);
        f(self);
        self.indent_level -= 1;
        self.buf = self.buf.trim_end_matches('\n').to_string();
    }

    pub fn print(&mut self) {
        wln!(self, "{{");
        wln!(self, "mut {:?};", self.cfg.places.raw);
        wln!(self, "next_local {:?};", self.cfg.next_local);
        for (id, bb) in self.cfg.blocks.iter_enumerated() {
            wln!(self, "{:?}:", id);
            self.indented(|s| s.print_bb(bb))
        }
        w!(self, "}}")
    }

    pub fn print_bb(&mut self, bb: &BasicBlockData) {
        for phi in &bb.phis {
            wln!(self, "phi {:?};", phi)
        }

        for instr in &bb.instructions {
            self.print_instr(instr)
        }
        if let Some(terminator) = &bb.terminator {
            self.print_terminator(terminator);
        }
        wln!(self)
    }

    pub fn print_instr(&mut self, instr: &Instruction) {
        if let Some(dst) = instr.dst {
            w!(self, "let {:?} := ", dst)
        }

        if instr.args.is_empty() {
            wln!(self, "{:?};", instr.op);
        } else {
            w!(self, "{:?} [", instr.op);
            self.print_operand(&instr.args[0]);
            for arg in &instr.args[1..] {
                w!(self, ", ");
                self.print_operand(arg)
            }
            wln!(self, "];");
        }
    }

    pub fn print_terminator(&mut self, term: &Terminator) {
        if let Terminator::Split { condition, true_block, false_block, loop_head } = term {
            w!(self, "if ");
            self.print_operand(condition);
            w!(
                self,
                " {{ {:?} }} else {{ {:?} }} {}",
                true_block,
                false_block,
                if *loop_head { "(loop)" } else { "" }
            )
        } else {
            w!(self, "{:?}", &term)
        }
    }

    fn print_operand(&mut self, op: &Operand) {
        if let Operand::Const(constant) = op {
            self.print_constant(constant)
        } else {
            w!(self, "{:?}", op)
        }
    }

    pub fn print_constant(&mut self, constant: &Const) {
        match constant {
            Const::String(val) => w!(self, "str {:?}", self.lit.resolve(val)),
            Const::StringArray(data) => {
                let slice: Vec<_> = data.slice.iter().map(|s| self.lit.resolve(s)).collect();
                w!(self, "str[] {:?}", slice)
            }
            _ => w!(self, "{:?}", constant),
        }
    }
}

impl<'a> Write for Printer<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for line in s.split_inclusive('\n') {
            if self.needs_indent {
                match self.buf.chars().last() {
                    Some('\n') | None => {}
                    _ => self.buf.push('\n'),
                }

                if line != "\n" {
                    // don't indent empty lines! required to play nice with expect_test
                    self.buf.push_str(&"    ".repeat(self.indent_level));
                }
                self.needs_indent = false;
            }

            self.buf.push_str(line);
            self.needs_indent = line.ends_with('\n');
        }

        Ok(())
    }
}
