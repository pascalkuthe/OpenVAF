use std::{
    fmt::{self, Write},
    iter::repeat,
};

use stdx::iter::zip;

use crate::ItemTree;

use super::{BlockScopeItem, Discipline, Function, Module, Nature};

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

impl ItemTree {
    pub fn dump(&self) -> String {
        let mut printer =
            Printer { tree: self, buf: String::new(), indent_level: 0, needs_indent: true };
        printer.print();
        printer.buf
    }
}

struct Printer<'a> {
    tree: &'a ItemTree,
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

    fn print(&mut self) {
        for nature in &self.tree.data.natures {
            w!(self, "nature {}", nature.name);
            self.indented(|s| s.print_nature_attrs(nature))
        }

        for discipline in &self.tree.data.disciplines {
            wln!(self, "discipline {}", discipline.name);
            self.indented(|s| s.print_discipline(discipline))
        }

        for module in &self.tree.data.modules {
            wln!(self, "module {}", module.name);
            self.indented(|s| s.print_module(module))
        }
    }

    fn print_nature_attrs(&mut self, nature: &Nature) {
        wln!(self, "parent = {:?}", nature.parent);
        wln!(self, "units = {:?}", nature.units);
        wln!(self, "ddt_nature = {:?}", nature.ddt_nature);
        wln!(self, "idt_nature = {:?}", nature.idt_nature);
        wln!(self, "access = {:?}", nature.access);
        for attr in nature.attrs.clone() {
            wln!(self, "{}", self.tree[attr].name)
        }
    }

    fn print_discipline(&mut self, discipline: &Discipline) {
        wln!(self, "potential = {:?}", discipline.potential);
        wln!(self, "flow = {:?}", discipline.flow);
        wln!(self, "domain = {:?}", discipline.domain);
        for attr in discipline.attrs.clone() {
            wln!(self, "{}", self.tree[attr].name)
        }
    }

    fn print_module(&mut self, module: &Module) {
        wln!(self, "expected_ports = {:?}", module.exptected_ports);
        for (port, port_kind) in zip(module.head_ports.clone(), repeat("head"))
            .chain(zip(module.body_ports.clone(), repeat("body")))
        {
            let port = &self.tree[port];
            wln!(
                self,
                "port ({}) {} = {{is_input: {}, is_output:{}, gnd: {} , discipline {:?}}}",
                port_kind,
                port.name,
                port.is_input,
                port.is_output,
                port.is_gnd,
                port.discipline,
            );
        }

        for net in module.nets.clone() {
            let net = &self.tree[net];
            wln!(
                self,
                "net {} = {{gnd: {} , discipline {:?}}}",
                net.name,
                net.is_gnd,
                net.discipline,
            );
        }

        for branch in module.branches.clone() {
            let branch = &self.tree[branch];
            wln!(self, "branch {} = {:?}", branch.name, branch.kind)
        }

        for function in module.functions.clone() {
            let function = &self.tree[function];
            wln!(self, "function {}", function.name);
            self.indented(|s| s.print_function(function))
        }
        self.print_scope_items(&module.scope_items)
    }

    fn print_function(&mut self, function: &Function) {
        for arg in function.args.clone() {
            let arg = &self.tree[arg];
            wln!(
                self,
                "arg {} = {{ is_input = {}, is_output = {}}}",
                arg.name,
                arg.is_input,
                arg.is_output
            );
        }

        self.print_scope_items(&function.scope_items)
    }

    fn print_scope_items(&mut self, items: &[BlockScopeItem]) {
        for item in items {
            match *item {
                BlockScopeItem::Scope(block) => {
                    let block = self.tree.block_scope(block);
                    wln!(self, "block {:?}", block.name);
                    self.indented(|s| s.print_scope_items(&block.scope_items));
                }
                BlockScopeItem::Parameter(param) => {
                    let param = &self.tree[param];
                    wln!(self, "param {} {}", param.ty, param.name);
                }
                BlockScopeItem::Variable(var) => {
                    let var = &self.tree[var];
                    wln!(self, "var {} {}", var.ty, var.name);
                }
            }
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
