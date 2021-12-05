use std::fmt::{self, Write};

use super::ScopeDefItem;
use crate::db::HirDefDB;
use crate::nameres::{DefMap, LocalScopeId};

impl DefMap {
    pub fn dump(&self, db: &dyn HirDefDB) -> String {
        let mut printer = Printer { db, buf: String::new(), indent_level: 0, needs_indent: true };
        printer.print_def_map_root(self);
        printer.buf.push('\n');
        printer.buf
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

// macro_rules! w {
//     ($dst:expr) => {
//         { let _ = write!($dst); }
//     };
//     ($dst:expr, $($arg:tt)*) => {
//         { let _ = write!($dst, $($arg)*); }
//     };
// }

struct Printer<'a> {
    db: &'a dyn HirDefDB,
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

    fn print_def_map_root(&mut self, map: &DefMap) {
        self.print_scope(map, map.root())
    }

    fn print_def_map(&mut self, map: &DefMap) {
        self.print_scope(map, map.entry())
    }

    fn print_scope(&mut self, map: &DefMap, local_scope: LocalScopeId) {
        let mut declarations: Vec<_> = map.scopes[local_scope]
            .declarations
            .iter()
            .map(|(name, def)| (name.clone(), *def))
            .collect();
        declarations.sort_unstable_by_key(|(name, _)| name.clone());
        for (name, def) in declarations {
            wln!(self, "{} = {};", name, def.item_kind());

            match def {
                ScopeDefItem::BlockId(block) => {
                    if let Some(def_map) = self.db.block_def_map(block) {
                        self.indented(|s| s.print_def_map(&def_map));
                    }
                }
                ScopeDefItem::FunctionId(fun) => {
                    let def_map = self.db.function_def_map(fun);
                    self.indented(|s| s.print_def_map(&def_map));
                }

                _ => {
                    if let Some(child) = map.scopes[local_scope].children.get(&name) {
                        self.indented(|s| s.print_scope(map, *child))
                    }
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
