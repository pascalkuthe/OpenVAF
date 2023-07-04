use core::fmt;
use std::fmt::Write;

use super::Body;
use crate::db::HirDefDB;
use crate::expr::CaseCond;
use crate::nameres::DefMapSource;
use crate::{Expr, ExprId, Lookup, Stmt, StmtId};

macro_rules! wln {
    ($dst:expr) => {
        { let _ = writeln!($dst); }
    };
    ($dst:expr, $($arg:tt)*) => {
        { let _ = writeln!($dst, $($arg)*); }
    };
}

macro_rules! w {
    ($dst:expr) => {
        { let _ = write!($dst); }
    };
    ($dst:expr, $($arg:tt)*) => {
        { let _ = write!($dst, $($arg)*); }
    };
}

impl Body {
    pub fn dump(&self, db: &dyn HirDefDB) -> String {
        let mut printer =
            Printer { body: self, db, buf: String::new(), indent_level: 0, needs_indent: true };

        for stmt in &*self.entry_stmts {
            w!(&mut printer, "analog ");
            printer.pretty_print_stmt(*stmt)
        }

        printer.buf
    }
}

struct Printer<'a> {
    body: &'a Body,
    db: &'a dyn HirDefDB,
    buf: String,
    indent_level: usize,
    needs_indent: bool,
}

impl Printer<'_> {
    fn indented(&mut self, f: impl FnOnce(&mut Self)) {
        self.indent_level += 1;
        wln!(self);
        f(self);
        self.indent_level -= 1;
        self.buf = self.buf.trim_end_matches('\n').to_string();
    }

    pub fn pretty_print_stmt(&mut self, s: StmtId) {
        match self.body.stmts[s] {
            Stmt::Missing => wln!(self, "<missing>;"),
            Stmt::Empty => wln!(self, ";"),
            Stmt::Expr(e) => {
                self.pretty_print_expr(e);
                wln!(self, ";");
            }
            Stmt::EventControl { ref event, body } => {
                wln!(self, "@({:?})", event);
                self.pretty_print_stmt(body)
            }
            Stmt::Assignment { dst, val, assignment_kind } => {
                self.pretty_print_expr(dst);
                w!(self, "{:?}", assignment_kind);
                self.pretty_print_expr(val);
                wln!(self, ";");
            }
            Stmt::Block { ref body } => {
                w!(self, "begin");
                if let Some(first) = body.iter().next() {
                    if let DefMapSource::Block(block) = self.body.stmt_scopes[*first].src {
                        let parent = block.lookup(self.db).parent;
                        w!(self, ": {:?} ({:?})", block, parent);
                    } else {
                        w!(self, ": ({:?})", self.body.stmt_scopes[s].src);
                    }
                }

                wln!(self);
                self.indented(|sel| {
                    for stmt in body {
                        sel.pretty_print_stmt(*stmt)
                    }
                });
                wln!(self, "end");
            }
            Stmt::If { cond, then_branch, else_branch } => {
                w!(self, "if ");
                self.pretty_print_expr(cond);
                wln!(self);
                self.pretty_print_stmt(then_branch);
                wln!(self, "else");
                self.pretty_print_stmt(else_branch)
            }
            Stmt::ForLoop { init, cond, incr, body } => {
                w!(self, "for(");
                self.indented(|sel| {
                    sel.pretty_print_stmt(init);
                    sel.pretty_print_expr(cond);
                    wln!(sel, ";");
                    sel.pretty_print_stmt(incr);
                });
                wln!(self, ")");
                self.pretty_print_stmt(body)
            }
            Stmt::WhileLoop { cond, body } => {
                w!(self, "while(");
                self.pretty_print_expr(cond);
                wln!(self, ")");
                self.indented(|sel| sel.pretty_print_stmt(body))
            }
            Stmt::Case { discr, ref case_arms } => {
                w!(self, "case(");
                self.pretty_print_expr(discr);
                self.indented(|sel| {
                    for case in case_arms {
                        match case.cond {
                            CaseCond::Default => w!(sel, "default"),
                            CaseCond::Vals(ref vals) => {
                                for val in vals {
                                    sel.pretty_print_expr(*val);
                                    wln!(sel, ", ")
                                }
                            }
                        }
                        w!(sel, ":");
                        sel.pretty_print_stmt(case.body)
                    }
                });
                wln!(self, "endcase");
            }
        }
    }
    pub fn pretty_print_expr(&mut self, e: ExprId) {
        match self.body.exprs[e] {
            Expr::Missing => w!(self, "<missing>"),
            Expr::Path { ref path, port: false } => w!(self, "{:?}", path),
            Expr::Path { ref path, port: true } => w!(self, "<{:?}>", path),
            Expr::BinaryOp { lhs, rhs, op } => {
                self.pretty_print_expr(lhs);
                match op {
                    Some(op) => w!(self, " {} ", op),
                    None => w!(self, " <ivalid> "),
                }
                self.pretty_print_expr(rhs)
            }
            Expr::UnaryOp { expr, op } => {
                w!(self, "{}", op);
                self.pretty_print_expr(expr)
            }
            Expr::Select { cond, then_val, else_val } => {
                self.pretty_print_expr(cond);
                w!(self, "?");
                self.pretty_print_expr(then_val);
                w!(self, ":");
                self.pretty_print_expr(else_val);
            }
            Expr::Call { ref fun, ref args } => {
                match fun {
                    Some(path) => w!(self, "{:?}", path),
                    None => w!(self, "<missing>"),
                }
                w!(self, "(");
                for arg in args {
                    self.pretty_print_expr(*arg);
                    w!(self, ", ");
                }
                w!(self, ")");
            }
            Expr::Array(ref vals) => {
                w!(self, "'{{");
                for val in vals {
                    self.pretty_print_expr(*val)
                }
                w!(self, "}}");
            }
            Expr::Literal(ref lit) => w!(self, "{:?}", lit),
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
