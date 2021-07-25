/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::cfg::{BasicBlockData, ControlFlowGraph};
use crate::{CfgFunctions, LocalKind, Mir, ModuleId, VariableLocalKind};
use diagnostics::ListPrettyPrinter;
use std::io;
use std::path::Path;

const INDENT: &str = "    ";
/// Alignment for lining up comments following MIR statements
const ALIGN: usize = 120;

impl<C: CfgFunctions> Mir<C> {
    pub fn print_header<W: io::Write>(&self, mut w: W) -> io::Result<()> {
        writeln!(w, "OpenVAF MIR print; Intended for human consumption only (no parsing)")?;
        writeln!(w, "NETS")?;
        for (net, info) in self.nodes.iter_enumerated() {
            writeln!(w, "{}{:?} = {:?}", INDENT, net, info)?;
        }

        writeln!(w)?;

        writeln!(w, "BRANCHES")?;
        for (branch, info) in self.branches.iter_enumerated() {
            writeln!(w, "{}{:?} = {:?}", INDENT, branch, info)?;
        }

        writeln!(w)?;

        writeln!(w, "PARMETERS")?;
        for (param, info) in self.parameters.iter_enumerated() {
            // TODO print default value cfg
            info.ty.with_info(|ty| {
                writeln!(w, "{}param {}: {} = {};", INDENT, param, ty, info.ident)
            })?;
        }

        Ok(())
    }

    pub fn print_modules<W: io::Write>(&self, mut w: W) -> io::Result<()> {
        for module in self.modules.iter() {
            writeln!(w, "Module {} ", module.ident)?;
            for port in module.ports.clone() {
                writeln!(w, "{}PORTS", INDENT)?;
                let directions = match (self.ports[port].output, self.ports[port].input) {
                    (true, true) => "inout",
                    (true, false) => "out",
                    (false, true) => "input",
                    _ => "ILLEGAL",
                };

                writeln!(
                    w,
                    "{0}{0}port {1}: {2} = {3:?};",
                    INDENT, port, directions, self.ports[port].node
                )?;
            }

            writeln!(w)?;

            module.analog_cfg.borrow().print(self, &mut w)?;
        }

        Ok(())
    }

    pub fn print_modules_with_shared<W: io::Write, X: CfgFunctions>(
        &self,
        mut w: W,
        id: ModuleId,
        cfg: &ControlFlowGraph<X>,
    ) -> io::Result<()> {
        for (module, info) in self.modules.iter_enumerated() {
            writeln!(w, "Module {} ", info.ident)?;
            for port in info.ports.clone() {
                writeln!(w, "{}PORTS", INDENT)?;
                let directions = match (self.ports[port].output, self.ports[port].input) {
                    (true, true) => "inout",
                    (true, false) => "out",
                    (false, true) => "input",
                    _ => "ILLEGAL",
                };

                writeln!(
                    w,
                    "{0}{0}port {1}: {2} = {3:?};",
                    INDENT, port, directions, self.ports[port].node
                )?;
            }

            writeln!(w)?;

            if module == id {
                cfg.print(self, &mut w)?;
            } else {
                info.analog_cfg.borrow().print(self, &mut w)?;
            }
        }

        Ok(())
    }

    pub fn print_to_file(&self, path: impl AsRef<Path>) -> io::Result<()> {
        let file = std::fs::File::create(path.as_ref())?;
        self.print(file)
    }

    pub fn print_to_file_with_shared<X: CfgFunctions>(
        &self,
        path: impl AsRef<Path>,
        id: ModuleId,
        cfg: &ControlFlowGraph<X>,
    ) -> io::Result<()> {
        let file = std::fs::File::create(path.as_ref())?;
        self.print_with_shared(file, id, cfg)
    }

    pub fn print(&self, mut dst: impl io::Write) -> io::Result<()> {
        self.print_header(&mut dst)?;
        writeln!(dst)?;
        self.print_modules(&mut dst)
    }

    pub fn print_with_shared<X: CfgFunctions>(
        &self,
        mut dst: impl io::Write,
        id: ModuleId,
        cfg: &ControlFlowGraph<X>,
    ) -> io::Result<()> {
        self.print_header(&mut dst)?;
        writeln!(dst)?;
        self.print_modules_with_shared(&mut dst, id, cfg)
    }
}
impl<C: CfgFunctions> ControlFlowGraph<C> {
    pub fn print<A: CfgFunctions, W: io::Write>(&self, mir: &Mir<A>, mut w: W) -> io::Result<()> {
        writeln!(w, "LOCALS")?;
        for (local, decl) in self.locals.iter_enumerated() {
            let (decl, comment) = match decl.kind {
                LocalKind::Temporary => {
                    decl.ty
                        .with_info(|ty| writeln!(w, "{0}{0}let {1}: {2};", INDENT, local, ty))?;
                    continue;
                }
                LocalKind::Variable(var, VariableLocalKind::User) => (
                    decl.ty.with_info(|ty| format!("{0}{0}let mut {1}: {2};", INDENT, local, ty)),
                    format!("Corresponds to {} ({:?})", mir.variables[var].ident, var),
                ),
                LocalKind::Variable(var, VariableLocalKind::Derivative(ref unkowns)) => {
                    let unkowns =
                        ListPrettyPrinter { list: unkowns.as_slice(), prefix: "d/d", postfix: " " };

                    (
                        decl.ty
                            .with_info(|ty| format!("{0}{0}let mut {1}: {2};", INDENT, local, ty)),
                        format!(
                            "Corresponds to {} {} ({:?})",
                            unkowns, mir.variables[var].ident, var
                        ),
                    )
                }

                LocalKind::Branch(access, branch, VariableLocalKind::User) => (
                    decl.ty.with_info(|ty| format!("{0}{0}let mut {1}: {2};", INDENT, local, ty)),
                    format!(
                        "Corresponds to {}({}) {:?}",
                        access, mir.branches[branch].ident, branch
                    ),
                ),
                LocalKind::Branch(access, branch, VariableLocalKind::Derivative(ref unkowns)) => {
                    let unkowns =
                        ListPrettyPrinter { list: unkowns.as_slice(), prefix: "d/d", postfix: " " };

                    (
                        decl.ty
                            .with_info(|ty| format!("{0}{0}let mut {1}: {2};", INDENT, local, ty)),
                        format!(
                            "Corresponds to a {} {}({}), {:?}",
                            unkowns, access, mir.branches[branch].ident, branch
                        ),
                    )
                }
            };

            writeln!(w, "{:A$} // {}", decl, comment, A = ALIGN,)?;
        }
        writeln!(w)?;

        for (bb, body) in self.blocks.iter_enumerated() {
            writeln!(w, "{}{:?}:", INDENT, bb)?;
            body.print(&mut w)?;
            writeln!(w)?;
        }
        Ok(())
    }
}

impl<C: CfgFunctions> BasicBlockData<C> {
    pub fn print<W: io::Write>(&self, mut w: W) -> io::Result<()> {
        //writeln!(w, "{}{:?}: {{", INDENT, block, )?;
        for (phi, info) in self.phi_statements.iter_enumerated() {
            let content = format!("{0}{0}{1};", INDENT, info);
            writeln!(w, "{:A$} // {:?}", content, phi, A = ALIGN,)?;
        }
        for (stmnt, (info, _)) in self.statements.iter_enumerated() {
            let content = format!("{0}{0}{1};", INDENT, info);
            writeln!(w, "{:A$} // {:?}", content, stmnt, A = ALIGN,)?;
        }

        match &self.terminator {
            Some(term) if term.kind.is_loop_head() => {
                let content = format!("{0}{0}{1};", INDENT, term.kind);
                writeln!(w, "{:A$} // loop condition", content, A = ALIGN,)
            }
            Some(term) => writeln!(w, "{0}{0}{1};", INDENT, term.kind),

            None => writeln!(w, "{0}{0} MISSING TERMINATOR", INDENT),
        }
    }
}
