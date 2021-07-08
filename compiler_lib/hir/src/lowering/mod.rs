/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub use cfg_builder::LocalCtx;
pub use errors::{Error, TyPrinter};

use errors::Error::{ExpectedLintName, TypeMissmatch};
use lints::{EmptyBuiltinAttribute, LintLevelOverwrite, UnknownLint};

use crate::{BranchKind, Hir};

use openvaf_data_structures::{bit_set::BitSet, index_vec::IndexVec, sync::RwLock, HashMap};

use openvaf_diagnostics::lints::{Lint, LintLevel, Linter};
use openvaf_diagnostics::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};

use openvaf_ir::ids::{
    BranchId, DisciplineId, ExpressionId, ModuleId, NatureId, NodeId, ParameterId, StatementId,
    SyntaxCtx, VariableId,
};
use openvaf_ir::{
    Attribute, ParameterExcludeConstraint, ParameterRangeConstraint, ParameterRangeConstraintBound,
};

use openvaf_middle::osdi_types::{
    ConstVal::Scalar,
    SimpleConstVal::{Integer, Real, String},
};
use openvaf_middle::{
    Branch, ConstVal, Expression, Mir, Module, Nature, Operand, Parameter, ParameterConstraint,
    SyntaxContextData, TryDefaultConversion, Type, Variable,
};

use openvaf_session::{
    sourcemap::StringLiteral,
    symbols::{kw, sym},
};

use crate::lowering::errors::Error::{
    SimultaneousVoltageAndCurrentContribute, SimultaneousVoltageAndCurrentProbe,
};
use openvaf_middle::functions::ParameterCallType;
use std::convert::TryInto;
use std::mem::take;
use tracing::trace_span;

mod cfg_builder;
pub mod errors;
pub mod lints;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AttributeCtx {
    Variable(VariableId),
    Parameter(ParameterId),
    Node(NodeId),
    Branch(BranchId),
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
    Syntactic,
}

pub trait HirLowering: Sized {
    fn handle_attribute(
        ctx: &mut HirFold<Self>,
        attr: &Attribute,
        src: AttributeCtx,
        sctx: SyntaxCtx,
    );

    fn handle_statement_attribute<'a, 'h>(
        ctx: &mut LocalCtx<'a, 'h, Self>,
        attr: &Attribute,
        stmt: StatementId,
        sctx: SyntaxCtx,
    );
}

pub struct HirFold<'a, L: HirLowering> {
    pub lowering: &'a mut L,
    pub hir: &'a Hir,
    pub mir: Mir,
    pub errors: MultiDiagnostic<Error>,
    pub sctx: SyntaxCtx,
    pub lower_sctx: BitSet<SyntaxCtx>,
}

impl<'h, L: HirLowering> HirFold<'h, L> {
    pub fn handle_attributes_with(
        &mut self,
        sctx: SyntaxCtx,
        src: AttributeCtx,
        mut handle_attribute: impl FnMut(&mut Self, &Attribute) -> bool,
    ) {
        if !self.lower_sctx.remove(sctx) {
            return;
        }

        for attr in self.hir[sctx].attributes.into_iter() {
            let attr = &self.hir[attr];
            if !(self.handle_builtin_attribute(attr, sctx) || handle_attribute(self, attr)) {
                L::handle_attribute(self, attr, src, sctx)
            }
        }
    }

    pub fn handle_attributes(&mut self, sctx: SyntaxCtx, src: AttributeCtx) {
        let span = trace_span!("attributes", sctx = sctx.index(), src = debug(src));
        let _enter = span.enter();
        self.handle_attributes_with(sctx, src, |_, _| false)
    }

    pub fn handle_builtin_attribute(&mut self, attr: &Attribute, sctx: SyntaxCtx) -> bool {
        let lvl = match attr.ident.name {
            sym::openvaf_allow => LintLevel::Allow,
            sym::openvaf_warn => LintLevel::Warn,
            sym::openvaf_deny => LintLevel::Deny,
            sym::openvaf_forbid => LintLevel::Forbid,
            _ => return false,
        };

        let names = if let Some(expr) = attr.value {
            match self.hir[expr].contents {
                crate::Expression::Constant(Scalar(String(val))) => {
                    vec![(val, self.hir[expr].span)]
                }
                crate::Expression::Array(ref expressions) => {
                    let res = expressions
                        .iter()
                        .filter_map(|expr| {
                            if let crate::Expression::Constant(Scalar(String(val))) =
                                self.hir[*expr].contents
                            {
                                Some((val, self.hir[*expr].span))
                            } else {
                                self.errors.add(ExpectedLintName {
                                    attr: attr.ident.name,
                                    span: self.hir[*expr].span,
                                });
                                None
                            }
                        })
                        .collect();
                    res
                }

                _ => {
                    self.errors.add(ExpectedLintName {
                        attr: attr.ident.name,
                        span: self.hir[expr].span,
                    });
                    return true;
                }
            }
        } else {
            Linter::dispatch_late(Box::new(EmptyBuiltinAttribute(attr.ident)), sctx);
            return true;
        };

        for (name, span) in names.into_iter() {
            let name = name.unescaped_contents();
            if let Some(lint) = Lint::from_name(&name) {
                if let Some(old) = self.mir.syntax_ctx[sctx].lint_levels.insert(lint, lvl) {
                    Linter::dispatch_late(
                        Box::new(LintLevelOverwrite {
                            span,
                            lint: name,
                            old,
                            new: lvl,
                        }),
                        sctx,
                    );
                }
            } else {
                Linter::dispatch_late(Box::new(UnknownLint { span, lint: name }), sctx);
            }
        }

        true
    }

    fn lower_nature(&mut self, id: NatureId, nature: &crate::Nature) -> Nature {
        let span = trace_span!("nature", id = id.index(), name = display(nature.ident));
        let _enter = span.enter();
        self.sctx = nature.sctx;
        self.handle_attributes(nature.sctx, AttributeCtx::Nature(id));
        let abstol = self.eval_real_constant(nature.abstol).unwrap_or(0.0);
        let units = self
            .eval_string_constant(nature.units)
            .unwrap_or(StringLiteral::DUMMY);

        Nature {
            ident: nature.ident,
            abstol,
            units,
            access: nature.access,
            idt_nature: nature.idt_nature,
            ddt_nature: nature.ddt_nature,
            sctx: nature.sctx,
        }
    }

    fn lower_branch(&mut self, id: BranchId, branch: &crate::Branch) -> Branch {
        let span = trace_span!("branch", id = id.index(), name = display(branch.ident));
        let _enter = span.enter();
        self.sctx = branch.sctx;
        self.handle_attributes(branch.sctx, AttributeCtx::Branch(id));

        if !branch.voltage_contributions.is_empty() && !branch.current_contributions.is_empty() {
            self.errors.add(SimultaneousVoltageAndCurrentContribute {
                branch: branch.ident,
                voltage_contribute: branch.voltage_contributions.clone(),
                current_contribute: branch.current_contributions.clone(),
            })
        }

        if !branch.voltage_access.is_empty() && !branch.current_acccess.is_empty() {
            self.errors.add(SimultaneousVoltageAndCurrentProbe {
                branch: branch.ident,
                voltage_probes: branch.voltage_access.clone(),
                current_probes: branch.current_acccess.clone(),
            })
        }

        Branch {
            ident: branch.ident,
            hi: branch.hi,
            lo: branch.lo,
            sctx: branch.sctx,
            generated: branch.kind != BranchKind::Explicit,
        }
    }

    fn lower_variable(&mut self, id: VariableId, var: &crate::Variable) -> Variable {
        let span = trace_span!("variable", id = id.index(), name = display(var.ident));
        let _enter = span.enter();
        let (unit, desc) =
            self.lower_units_and_desc_attributes(var.sctx, AttributeCtx::Variable(id));
        let default = var
            .default
            .and_then(|e| self.lower_assign_expr(e, var.ty))
            .unwrap_or_else(|| {
                let default = match var.ty {
                    Type::INT => ConstVal::Scalar(Integer(0)),
                    Type::REAL => ConstVal::Scalar(Real(0.0)),
                    Type::STRING => ConstVal::Scalar(String(StringLiteral::DUMMY)),
                    _ => todo!("Array defaults?"),
                };

                Expression::<ParameterCallType>::new_const(default, self.mir[var.sctx].span)
            });

        Variable {
            ident: var.ident,
            variable_type: var.ty,
            default: RwLock::new(default),
            unit,
            desc,
            sctx: var.sctx,
            ty: var.ty,
        }
    }

    fn lower_units_and_desc_attributes(
        &mut self,
        sctx: SyntaxCtx,
        src: AttributeCtx,
    ) -> (Option<StringLiteral>, Option<StringLiteral>) {
        let mut unit = None;
        let mut desc = None;

        self.handle_attributes_with(sctx, src, |fold, attr| match attr.ident.name {
            sym::desc => {
                if let Some(val) = attr.value {
                    desc = fold.eval_string_constant(val)
                } else {
                    Linter::dispatch_late(Box::new(EmptyBuiltinAttribute(attr.ident)), sctx)
                }
                true
            }
            kw::units => {
                if let Some(val) = attr.value {
                    unit = fold.eval_string_constant(val)
                } else {
                    Linter::dispatch_late(Box::new(EmptyBuiltinAttribute(attr.ident)), sctx)
                }
                true
            }
            _ => false,
        });

        (unit, desc)
    }

    fn lower_parameter(&mut self, id: ParameterId, param: &crate::Parameter) -> Option<Parameter> {
        let span = trace_span!("parameter", id = id.index(), name = display(param.ident));
        let _enter = span.enter();

        self.sctx = param.sctx;

        let (unit, desc) =
            self.lower_units_and_desc_attributes(param.sctx, AttributeCtx::Parameter(id));

        let default = self.lower_assign_expr(param.default, param.ty);

        let kind = match &param.constraints {
            crate::ParameterConstraint::Ordered(included, excluded) => {
                let included = included
                    .iter()
                    .filter_map(|b| self.lower_parameter_range_constraint(b, param.ty))
                    .collect();
                let excluded = excluded
                    .iter()
                    .filter_map(|b| self.lower_parameter_exclude_constraint(b, param.ty))
                    .collect();
                ParameterConstraint::Ordered { included, excluded }
            }
            crate::ParameterConstraint::Unordered(included, excluded) => {
                let included = included
                    .iter()
                    .filter_map(|e| self.lower_assign_expr(*e, param.ty))
                    .collect();
                let excluded = excluded
                    .iter()
                    .filter_map(|e| self.lower_assign_expr(*e, param.ty))
                    .collect();
                ParameterConstraint::UnOrdered { included, excluded }
            }
        };

        Some(Parameter {
            ident: param.ident,
            ty: param.ty,
            default: RwLock::new(default?),
            kind: RwLock::new(kind),
            unit,
            sctx: param.sctx,
            desc,
        })
    }

    fn lower_parameter_exclude_constraint(
        &mut self,
        bound: &ParameterExcludeConstraint<ExpressionId>,
        ty: Type,
    ) -> Option<ParameterExcludeConstraint<Expression<ParameterCallType>>> {
        let res = match bound {
            ParameterExcludeConstraint::Value(expr) => {
                let expr = self.lower_assign_expr(*expr, ty)?;
                ParameterExcludeConstraint::Value(expr)
            }
            ParameterExcludeConstraint::Range(ref range) => {
                let range = self.lower_parameter_range_constraint(range, ty)?;
                ParameterExcludeConstraint::Range(range)
            }
        };

        Some(res)
    }

    fn lower_parameter_range_constraint(
        &mut self,
        bound: &ParameterRangeConstraint<ExpressionId>,
        ty: Type,
    ) -> Option<ParameterRangeConstraint<Expression<ParameterCallType>>> {
        let lo = self.lower_parameter_range_constraint_bound(bound.start, ty);
        let hi = self.lower_parameter_range_constraint_bound(bound.end, ty);
        Some(lo?..hi?)
    }

    fn lower_parameter_range_constraint_bound(
        &mut self,
        bound: ParameterRangeConstraintBound<ExpressionId>,
        ty: Type,
    ) -> Option<ParameterRangeConstraintBound<Expression<ParameterCallType>>> {
        Some(ParameterRangeConstraintBound {
            inclusive: bound.inclusive,
            bound: self.lower_assign_expr(bound.bound, ty)?,
        })
    }

    fn lower_module(&mut self, id: ModuleId, module: &crate::Module) -> Module {
        let span = trace_span!("module", id = id.index(), name = display(module.ident));
        let _enter = span.enter();

        self.sctx = module.sctx;
        self.handle_attributes(module.sctx, AttributeCtx::Module(id));

        let mut local_ctx = LocalCtx::new_main(self);
        local_ctx.lower_block(&module.analog);
        debug_assert_eq!(
            local_ctx.cfg_builder.current,
            local_ctx.cfg_builder.cfg.blocks.last_idx()
        );

        Module {
            ident: module.ident,
            ports: module.ports.clone(),
            parameters: module.parameters.clone(),
            analog_cfg: RwLock::new(local_ctx.cfg_builder.finish(self.sctx)),
            sctx: module.sctx,
        }
    }

    pub fn lower_expression(&mut self, expr: ExpressionId) -> Option<Expression> {
        let span = self.hir[expr].span;
        let mut lctx = LocalCtx::new_small(self);
        let expr = lctx.lower_expr(expr)?;
        let operand = lctx.rvalue_to_operand(expr, span);
        let cfg = lctx.cfg_builder.finish(self.sctx);
        Some(Expression(cfg, operand))
    }

    pub fn lower_assign_expr(
        &mut self,
        expr: ExpressionId,
        ty: Type,
    ) -> Option<Expression<ParameterCallType>> {
        let span = self.hir[expr].span;
        let mut lctx = LocalCtx::new_small(self);
        let expr = lctx.lower_assignment_expr(expr, ty)?;
        let operand = lctx.rvalue_to_operand(expr, span);
        let cfg = if lctx
            .cfg_builder
            .cfg
            .blocks
            .iter()
            .all(|x| x.statements.is_empty())
        {
            lctx.cfg_builder.cfg.blocks.clear();
            lctx.cfg_builder.cfg
        } else {
            lctx.cfg_builder.finish(self.sctx)
        };

        // In theory this conversion can fail if non const functions/references are used
        // In practice this should have been caught during AST lowering and its therefore fine to panick here
        let cfg = cfg.map(&mut TryDefaultConversion);
        let operand = Operand::new(
            operand.contents.map_input(|x| x.try_into().unwrap()),
            operand.span,
        );

        Some(Expression(cfg, operand))
    }

    pub fn lower_real_expression(&mut self, expr: ExpressionId) -> Option<Expression> {
        let mut lctx = LocalCtx::new_small(self);
        let operand = lctx.fold_real(expr)?;
        let cfg = lctx.cfg_builder.finish(self.sctx);
        Some(Expression(cfg, operand))
    }

    pub fn lower_string_expression(&mut self, expr: ExpressionId) -> Option<Expression> {
        let span = self.hir[expr].span;
        let mut lctx = LocalCtx::new_small(self);
        let res = lctx.lower_expr(expr)?;
        if res.ty == Type::STRING {
            let operand = lctx.rvalue_to_operand(res, span);
            let cfg = lctx.cfg_builder.finish(self.sctx);
            Some(Expression(cfg, operand))
        } else {
            self.errors.add(TypeMissmatch {
                span,
                expected_type: TyPrinter(Type::STRING),
                found: res.ty.into(),
            });
            None
        }
    }

    pub fn eval_string_constant(&mut self, expr: ExpressionId) -> Option<StringLiteral> {
        self.lower_string_expression(expr)
            .map(|x| x.const_eval().unwrap())
            .and_then(|x| {
                if let Scalar(String(val)) = x {
                    Some(val)
                } else {
                    None
                }
            })
    }

    pub fn eval_real_constant(&mut self, expr: ExpressionId) -> Option<f64> {
        self.lower_real_expression(expr)
            .and_then(|x| x.const_eval())
            .and_then(|x| {
                if let Scalar(Real(val)) = x {
                    Some(val)
                } else {
                    None
                }
            })
    }

    pub fn eval_int_constant(&mut self, expr: ExpressionId) -> Option<i64> {
        self.lower_real_expression(expr)
            .and_then(|x| x.const_eval())
            .and_then(|x| {
                if let Scalar(Integer(val)) = x {
                    Some(val)
                } else {
                    None
                }
            })
    }
}

pub fn lower_hir_userfacing<L: HirLowering>(hir: Hir, lowering: &mut L) -> UserResult<Mir> {
    lower_hir_userfacing_with_printer(hir, lowering)
}

pub fn lower_hir_userfacing_with_printer<L: HirLowering, P: DiagnosticSlicePrinter>(
    hir: Hir,
    lowering: &mut L,
) -> UserResult<Mir, P> {
    lower_hir(hir, lowering).map_err(|err| err.user_facing())
}

pub fn lower_hir<L: HirLowering>(
    mut src: Hir,
    lowering: &mut L,
) -> Result<Mir, MultiDiagnostic<Error>> {
    let span = trace_span!("hir_lowering");
    let _enter = span.enter();

    let syntax_ctx: IndexVec<SyntaxCtx, _> = src
        .syntax_ctx
        .iter()
        .map(|data| SyntaxContextData {
            span: data.span,
            lint_levels: HashMap::default(),
            parent: data.parent,
        })
        .collect();

    let dst = Mir {
        branches: IndexVec::new(),
        nodes: take(&mut src.nodes),
        ports: take(&mut src.ports),
        disciplines: take(&mut src.disciplines),
        modules: IndexVec::new(),
        parameters: IndexVec::new(),
        variables: IndexVec::new(),
        natures: IndexVec::new(),
        syntax_ctx,
    };

    let mut fold = HirFold {
        lowering,
        hir: &src,
        mir: dst,
        errors: MultiDiagnostic(Vec::with_capacity(64)),
        sctx: SyntaxCtx::ROOT,
        lower_sctx: BitSet::new_filled(src.syntax_ctx.len()),
    };

    // These were just copied from the HIR
    // Only thing to do here is fold attributes

    let discipline_iter = fold
        .mir
        .disciplines
        .iter_enumerated()
        .map(|(id, discipline)| (discipline.sctx, AttributeCtx::Discipline(id)));

    let net_iter = fold
        .mir
        .nodes
        .iter_enumerated()
        .map(|(id, net)| (net.sctx, AttributeCtx::Node(id)));

    let attributes: Vec<_> = discipline_iter.chain(net_iter).collect();

    for (sctx, src) in attributes {
        fold.handle_attributes(sctx, src);
    }

    fold.mir.natures = src
        .natures
        .iter_enumerated()
        .map(|(id, nature)| fold.lower_nature(id, nature))
        .collect();

    fold.mir.branches = src
        .branches
        .iter_enumerated()
        .map(|(id, nature)| fold.lower_branch(id, nature))
        .collect();

    fold.mir.parameters = src
        .parameters
        .iter_enumerated()
        .filter_map(|(id, param)| fold.lower_parameter(id, param))
        .collect();

    fold.mir.variables = src
        .variables
        .iter_enumerated()
        .map(|(id, nature)| fold.lower_variable(id, nature))
        .collect();

    fold.mir.modules = src
        .modules
        .iter_enumerated()
        .map(|(id, module)| fold.lower_module(id, module))
        .collect();

    // Process the attributes of the remaining contexts
    // These will belong to objects that were dropped during ast_lowering or hir_lowering (most notably blocks and functions)
    // and as such have no associated mir object (AttributeCtxt)

    let ones: Vec<_> = fold.lower_sctx.iter().collect();
    for sctx in ones {
        fold.handle_attributes(sctx, AttributeCtx::Syntactic)
    }

    if fold.errors.is_empty() {
        Ok(fold.mir)
    } else {
        Err(fold.errors)
    }
}
