/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub use crate::error::{Error, TyPrinter};
use openvaf_diagnostics::{lints::Lint, DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use openvaf_hir::{DisciplineAccess, ExpressionId, Hir, Primary};
use openvaf_ir::{
    Attribute, NoiseSource, ParameterExcludeConstraint, ParameterRangeConstraint,
    ParameterRangeConstraintBound, PrintOnFinish, StopTaskKind,
};
use openvaf_middle::osdi_types::ConstVal::Scalar;
use openvaf_middle::osdi_types::SimpleConstVal::{Integer, Real, String};
use openvaf_middle::{
    CallType, ConstVal, Expression, Mir, Module, Nature, Operand, OperandData, Parameter,
    ParameterCallType, ParameterConstraint, ParameterInput, RValue, RealConstCallType, StmntKind,
    SyntaxContextData, Type, Variable,
};
use openvaf_session::symbols::keywords;

use crate::error::Error::{ExpectedLintName, TypeMissmatch};
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_data_structures::{BitSet, HashMap};

use openvaf_ir::ids::{
    BranchId, DisciplineId, ModuleId, NatureId, NetId, ParameterId, PortId, StatementId, SyntaxCtx,
    VariableId,
};

use openvaf_session::sourcemap::{Span, StringLiteral};

use openvaf_data_structures::sync::RwLock;
use std::mem::take;

mod cfg_builder;

use tracing::trace_span;

use crate::lints::{EmptyBuiltinAttribute, LintLevelOverwrite, UnknownLint};
pub use cfg_builder::LocalCtx;
use openvaf_diagnostics::lints::{LintLevel, Linter};

mod error;
mod lints;

pub type HirSystemFunctionCall = openvaf_hir::SystemFunctionCall;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AttributeCtx {
    Variable(VariableId),
    Parameter(ParameterId),
    Net(NetId),
    Branch(BranchId),
    Module(ModuleId),
    Nature(NatureId),
    Discipline(DisciplineId),
    Syntactic,
}

pub trait HirLowering: Sized {
    type AnalogBlockExprLower: ExpressionLowering<Self>;
    fn handle_attribute(
        ctx: &mut HirFold<Self>,
        attr: &Attribute,
        src: AttributeCtx,
        sctx: SyntaxCtx,
    );

    fn handle_statement_attribute<'a, 'h, C: ExpressionLowering<Self>>(
        ctx: &mut LocalCtx<'a, 'h, C, Self>,
        attr: &Attribute,
        stmt: StatementId,
        sctx: SyntaxCtx,
    );
}

// TODO enforce times somehow?
pub trait ExpressionLowering<L: HirLowering>: CallType {
    fn port_flow(ctx: &mut LocalCtx<Self, L>, port: PortId, span: Span) -> Option<RValue<Self>>;

    fn branch_access(
        ctx: &mut LocalCtx<Self, L>,
        access: DisciplineAccess,
        branch: BranchId,
        span: Span,
    ) -> Option<RValue<Self>>;

    fn parameter_ref(
        ctx: &mut LocalCtx<Self, L>,
        param: ParameterId,
        span: Span,
    ) -> Option<RValue<Self>>;

    fn time_derivative(
        ctx: &mut LocalCtx<Self, L>,
        expr: ExpressionId,
        span: Span,
    ) -> Option<RValue<Self>>;

    fn noise(
        ctx: &mut LocalCtx<Self, L>,
        source: NoiseSource<ExpressionId, ()>,
        name: Option<ExpressionId>,
        span: Span,
    ) -> Option<RValue<Self>>;

    fn system_function_call(
        ctx: &mut LocalCtx<Self, L>,
        call: &HirSystemFunctionCall,
        span: Span,
    ) -> Option<RValue<Self>>;

    fn stop_task(
        ctx: &mut LocalCtx<Self, L>,
        kind: StopTaskKind,
        finish: PrintOnFinish,
        span: Span,
    ) -> Option<StmntKind<Self>>;

    fn collapse_hint(
        ctx: &mut LocalCtx<Self, L>,
        hi: NetId,
        lo: NetId,
        span: Span,
    ) -> Option<StmntKind<Self>>;
}

impl<L: HirLowering> ExpressionLowering<L> for ParameterCallType {
    fn port_flow(_: &mut LocalCtx<Self, L>, _: PortId, _: Span) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn branch_access(
        _: &mut LocalCtx<Self, L>,
        _: DisciplineAccess,
        _: BranchId,
        _: Span,
    ) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn parameter_ref(
        _: &mut LocalCtx<Self, L>,
        param: ParameterId,
        span: Span,
    ) -> Option<RValue<Self>> {
        Some(RValue::Use(Operand::new(
            OperandData::Read(ParameterInput::Value(param)),
            span,
        )))
    }

    fn time_derivative(
        _: &mut LocalCtx<Self, L>,
        _: ExpressionId,
        _: Span,
    ) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn noise(
        _: &mut LocalCtx<Self, L>,
        _: NoiseSource<ExpressionId, ()>,
        _: Option<ExpressionId>,
        _: Span,
    ) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn system_function_call(
        _: &mut LocalCtx<Self, L>,
        call: &HirSystemFunctionCall,
        span: Span,
    ) -> Option<RValue<Self>> {
        if let HirSystemFunctionCall::ParameterGiven(param) = call {
            Some(RValue::Use(Operand::new(
                OperandData::Read(ParameterInput::Given(*param)),
                span,
            )))
        } else {
            unreachable!()
        }
    }

    fn stop_task(
        _: &mut LocalCtx<Self, L>,
        _: StopTaskKind,
        _: PrintOnFinish,
        _: Span,
    ) -> Option<StmntKind<Self>> {
        unimplemented!()
    }

    fn collapse_hint(
        _: &mut LocalCtx<Self, L>,
        _hi: NetId,
        _lo: NetId,
        span: Span,
    ) -> Option<StmntKind<Self>> {
        unimplemented!()
    }
}

impl<L: HirLowering> ExpressionLowering<L> for RealConstCallType {
    fn port_flow(_: &mut LocalCtx<Self, L>, _: PortId, _: Span) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn branch_access(
        _: &mut LocalCtx<Self, L>,
        _: DisciplineAccess,
        _: BranchId,
        _: Span,
    ) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn parameter_ref(_: &mut LocalCtx<Self, L>, _: ParameterId, _: Span) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn time_derivative(
        _: &mut LocalCtx<Self, L>,
        _: ExpressionId,
        _: Span,
    ) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn noise(
        _: &mut LocalCtx<Self, L>,
        _: NoiseSource<ExpressionId, ()>,
        _: Option<ExpressionId>,
        _: Span,
    ) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn system_function_call(
        _: &mut LocalCtx<Self, L>,
        _: &HirSystemFunctionCall,
        _: Span,
    ) -> Option<RValue<Self>> {
        unimplemented!()
    }

    fn stop_task(
        _: &mut LocalCtx<Self, L>,
        _: StopTaskKind,
        _: PrintOnFinish,
        _: Span,
    ) -> Option<StmntKind<Self>> {
        unimplemented!()
    }

    fn collapse_hint(
        _: &mut LocalCtx<Self, L>,
        _hi: NetId,
        _lo: NetId,
        span: Span,
    ) -> Option<StmntKind<Self>> {
        unimplemented!()
    }
}

pub struct HirFold<'a, L: HirLowering> {
    pub lowering: &'a mut L,
    pub hir: &'a Hir,
    pub mir: Mir<L::AnalogBlockExprLower>,
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
        if !self.lower_sctx.remove(sctx) && cfg!(debug_assertions) {
            unreachable!("{:?} was processed twice!", sctx);
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
            keywords::openvaf_allow => LintLevel::Allow,
            keywords::openvaf_warn => LintLevel::Warn,
            keywords::openvaf_deny => LintLevel::Deny,
            keywords::openvaf_forbid => LintLevel::Forbid,
            _ => return false,
        };

        let names = if let Some(expr) = attr.value {
            match self.hir[expr].contents {
                openvaf_hir::Expression::Primary(Primary::Constant(Scalar(String(val)))) => {
                    vec![(val, self.hir[expr].span)]
                }
                openvaf_hir::Expression::Array(ref expressions) => {
                    let res = expressions
                        .iter()
                        .filter_map(|expr| {
                            if let openvaf_hir::Expression::Primary(Primary::Constant(Scalar(
                                String(val),
                            ))) = self.hir[*expr].contents
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

    fn lower_nature(&mut self, id: NatureId, nature: &openvaf_hir::Nature) -> Nature {
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

    fn lower_variable(&mut self, id: VariableId, var: &openvaf_hir::Variable) -> Variable {
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
            keywords::desc => {
                if let Some(val) = attr.value {
                    desc = fold.eval_string_constant(val)
                } else {
                    Linter::dispatch_late(Box::new(EmptyBuiltinAttribute(attr.ident)), sctx)
                }
                true
            }
            keywords::units => {
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

    fn lower_parameter(
        &mut self,
        id: ParameterId,
        param: &openvaf_hir::Parameter,
    ) -> Option<Parameter> {
        let span = trace_span!("parameter", id = id.index(), name = display(param.ident));
        let _enter = span.enter();

        self.sctx = param.sctx;

        let (unit, desc) =
            self.lower_units_and_desc_attributes(param.sctx, AttributeCtx::Parameter(id));

        let default = self.lower_assign_expr(param.default, param.ty);

        let kind = match &param.constraints {
            openvaf_hir::ParameterConstraint::Ordered(included, excluded) => {
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
            openvaf_hir::ParameterConstraint::Unordered(included, excluded) => {
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

    fn lower_module(
        &mut self,
        id: ModuleId,
        module: &openvaf_hir::Module,
    ) -> Module<L::AnalogBlockExprLower> {
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

    pub fn lower_expression<C: ExpressionLowering<L>>(
        &mut self,
        expr: ExpressionId,
    ) -> Option<Expression<C>> {
        let span = self.hir[expr].span;
        let mut lctx = LocalCtx::new_small(self);
        let expr = lctx.lower_expr(expr)?;
        let operand = lctx.rvalue_to_operand(expr, span);
        let cfg = lctx.cfg_builder.finish(self.sctx);
        Some(Expression(cfg, operand))
    }

    pub fn lower_assign_expr<C: ExpressionLowering<L>>(
        &mut self,
        expr: ExpressionId,
        ty: Type,
    ) -> Option<Expression<C>> {
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

        Some(Expression(cfg, operand))
    }

    pub fn lower_real_expression<C: ExpressionLowering<L>>(
        &mut self,
        expr: ExpressionId,
    ) -> Option<Expression<C>> {
        let mut lctx = LocalCtx::new_small(self);
        let operand = lctx.fold_real(expr)?;
        let cfg = lctx.cfg_builder.finish(self.sctx);
        Some(Expression(cfg, operand))
    }

    pub fn lower_string_expression<C: ExpressionLowering<L>>(
        &mut self,
        expr: ExpressionId,
    ) -> Option<Expression<C>> {
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
        self.lower_string_expression::<RealConstCallType>(expr)
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
        self.lower_real_expression::<RealConstCallType>(expr)
            .map(|x| x.const_eval().unwrap())
            .and_then(|x| {
                if let Scalar(Real(val)) = x {
                    Some(val)
                } else {
                    None
                }
            })
    }

    pub fn eval_int_constant(&mut self, expr: ExpressionId) -> Option<i64> {
        self.lower_real_expression::<RealConstCallType>(expr)
            .map(|x| x.const_eval().unwrap())
            .and_then(|x| {
                if let Scalar(Integer(val)) = x {
                    Some(val)
                } else {
                    None
                }
            })
    }
}

pub fn lower_hir_userfacing<L: HirLowering>(
    hir: Hir,
    lowering: &mut L,
) -> UserResult<Mir<L::AnalogBlockExprLower>> {
    lower_hir_userfacing_with_printer(hir, lowering)
}

pub fn lower_hir_userfacing_with_printer<L: HirLowering, P: DiagnosticSlicePrinter>(
    hir: Hir,
    lowering: &mut L,
) -> UserResult<Mir<L::AnalogBlockExprLower>, P> {
    lower_hir(hir, lowering).map_err(|err| err.user_facing())
}

pub fn lower_hir<L: HirLowering>(
    mut src: Hir,
    lowering: &mut L,
) -> Result<Mir<L::AnalogBlockExprLower>, MultiDiagnostic<Error>> {
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
        branches: take(&mut src.branches),
        nets: take(&mut src.nets),
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
        lower_sctx: BitSet::new_filled(src.syntax_ctx.len_idx()),
    };

    // These were just copied from the HIR
    // Only thing to do here is fold attributes

    let discipline_iter = fold
        .mir
        .disciplines
        .iter_enumerated()
        .map(|(id, discipline)| (discipline.sctx, AttributeCtx::Discipline(id)));
    let branch_iter = fold
        .mir
        .branches
        .iter_enumerated()
        .map(|(id, branch)| (branch.sctx, AttributeCtx::Branch(id)));
    let net_iter = fold
        .mir
        .nets
        .iter_enumerated()
        .map(|(id, net)| (net.sctx, AttributeCtx::Net(id)));

    let attributes: Vec<_> = discipline_iter.chain(branch_iter).chain(net_iter).collect();

    for (sctx, src) in attributes {
        fold.handle_attributes(sctx, src);
    }

    fold.mir.natures = src
        .natures
        .iter_enumerated()
        .map(|(id, nature)| fold.lower_nature(id, nature))
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

    let ones: Vec<_> = fold.lower_sctx.ones().collect();
    for sctx in ones {
        fold.handle_attributes(sctx, AttributeCtx::Syntactic)
    }

    if fold.errors.is_empty() {
        Ok(fold.mir)
    } else {
        Err(fold.errors)
    }
}
