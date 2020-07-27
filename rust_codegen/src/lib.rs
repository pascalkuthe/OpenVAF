//  * ******************************************************************************************
//  * Copyright (c) 2020 Pascal Kuthe. This file is part of the verilog-arc project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/verilogarc/blob/master/LICENSE.
//  *  No part of verilog-arc, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

pub use open_vaf;
use open_vaf::ast::{UnaryOperator, VariableType};
use open_vaf::cfg::ControlFlowGraph;
use open_vaf::cfg::{BasicBlockId, Terminator};
use open_vaf::hir::DisciplineAccess;
use open_vaf::ir::ids::{
    BranchId, IntegerExpressionId, NetId, ParameterId, PortId, RealExpressionId, StatementId,
    StringExpressionId, VariableId,
};
use open_vaf::ir::mir::RealExpression;
use open_vaf::ir::{DoubleArgMath, NoiseSource, PrintOnFinish, SingleArgMath, StopTaskKind};
use open_vaf::mir::ExpressionId;
use open_vaf::mir::{
    ComparisonOperator, IntegerBinaryOperator, IntegerExpression, Mir, ParameterType,
    RealBinaryOperator, Statement, StringExpression,
};
use open_vaf::SourceMap;
use open_vaf::StringLiteral;
use proc_macro2::{Ident, Literal, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};

pub struct RealNumberInterpolator(pub f64);
impl ToTokens for RealNumberInterpolator {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.0 {
            val if val.is_finite() => tokens.append(Literal::f64_suffixed(val)),
            val if val == f64::INFINITY => quote!(f64::INFINITY).to_tokens(tokens),
            val if val == f64::NEG_INFINITY => quote!(f64::NEG_INFINITY).to_tokens(tokens),
            val if val.is_nan() => quote!(f64::NAN).to_tokens(tokens),
            val => unreachable!(
                "f64 can only be NAN, INFINITY, NEG_INFINITY or a finite number! {} didnt match any of these",
                val
            ),
        }
    }
}

pub struct ParameterTypeInterpolator<'lt>(pub &'lt ParameterType);
impl<'lt> ToTokens for ParameterTypeInterpolator<'lt> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.0 {
            ParameterType::String { .. } => quote!(&str).to_tokens(tokens),
            ParameterType::Real { .. } => quote!(f64).to_tokens(tokens),
            ParameterType::Integer { .. } => quote!(i64).to_tokens(tokens),
        }
    }
}

pub struct CfgInterpolator<'lt, EI> {
    pub mir: &'lt Mir,
    pub cfg: &'lt ControlFlowGraph,
    pub sm: &'lt SourceMap,
    pub start: BasicBlockId,
    pub end: Option<BasicBlockId>,
    pub external_interpolator: &'lt EI,
}
impl<'lt, EI: TargetSpecificInterpolator> ToTokens for CfgInterpolator<'lt, EI> {
    #[allow(clippy::too_many_lines)]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut current = self.start;
        loop {
            match self.end {
                Some(end) if end == current => break,
                _ => BasicBlockInterpolator {
                    mir: self.mir,
                    external_interpolator: self.external_interpolator,
                    block: current,
                    cfg: self.cfg,
                    sm: self.sm,
                }
                .to_tokens(tokens),
            }
            match self.cfg.blocks[current].terminator {
                Terminator::Goto(next) => current = next,
                Terminator::End => break,
                Terminator::Split {
                    condition,
                    true_block,
                    false_block,
                    merge,
                } => {
                    let condition = IntegerExpressionInterpolator {
                        mir: self.mir,
                        sm: self.sm,
                        expression: condition,
                        external_interpolator: self.external_interpolator,
                    };

                    if merge == current {
                        //while loop
                        let mut body = TokenStream::new();
                        CfgInterpolator {
                            mir: self.mir,
                            start: true_block,
                            end: Some(merge),
                            cfg: self.cfg,
                            sm: self.sm,
                            external_interpolator: self.external_interpolator,
                        }
                        .to_tokens(&mut body);

                        quote!(
                            while (#condition) != 0 {
                                #body
                            }
                        )
                        .to_tokens(tokens);

                        current = false_block;
                    } else if merge == true_block {
                        // if statement with empty if block
                        let mut body = TokenStream::new();
                        CfgInterpolator {
                            mir: self.mir,
                            start: false_block,
                            end: Some(merge),
                            cfg: self.cfg,
                            sm: self.sm,
                            external_interpolator: self.external_interpolator,
                        }
                        .to_tokens(&mut body);

                        // condition == 0 is the opposite of condition != 0
                        quote!(
                            if (#condition) == 0 {
                                #body
                            }
                        )
                        .to_tokens(tokens);

                        current = merge;
                    } else if merge == false_block {
                        // if statement with empty else
                        let mut body = TokenStream::new();
                        CfgInterpolator {
                            mir: self.mir,
                            start: true_block,
                            end: Some(merge),
                            cfg: self.cfg,
                            sm: self.sm,
                            external_interpolator: self.external_interpolator,
                        }
                        .to_tokens(&mut body);

                        quote!(
                            if (#condition) != 0 {
                                #body
                            }
                        )
                        .to_tokens(tokens);

                        current = merge;
                    } else {
                        // if else statement
                        let mut if_block = TokenStream::new();
                        let mut else_block = TokenStream::new();

                        CfgInterpolator {
                            mir: self.mir,
                            start: true_block,
                            end: Some(merge),
                            cfg: self.cfg,
                            sm: self.sm,
                            external_interpolator: self.external_interpolator,
                        }
                        .to_tokens(&mut if_block);

                        CfgInterpolator {
                            mir: self.mir,
                            start: false_block,
                            end: Some(merge),
                            cfg: self.cfg,
                            sm: self.sm,
                            external_interpolator: self.external_interpolator,
                        }
                        .to_tokens(&mut else_block);

                        quote!(
                            if (#condition) != 0 {
                                #if_block
                            } else {
                                #else_block
                            }
                        )
                        .to_tokens(tokens);
                        current = merge;
                    }
                }
            }
        }
    }
}

pub struct BasicBlockInterpolator<'lt, EI> {
    pub mir: &'lt Mir,
    pub cfg: &'lt ControlFlowGraph,
    pub sm: &'lt SourceMap,
    pub block: BasicBlockId,
    pub external_interpolator: &'lt EI,
}

impl<'lt, EI: TargetSpecificInterpolator> ToTokens for BasicBlockInterpolator<'lt, EI> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for stmt in self.cfg.blocks[self.block].statements.iter().copied() {
            match self.mir[stmt].contents {
                Statement::Assignment(variable, value) => {
                    let name = gen_variable_ident(variable);
                    match value {
                        ExpressionId::Real(value) => {
                            let value = RealExpressionInterpolator {
                                expression: value,
                                mir: self.mir,
                                sm: self.sm,
                                external_interpolator: self.external_interpolator,
                            };
                            (quote! {#name = #value;}).to_tokens(tokens);
                        }

                        ExpressionId::Integer(value) => {
                            let value = IntegerExpressionInterpolator {
                                expression: value,
                                mir: self.mir,
                                sm: self.sm,
                                external_interpolator: self.external_interpolator,
                            };
                            (quote! {#name = #value;}).to_tokens(tokens);
                        }
                        ExpressionId::String(id) => {
                            let value = StringExpressionInterpolator {
                                mir: self.mir,
                                sm: self.sm,
                                expression: id,
                                external_interpolator: self.external_interpolator,
                            };
                            (quote! {#name = #value;}).to_tokens(tokens);
                        }
                    }
                }

                Statement::Contribute(access, branch, val) => self
                    .external_interpolator
                    .contribute_to_tokens(tokens, stmt, access, branch, val),

                Statement::StopTask(kind, print) => self
                    .external_interpolator
                    .stop_task_to_tokens(stmt, kind, print, tokens),
            }
        }
    }
}

pub trait TargetSpecificInterpolator: Sized {
    fn stop_task_to_tokens(
        &self,
        stmt: StatementId,
        kind: StopTaskKind,
        print: PrintOnFinish,
        tokens: &mut TokenStream,
    );

    fn port_flow_to_tokens(
        &self,
        _expr: RealExpressionId,
        port: PortId,
        order: u8,
        tokens: &mut TokenStream,
    ) {
        tokens.append(gen_port_flow_access(port, order))
    }

    fn temperature_to_tokens(&self, _expr: RealExpressionId, tokens: &mut TokenStream) {
        //By default temperature is just a variable
        tokens.append(Ident::new("temperature", Span::call_site()))
    }

    fn branch_access_to_tokens(
        &self,
        _expr: RealExpressionId,
        discipline_access: DisciplineAccess,
        branch: BranchId,
        order: u8,
        tokens: &mut TokenStream,
    ) {
        //By default temperature is just a variable
        tokens.append(gen_branch_access(discipline_access, branch, order))
    }

    fn simparam_to_tokens(
        &self,
        expr: RealExpressionId,
        tokens: &mut TokenStream,
        name: StringExpressionId,
        default: Option<RealExpressionInterpolator<'_, Self>>,
    );

    fn simparam_str_to_tokens(
        &self,
        expr: StringExpressionId,
        tokens: &mut TokenStream,
        name: StringExpressionId,
    );

    fn param_given_to_tokens(
        &self,
        expr: IntegerExpressionId,
        tokens: &mut TokenStream,
        param: ParameterId,
    );

    fn port_connected_to_tokens(
        &self,
        expr: IntegerExpressionId,
        tokens: &mut TokenStream,
        port: PortId,
    );

    fn limexp_to_tokens(
        &self,
        tokens: &mut TokenStream,
        arg: RealExpressionInterpolator<'_, Self>,
    ) {
        //Default to normal exp function
        quote!(#arg.exp()).to_tokens(tokens)
    }
    fn noise_to_tokens(
        &self,
        expr: RealExpressionId,
        tokens: &mut TokenStream,
        noise_source: NoiseSource<RealExpressionId, ()>,
        source: Option<StringLiteral>,
    );

    fn contribute_to_tokens(
        &self,
        tokens: &mut TokenStream,
        stmt: StatementId,
        access: DisciplineAccess,
        branch: BranchId,
        val: RealExpressionId,
    );
}

pub struct RealBuiltInFunctionCallInterpolator1p<'lt, EI> {
    pub mir: &'lt Mir,
    pub sm: &'lt SourceMap,
    pub call: SingleArgMath,
    pub arg: RealExpressionId,
    pub external_interpolator: &'lt EI,
}

impl<'lt, EI: TargetSpecificInterpolator> ToTokens
    for RealBuiltInFunctionCallInterpolator1p<'lt, EI>
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let arg = RealExpressionInterpolator {
            mir: self.mir,
            sm: self.sm,
            external_interpolator: self.external_interpolator,
            expression: self.arg,
        };
        match self.call {
            SingleArgMath::Sqrt => {
                quote!(#arg.sqrt()).to_tokens(tokens);
            }
            SingleArgMath::Exp(false) => {
                quote!(#arg.exp()).to_tokens(tokens);
            }
            SingleArgMath::Exp(true) => {
                self.external_interpolator.limexp_to_tokens(tokens, arg);
            }
            SingleArgMath::Ln => {
                quote!(#arg.ln()).to_tokens(tokens);
            }
            SingleArgMath::Log => {
                quote!(#arg.log10()).to_tokens(tokens);
            }
            SingleArgMath::Abs => {
                quote!(#arg.abs()).to_tokens(tokens);
            }
            SingleArgMath::Floor => {
                quote!(#arg.floor()).to_tokens(tokens);
            }
            SingleArgMath::Ceil => {
                quote!(#arg.ceil()).to_tokens(tokens);
            }
            SingleArgMath::Sin => {
                quote!(#arg.sin()).to_tokens(tokens);
            }
            SingleArgMath::Cos => {
                quote!(#arg.cos()).to_tokens(tokens);
            }
            SingleArgMath::Tan => {
                quote!(#arg.tan()).to_tokens(tokens);
            }
            SingleArgMath::ArcSin => {
                quote!(#arg.asin()).to_tokens(tokens);
            }
            SingleArgMath::ArcCos => {
                quote!(#arg.acos()).to_tokens(tokens);
            }
            SingleArgMath::ArcTan => {
                quote!(#arg.atan()).to_tokens(tokens);
            }
            SingleArgMath::SinH => {
                quote!(#arg.sinh()).to_tokens(tokens);
            }
            SingleArgMath::CosH => {
                quote!(#arg.cosh()).to_tokens(tokens);
            }
            SingleArgMath::TanH => {
                quote!(#arg.tanh()).to_tokens(tokens);
            }
            SingleArgMath::ArcSinH => {
                quote!(#arg.asinh()).to_tokens(tokens);
            }
            SingleArgMath::ArcCosH => {
                quote!(#arg.acosh()).to_tokens(tokens);
            }
            SingleArgMath::ArcTanH => {
                quote!(#arg.atanh()).to_tokens(tokens);
            }
        }
    }
}

pub struct RealBuiltInFunctionCallInterpolator2p<'lt, EI: TargetSpecificInterpolator> {
    pub mir: &'lt Mir,
    pub sm: &'lt SourceMap,
    pub call: DoubleArgMath,
    pub arg1: RealExpressionId,
    pub arg2: RealExpressionId,
    pub external_interpolator: &'lt EI,
}
impl<'lt, EI: TargetSpecificInterpolator> ToTokens
    for RealBuiltInFunctionCallInterpolator2p<'lt, EI>
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let arg1 = RealExpressionInterpolator {
            mir: self.mir,
            sm: self.sm,
            expression: self.arg1,
            external_interpolator: self.external_interpolator,
        };
        let arg2 = RealExpressionInterpolator {
            mir: self.mir,
            sm: self.sm,
            expression: self.arg2,
            external_interpolator: self.external_interpolator,
        };
        arg1.to_tokens(tokens);

        match self.call {
            DoubleArgMath::Pow => {
                quote! (.powf).to_tokens(tokens);
            }
            DoubleArgMath::Hypot => {
                quote! (.hypot).to_tokens(tokens);
            }
            DoubleArgMath::Min => {
                quote! (.min).to_tokens(tokens);
            }
            DoubleArgMath::Max => {
                quote! (.max).to_tokens(tokens);
            }
            DoubleArgMath::ArcTan2 => {
                quote! (.antan2).to_tokens(tokens);
            }
        }
        quote!((#arg2)).to_tokens(tokens);
    }
}

#[derive(Clone)]
pub struct IntegerExpressionInterpolator<'lt, EI> {
    pub mir: &'lt Mir,
    pub sm: &'lt SourceMap,
    pub expression: IntegerExpressionId,
    pub external_interpolator: &'lt EI,
}
impl<'lt, EI: TargetSpecificInterpolator> ToTokens for IntegerExpressionInterpolator<'lt, EI> {
    // TODO rewrite as Mir expr visit
    #[allow(clippy::too_many_lines)]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.mir[self.expression].contents {
            IntegerExpression::Condition(condition, if_val, else_val) => {
                let condition = IntegerExpressionInterpolator {
                    expression: condition,
                    ..*self
                };
                let if_val = IntegerExpressionInterpolator {
                    expression: if_val,
                    ..*self
                };
                let else_val = IntegerExpressionInterpolator {
                    expression: else_val,
                    ..*self
                };
                (quote! {
                    if #condition != 0 {
                        #if_val
                    } else {
                        #else_val
                    }
                })
                .to_tokens(tokens)
            }

            IntegerExpression::Literal(val) => tokens.append(Literal::i64_suffixed(val)),

            IntegerExpression::VariableReference(var_id) => {
                tokens.append(gen_variable_ident(var_id))
            }

            IntegerExpression::ParameterReference(par_id) => {
                tokens.append(gen_parameter_ident(par_id))
            }

            IntegerExpression::NetReference(_) | IntegerExpression::PortReference(_) => {
                unimplemented!("Digital Nets")
            }

            IntegerExpression::BinaryOperator(lhs, op, rhs) => {
                let lhs = IntegerExpressionInterpolator {
                    expression: lhs,
                    ..*self
                };
                let rhs = IntegerExpressionInterpolator {
                    expression: rhs,
                    ..*self
                };
                match op.contents {
                    IntegerBinaryOperator::Sum => quote! {(#lhs + #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::Subtract => quote! {(#lhs - #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::Multiply => quote! {(#lhs*#rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::Divide => quote! {(#lhs/#rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::Exponent => (quote! {

                        if #rhs >= 0 {
                            if rhs
                            #lhs.pow(#rhs)
                        } else {
                            0
                        }
                    })
                    .to_tokens(tokens),
                    IntegerBinaryOperator::Modulus => quote! {(#lhs % #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::ShiftLeft => quote! {(#lhs << #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::ShiftRight => quote! {(#lhs >> #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::LogicOr => {
                        (quote! {(((#lhs !=  0) || (#rhs != 0)) as i64)}).to_tokens(tokens)
                    }
                    IntegerBinaryOperator::LogicAnd => {
                        (quote! {(((#lhs != 0) && (#rhs != 0)) as i64)}).to_tokens(tokens)
                    }
                    IntegerBinaryOperator::Xor => quote! {(#lhs ^ #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::NXor => quote! {!(#lhs ^ #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::And => quote! {(#lhs & #rhs)}.to_tokens(tokens),
                    IntegerBinaryOperator::Or => quote! {(#lhs | #rhs)}.to_tokens(tokens),
                }
            }

            IntegerExpression::RealComparison(lhs, op, rhs) => {
                let lhs = RealExpressionInterpolator {
                    expression: lhs,
                    mir: self.mir,
                    sm: self.sm,
                    external_interpolator: self.external_interpolator,
                };
                let rhs = RealExpressionInterpolator {
                    expression: rhs,
                    mir: self.mir,
                    sm: self.sm,
                    external_interpolator: self.external_interpolator,
                };
                match op.contents {
                    ComparisonOperator::LessThen => {
                        (quote! {((#lhs < #rhs) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::LessEqual => {
                        (quote! {((#lhs <= #rhs) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::GreaterThen => {
                        (quote! {((#lhs > #rhs) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::GreaterEqual => {
                        (quote! {((#lhs >= #rhs ) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::LogicEqual => {
                        (quote! {((#lhs.approx_eq(#rhs, F64Margin::default())) as i64)})
                            .to_tokens(tokens)
                    }
                    ComparisonOperator::LogicalNotEqual => {
                        (quote! {((#lhs.approx_ne(#rhs, F64Margin::default())) as i64)})
                            .to_tokens(tokens)
                    }
                }
            }

            IntegerExpression::IntegerComparison(lhs, op, rhs) => {
                let lhs = IntegerExpressionInterpolator {
                    expression: lhs,
                    ..*self
                };
                let rhs = IntegerExpressionInterpolator {
                    expression: rhs,
                    ..*self
                };
                match op.contents {
                    ComparisonOperator::LessThen => {
                        (quote! {((#lhs < #rhs) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::LessEqual => {
                        (quote! {((#lhs <= #rhs) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::GreaterThen => {
                        (quote! {((#lhs > #rhs) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::GreaterEqual => {
                        (quote! {((#lhs >= #rhs ) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::LogicEqual => {
                        (quote! {((#lhs == #rhs) as i64)}).to_tokens(tokens)
                    }
                    ComparisonOperator::LogicalNotEqual => {
                        (quote! {((#lhs != #rhs) as i64)}).to_tokens(tokens)
                    }
                }
            }

            IntegerExpression::UnaryOperator(op, expr) => {
                let expr = IntegerExpressionInterpolator {
                    expression: expr,
                    ..*self
                };
                match op.contents {
                    UnaryOperator::LogicNegate | UnaryOperator::BitNegate =>
                    //these operators are the same in rust
                    {
                        (quote! {(!#expr)}).to_tokens(tokens)
                    }
                    UnaryOperator::ArithmeticNegate => quote! {(-#expr)}.to_tokens(tokens),
                    UnaryOperator::ExplicitPositive => expr.to_tokens(tokens),
                }
            }

            IntegerExpression::Abs(arg) => {
                let arg = IntegerExpressionInterpolator {
                    expression: arg,
                    ..*self
                };
                (quote! {
                    #arg.abs()
                })
                .to_tokens(tokens);
            }

            IntegerExpression::Min(arg1, arg2) => {
                let arg1 = IntegerExpressionInterpolator {
                    expression: arg1,
                    ..*self
                };
                let arg2 = IntegerExpressionInterpolator {
                    expression: arg2,
                    ..*self
                };
                (quote! {
                    #arg1.min(#arg2)
                })
                .to_tokens(tokens);
            }

            IntegerExpression::Max(arg1, arg2) => {
                let arg1 = IntegerExpressionInterpolator {
                    expression: arg1,
                    ..*self
                };
                let arg2 = IntegerExpressionInterpolator {
                    expression: arg2,
                    ..*self
                };
                (quote! {
                    #arg1.max(#arg2)
                })
                .to_tokens(tokens);
            }

            IntegerExpression::RealCast(expression) => {
                let expression = RealExpressionInterpolator {
                    expression,
                    mir: self.mir,
                    sm: self.sm,
                    external_interpolator: self.external_interpolator,
                };
                (quote! {
                    (#expression.round() as i64)
                })
                .to_tokens(tokens);
            }

            IntegerExpression::StringEq(lhs, rhs) => {
                let lhs = StringExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    expression: lhs,
                    external_interpolator: self.external_interpolator,
                };
                let rhs = StringExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    expression: rhs,
                    external_interpolator: self.external_interpolator,
                };
                quote!((#lhs == #rhs) as i64).to_tokens(tokens)
            }

            IntegerExpression::StringNEq(lhs, rhs) => {
                let lhs = StringExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    expression: lhs,
                    external_interpolator: self.external_interpolator,
                };
                let rhs = StringExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    expression: rhs,
                    external_interpolator: self.external_interpolator,
                };
                quote!((#lhs != #rhs) as i64).to_tokens(tokens)
            }
            IntegerExpression::ParamGiven(param) => self
                .external_interpolator
                .param_given_to_tokens(self.expression, tokens, param),
            IntegerExpression::PortConnected(port) => self
                .external_interpolator
                .port_connected_to_tokens(self.expression, tokens, port),
        }
    }
}

#[derive(Clone)]
pub struct StringExpressionInterpolator<'lt, EI> {
    pub mir: &'lt Mir,
    pub sm: &'lt SourceMap,
    pub expression: StringExpressionId,
    pub external_interpolator: &'lt EI,
}
impl<'lt, EI: TargetSpecificInterpolator> ToTokens for StringExpressionInterpolator<'lt, EI> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.mir[self.expression].contents {
            StringExpression::SimParam(name) => {
                self.external_interpolator
                    .simparam_str_to_tokens(self.expression, tokens, name)
            }
            StringExpression::Condition(cond, true_val, false_val) => {
                let cond = IntegerExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    expression: cond,
                    external_interpolator: self.external_interpolator,
                };

                let true_val = StringExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    expression: true_val,
                    external_interpolator: self.external_interpolator,
                };

                let false_val = StringExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    expression: false_val,
                    external_interpolator: self.external_interpolator,
                };

                (quote! {
                    if #cond != 0 {
                        #true_val
                    }else{
                        #false_val
                    }
                })
                .to_tokens(tokens)
            }
            StringExpression::Literal(val) => {
                tokens.append(Literal::string(&val.raw_contents(self.sm)))
            }
            StringExpression::VariableReference(var) => tokens.append(gen_variable_ident(var)),
            StringExpression::ParameterReference(param) => {
                tokens.append(gen_parameter_ident(param))
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct RealExpressionInterpolator<'lt, EI> {
    pub mir: &'lt Mir,
    pub sm: &'lt SourceMap,
    pub expression: RealExpressionId,
    pub external_interpolator: &'lt EI,
}
impl<'lt, EI: TargetSpecificInterpolator> ToTokens for RealExpressionInterpolator<'lt, EI> {
    // TODO rewrite as Mir expr visit
    #[allow(clippy::too_many_lines)]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.mir[self.expression].contents {
            RealExpression::Temperature => self
                .external_interpolator
                .temperature_to_tokens(self.expression, tokens),

            RealExpression::SimParam(name, default) => {
                self.external_interpolator.simparam_to_tokens(
                    self.expression,
                    tokens,
                    name,
                    default.map(|default| RealExpressionInterpolator {
                        expression: default,
                        ..*self
                    }),
                )
            }

            RealExpression::Condition(condition, if_val, else_val) => {
                let condition = IntegerExpressionInterpolator {
                    mir: self.mir,
                    sm: self.sm,
                    external_interpolator: self.external_interpolator,
                    expression: condition,
                };
                let if_val = RealExpressionInterpolator {
                    expression: if_val,
                    ..*self
                };
                let else_val = RealExpressionInterpolator {
                    expression: else_val,
                    ..*self
                };
                (quote! {
                    if #condition != 0 {
                        #if_val
                    }else {
                        #else_val
                    }
                })
                .to_tokens(tokens)
            }

            RealExpression::Literal(val) => RealNumberInterpolator(val).to_tokens(tokens),

            RealExpression::BranchAccess(discipline_access, branch, order) => self
                .external_interpolator
                .branch_access_to_tokens(self.expression, discipline_access, branch, order, tokens),

            RealExpression::VariableReference(var_id) => tokens.append(gen_variable_ident(var_id)),

            RealExpression::BuiltInFunctionCall2p(call, arg1, arg2) => {
                let call = RealBuiltInFunctionCallInterpolator2p {
                    mir: self.mir,
                    sm: self.sm,
                    call,
                    arg1,
                    arg2,
                    external_interpolator: self.external_interpolator,
                };
                (quote! {(#call)}).to_tokens(tokens)
            }
            RealExpression::BuiltInFunctionCall1p(call, arg) => {
                let call = RealBuiltInFunctionCallInterpolator1p {
                    mir: self.mir,
                    sm: self.sm,
                    external_interpolator: self.external_interpolator,
                    call,
                    arg,
                };
                (quote! {(#call)}).to_tokens(tokens)
            }

            RealExpression::ParameterReference(par_id) => {
                tokens.append(gen_parameter_ident(par_id))
            }

            RealExpression::BinaryOperator(lhs, op, rhs) => {
                let lhs = RealExpressionInterpolator {
                    expression: lhs,
                    ..*self
                };
                let rhs = RealExpressionInterpolator {
                    expression: rhs,
                    ..*self
                };
                match op.contents {
                    RealBinaryOperator::Sum => quote! {(#lhs + #rhs)}.to_tokens(tokens),
                    RealBinaryOperator::Subtract => quote! {(#lhs - #rhs)}.to_tokens(tokens),
                    RealBinaryOperator::Multiply => quote! {(#lhs*#rhs)}.to_tokens(tokens),
                    RealBinaryOperator::Divide => quote! {(#lhs/#rhs)}.to_tokens(tokens),
                    RealBinaryOperator::Exponent => quote! {(#lhs.powf(#rhs))}.to_tokens(tokens),
                    RealBinaryOperator::Modulus => quote! {(#lhs % #rhs)}.to_tokens(tokens),
                }
            }

            RealExpression::Negate(_, expression) => {
                let expr = RealExpressionInterpolator {
                    expression,
                    ..*self
                };
                quote!((-#expr)).to_tokens(tokens)
            }

            RealExpression::IntegerConversion(expression) => {
                let expression = IntegerExpressionInterpolator {
                    expression,
                    mir: self.mir,
                    sm: self.sm,
                    external_interpolator: self.external_interpolator,
                };
                (quote! {
                    (#expression as f64)
                })
                .to_tokens(tokens);
            }
            RealExpression::Noise(source, name) => {
                self.external_interpolator
                    .noise_to_tokens(self.expression, tokens, source, name)
            }
            RealExpression::PortFlowAccess(port, order) => self
                .external_interpolator
                .port_flow_to_tokens(self.expression, port, order, tokens),
        }
    }
}

#[must_use]
pub fn gen_branch_access(
    discipline_access: DisciplineAccess,
    branch_access: BranchId,
    order: u8,
) -> Ident {
    Ident::new(
        format!("branch_{:?}_{}_{}", discipline_access, branch_access, order).as_str(),
        proc_macro2::Span::call_site(),
    )
}

#[must_use]
pub fn gen_port_flow_access(port: PortId, order: u8) -> Ident {
    Ident::new(
        format!("portflow_{}_{}", port, order).as_str(),
        proc_macro2::Span::call_site(),
    )
}

#[must_use]
pub fn gen_parameter_ident(parameter: ParameterId) -> Ident {
    Ident::new(
        format!("parameter_{}", parameter).as_str(),
        proc_macro2::Span::call_site(),
    )
}

#[must_use]
pub fn gen_variable_ident(id: VariableId) -> Ident {
    Ident::new(
        format!("variable_{}", id).as_str(),
        proc_macro2::Span::call_site(),
    )
}

#[must_use]
pub fn gen_port_ident(port: PortId) -> Ident {
    Ident::new(
        format!("port_{}", port).as_str(),
        proc_macro2::Span::call_site(),
    )
}

#[must_use]
pub fn generate_variable_type(variable_type: VariableType) -> TokenTree {
    match variable_type {
        VariableType::Integer => {
            TokenTree::Ident(Ident::new("i32", proc_macro2::Span::call_site()))
        }
        VariableType::Real => TokenTree::Ident(Ident::new("f64", proc_macro2::Span::call_site())),
        VariableType::String => {
            TokenTree::Ident(Ident::new("String", proc_macro2::Span::call_site()))
        }
    }
}

#[must_use]
pub fn gen_net_ident(net: NetId) -> Ident {
    Ident::new(
        format!("net_{}", net).as_str(),
        proc_macro2::Span::call_site(),
    )
}
