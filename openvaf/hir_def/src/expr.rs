//! This module describes hir-level representation of expressions.
//!
//! This representation is:
//!
//! 1. Identity-based. Each expression has an `id`, so we can distinguish
//!    between different `1` in `1 + 1`.
//! 2. Independent of syntax. Though syntactic provenance information can be
//!    attached separately via id-based side map.
//! 3. Unresolved. Paths are stored as sequences of names, and not as defs the
//!    names refer to.
//!
//! See also a neighboring `body` module.

use std::fmt::{Debug, Display};
use std::intrinsics::transmute;

use arena::Idx;
use stdx::impl_debug;
use syntax::ast::{self, BinaryOp, UnaryOp};

use crate::Path;

pub type ExprId = Idx<Expr>;

#[derive(Clone, Eq, PartialEq)]
pub enum Literal {
    String(Box<str>),
    Int(i32),
    Float(BitewiseF64),
    Inf,
}

impl Literal {
    pub fn unwrap_str(&self) -> &str {
        if let Literal::String(lit) = self {
            lit
        } else {
            unreachable!("called unwrap str on {self:?}")
        }
    }
}

impl_debug! {
    match Literal{
        Literal::String(_) => "\"<literal>\"";
        Literal::Int(val) => "{}",val;
        Literal::Float(val) => "{}",f64::from(*val);
        Literal::Inf => "inf";
    }
}

/// Wrapper around f64 that implements
#[derive(Clone, Eq, PartialEq, Hash, Copy)]
#[repr(transparent)]
pub struct BitewiseF64(u64);

pub const PZERO: BitewiseF64 = BitewiseF64(unsafe { transmute(0f64) });
pub const NZERO: BitewiseF64 = BitewiseF64(unsafe { transmute(-0f64) });

impl From<f64> for BitewiseF64 {
    #[inline(always)] // compiles to no op
    fn from(val: f64) -> Self {
        Self(val.to_bits())
    }
}
impl From<BitewiseF64> for f64 {
    #[inline(always)] // compiles to no op
    fn from(bitwise: BitewiseF64) -> Self {
        f64::from_bits(bitwise.0)
    }
}

impl AsRef<f64> for BitewiseF64 {
    #[inline(always)] // compiles to no op
    fn as_ref(&self) -> &f64 {
        // This is save because BitewiseF64 is repr(transparent) and therefore bitwise equivalent
        // to an f64
        unsafe { transmute(self) }
    }
}

impl AsMut<f64> for BitewiseF64 {
    #[inline(always)] // compiles to no op
    fn as_mut(&mut self) -> &mut f64 {
        // This is save because BitewiseF64 is repr(transparent) and therefore bitwise equivalent
        // to an f64
        unsafe { transmute(self) }
    }
}

impl Debug for BitewiseF64 {
    #[inline] // compiles to f64::fmt(self,f)
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&f64::from(*self), f)
    }
}

impl Display for BitewiseF64 {
    #[inline] // compiles to f64::fmt(self,f)
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&f64::from(*self), f)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// This is produced if the syntax tree does not have a required expression piece.
    Missing,
    Path {
        path: Path,
        port: bool,
    },
    BinaryOp {
        lhs: ExprId,
        rhs: ExprId,
        op: Option<BinaryOp>,
    },
    UnaryOp {
        expr: ExprId,
        op: UnaryOp,
    },
    Select {
        cond: ExprId,
        then_val: ExprId,
        else_val: ExprId,
    },
    Call {
        fun: Option<Path>,
        args: Vec<ExprId>,
    },
    Array(Vec<ExprId>),
    Literal(Literal),
}

impl Expr {
    #[inline]
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match *self {
            Expr::Missing | Expr::Path { .. } | Expr::Literal(_) => {}
            Expr::BinaryOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Expr::UnaryOp { expr, .. } => f(expr),
            Expr::Select { cond, then_val, else_val } => {
                f(cond);
                f(then_val);
                f(else_val);
            }
            Expr::Call { args: ref exprs, .. } | Expr::Array(ref exprs) => {
                for e in exprs {
                    f(*e)
                }
            }
        }
    }

    pub fn unwrap_literal(&self) -> &Literal {
        if let Expr::Literal(lit) = self {
            lit
        } else {
            unreachable!("called unwrap_literal on {self:?}")
        }
    }
}

pub type StmtId = Idx<Stmt>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    Missing,
    Empty,
    Expr(ExprId),
    EventControl { event: Event, body: StmtId },
    Assigment { dst: ExprId, val: ExprId, assignment_kind: ast::AssignOp },
    Block { /*scope: Option<BlockId>,*/ body: Vec<StmtId> },
    If { cond: ExprId, then_branch: StmtId, else_branch: StmtId },
    ForLoop { init: StmtId, cond: ExprId, incr: StmtId, body: StmtId },
    WhileLoop { cond: ExprId, body: StmtId },
    Case { discr: ExprId, case_arms: Vec<Case> }, // TODO lint on unreachable
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum GlobalEvent {
    InitalStep,
    FinalStep,
}

// non_exhaustive because currently the full standard is not implemented
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
#[non_exhaustive]
pub enum Event {
    Global { kind: GlobalEvent, phases: Vec<String> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CaseCond {
    Default,
    Vals(Vec<ExprId>), // TODO PROFILE: SmallVec<[ExprId; 1]> here
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Case {
    pub cond: CaseCond,
    pub body: StmtId,
}

impl Stmt {
    #[inline]
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match *self {
            Stmt::Empty | Stmt::Missing | Stmt::Block { .. } | Stmt::EventControl { .. } => (),
            Stmt::If { cond: expr, .. }
            | Stmt::ForLoop { cond: expr, .. }
            | Stmt::WhileLoop { cond: expr, .. }
            | Stmt::Expr(expr) => f(expr),
            Stmt::Assigment { dst, val, .. } => {
                f(dst);
                f(val)
            }
            Stmt::Case { discr, ref case_arms, .. } => {
                f(discr);
                for case in case_arms {
                    if let CaseCond::Vals(ref vals) = case.cond {
                        for val in vals {
                            f(*val)
                        }
                    }
                }
            }
        }
    }

    #[inline]
    pub fn walk_child_stmts(&self, mut f: impl FnMut(StmtId)) {
        match *self {
            Stmt::Expr(_) | Stmt::Assigment { .. } | Stmt::Missing | Stmt::Empty => (),
            Stmt::WhileLoop { body, .. } | Stmt::EventControl { body, .. } => f(body),
            Stmt::If { then_branch: true_stmt, else_branch: false_stmt, .. } => {
                f(true_stmt);
                f(false_stmt);
            }
            Stmt::ForLoop { init, incr, body, .. } => {
                f(init);
                f(incr);
                f(body);
            }
            Stmt::Case { ref case_arms, .. } => {
                for arm in case_arms {
                    f(arm.body)
                }
            }
            Stmt::Block { ref body, .. } => {
                for stmt in body {
                    f(*stmt)
                }
            }
        }
    }

    #[inline]
    pub fn unwrap_expr(&self) -> ExprId {
        if let Stmt::Expr(e) = self {
            *e
        } else {
            unreachable!("Called unwrap_expr on {:?}", self)
        }
    }
}
