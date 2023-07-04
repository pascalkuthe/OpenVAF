//! Various extension methods to ast Expr Nodes, which are hard to code-generate.

use stdx::impl_display;

use super::Stmt;
use crate::ast::{self, support, AstChildren, AstNode, AstToken};
use crate::{SyntaxToken, T};

impl ast::Expr {
    pub fn as_literal(&self) -> Option<LiteralKind> {
        if let ast::Expr::Literal(lit) = self {
            Some(lit.kind())
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// The `~` operator for bit inversion
    BitNegate,
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
    /// The `+` operator (does absolutely nothing)
    Identity,
}

impl ast::PrefixExpr {
    pub fn op_kind(&self) -> Option<UnaryOp> {
        match self.op_token()?.kind() {
            T![~] => Some(UnaryOp::BitNegate),
            T![!] => Some(UnaryOp::Not),
            T![-] => Some(UnaryOp::Neg),
            T![+] => Some(UnaryOp::Identity),
            _ => None,
        }
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax().first_child_or_token()?.into_token()
    }
}

impl_display! {
    match UnaryOp{
        UnaryOp::BitNegate => "~";
        UnaryOp::Not => "!";
        UnaryOp::Neg => "-";
        UnaryOp::Identity => "+";
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// The `||` operator for boolean OR
    BooleanOr,
    /// The `&&` operator for boolean AND
    BooleanAnd,
    /// The `==` operator for equality testing
    EqualityTest,
    /// The `!=` operator for equality testing
    NegatedEqualityTest,
    /// The `<=` operator for lesser-equal testing
    LesserEqualTest,
    /// The `>=` operator for greater-equal testing
    GreaterEqualTest,
    /// The `<` operator for comparison
    LesserTest,
    /// The `>` operator for comparison
    GreaterTest,
    /// The `+` operator for addition
    Addition,
    /// The `*` operator for multiplication
    Multiplication,
    /// The `-` operator for subtraction
    Subtraction,
    /// The `/` operator for division
    Division,
    /// The `%` operator for remainder after division
    Remainder,
    /// The `<<` operator for left shift
    LeftShift,
    /// The `>>` operator for right shift
    RightShift,
    /// The `^` operator for bitwise XOR
    BitwiseXor,
    /// The `~^`/`^~` operator for bitwise XOR
    BitwiseEq,
    /// The `|` operator for bitwise OR
    BitwiseOr,
    /// The `&` operator for bitwise AND
    BitwiseAnd,
    /// The `**` operator for exponents
    Power,
}

impl ast::BinExpr {
    pub fn op_details(&self) -> Option<(SyntaxToken, BinaryOp)> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|c| {
            let bin_op = match c.kind() {
                T![||] => BinaryOp::BooleanOr,
                T![&&] => BinaryOp::BooleanAnd,
                T![==] => BinaryOp::EqualityTest,
                T![!=] => BinaryOp::NegatedEqualityTest,
                T![<=] => BinaryOp::LesserEqualTest,
                T![>=] => BinaryOp::GreaterEqualTest,
                T![<] => BinaryOp::LesserTest,
                T![>] => BinaryOp::GreaterTest,
                T![+] => BinaryOp::Addition,
                T![*] => BinaryOp::Multiplication,
                T![-] => BinaryOp::Subtraction,
                T![/] => BinaryOp::Division,
                T![%] => BinaryOp::Remainder,
                T![<<] => BinaryOp::LeftShift,
                T![>>] => BinaryOp::RightShift,
                T![^] => BinaryOp::BitwiseXor,
                T![|] => BinaryOp::BitwiseOr,
                T![&] => BinaryOp::BitwiseAnd,
                T![**] => BinaryOp::Power,
                T![~^] | T![^~] => BinaryOp::BitwiseEq,
                _ => return None,
            };
            Some((c, bin_op))
        })
    }

    pub fn op_kind(&self) -> Option<BinaryOp> {
        self.op_details().map(|t| t.1)
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.op_details().map(|t| t.0)
    }

    pub fn lhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }

    pub fn rhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }

    // pub fn sub_exprs(&self) -> (Option<ast::Expr>, Option<ast::Expr>) {
    //     let mut children = support::children(self.syntax());
    //     let first = children.next();
    //     let second = children.next();
    //     (first, second)
    // }
}

impl_display! {
    match BinaryOp{
        BinaryOp::BooleanOr => "||";
        BinaryOp::BooleanAnd => "&&";
        BinaryOp::EqualityTest => "==";
        BinaryOp::NegatedEqualityTest => "!=";
        BinaryOp::LesserEqualTest => "<=";
        BinaryOp::GreaterEqualTest => ">=";
        BinaryOp::LesserTest => "<";
        BinaryOp::GreaterTest => ">";
        BinaryOp::Addition => "+";
        BinaryOp::Multiplication => "*";
        BinaryOp::Subtraction => "-";
        BinaryOp::Division => "/";
        BinaryOp::Remainder => "%";
        BinaryOp::LeftShift => "<<";
        BinaryOp::RightShift => ">>";
        BinaryOp::BitwiseXor => "^";
        BinaryOp::BitwiseEq => "~^";
        BinaryOp::BitwiseOr => "|";
        BinaryOp::BitwiseAnd => "&";
        BinaryOp::Power => "**";
    }
}

pub enum ArrayExprKind {
    Repeat { initializer: Option<ast::Expr>, repeat: Option<ast::Expr> },
    ElementList(AstChildren<ast::Expr>),
}

impl ast::ArrayExpr {
    pub fn kind(&self) -> ArrayExprKind {
        if self.is_repeat() {
            ArrayExprKind::Repeat {
                initializer: support::children(self.syntax()).next(),
                repeat: support::children(self.syntax()).nth(1),
            }
        } else {
            ArrayExprKind::ElementList(support::children(self.syntax()))
        }
    }

    fn is_repeat(&self) -> bool {
        self.syntax().children_with_tokens().any(|it| it.kind() == T![;])
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    String(ast::StrLit),
    IntNumber(ast::IntNumber),
    SiRealNumber(ast::SiRealNumber),
    StdRealNumber(ast::StdRealNumber),
    Inf,
}

impl ast::Literal {
    pub fn token(&self) -> SyntaxToken {
        self.syntax()
            .children_with_tokens()
            .find(|e| !e.kind().is_trivia())
            .and_then(|e| e.into_token())
            .unwrap()
    }
    pub fn kind(&self) -> LiteralKind {
        let token = self.token();

        if let Some(t) = ast::IntNumber::cast(token.clone()) {
            return LiteralKind::IntNumber(t);
        }
        if let Some(t) = ast::SiRealNumber::cast(token.clone()) {
            return LiteralKind::SiRealNumber(t);
        }
        if let Some(t) = ast::StdRealNumber::cast(token.clone()) {
            return LiteralKind::StdRealNumber(t);
        }

        if let Some(t) = ast::StrLit::cast(token.clone()) {
            return LiteralKind::String(t);
        }

        match token.kind() {
            T![inf] => LiteralKind::Inf,
            _ => unreachable!(),
        }
    }
}

impl ast::StdRealNumber {
    pub fn value(&self) -> f64 {
        let src = self.syntax.text();
        src.parse().unwrap()
    }
}

impl ast::SiRealNumber {
    pub fn value(&self) -> f64 {
        let src = self.syntax.text();
        let (src, scale_char) = src.split_at(src.len() - 1);
        let exp = match scale_char {
            "T" => 12,
            "G" => 9,
            "M" => 6,
            "K" | "k" => 3,
            "m" => -3,
            "u" => -6,
            "n" => -9,
            "p" => -12,
            "f" => -15,
            "a" => -18,
            _ => unreachable!(),
        };
        src.parse::<f64>().unwrap() * (10_f64).powi(exp)
    }
}

impl ast::IntNumber {
    pub fn value(&self) -> i32 {
        self.syntax.text().parse().unwrap()
    }
}

impl ast::StrLit {
    pub fn value(&self) -> &str {
        let src = self.syntax.text();
        &src[1..src.len() - 1]
    }
    pub fn unescaped_value(&self) -> String {
        self.value()
            .replace(r"\n", "\n")
            .replace(r"\\", "\\")
            .replace(r"\t", "\t")
            .replace(r#"\""#, "\"")
            .replace("\\\n", "\n")
            .replace("\\\r\n", "\r\n")
    }
}
impl ast::SelectExpr {
    pub fn then_val(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }

    pub fn else_val(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(2)
    }
}

pub enum AsssigmentOp {
    /// a variable assignment stmt
    /// lhs must be an identifier (example `I = V(a,c)/R;`)
    Eq,

    /// a contribute (<+) stmt
    /// lhs must be a branch access (example `I(a,c) <+ V(a,c)/R;`)
    Contribute,
}

impl ast::Assign {
    pub fn op_details(&self) -> Option<(SyntaxToken, AsssigmentOp)> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|c| {
            let bin_op = match c.kind() {
                T![=] => AsssigmentOp::Eq,
                T![<+] => AsssigmentOp::Contribute,
                _ => return None,
            };
            Some((c, bin_op))
        })
    }
}

impl ast::BlockStmt {
    pub fn body(&self) -> AstChildren<Stmt> {
        support::children(self.syntax())
    }
}
