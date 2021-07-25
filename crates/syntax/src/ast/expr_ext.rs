//! Various extension methods to ast Expr Nodes, which are hard to code-generate.

use crate::SyntaxNode;
use crate::{
    ast::{self, support, AstChildren, AstNode, AstToken},
    SyntaxToken, T,
};
use parser::SyntaxKind::{self, PATH, SYSFUN};

use super::FunctionRef;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrefixOp {
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
    pub fn op_kind(&self) -> Option<PrefixOp> {
        match self.op_token()?.kind() {
            T![~] => Some(PrefixOp::BitNegate),
            T![!] => Some(PrefixOp::Not),
            T![-] => Some(PrefixOp::Neg),
            T![+] => Some(PrefixOp::Identity),
            _ => None,
        }
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax().first_child_or_token()?.into_token()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
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
    pub fn op_details(&self) -> Option<(SyntaxToken, BinOp)> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|c| {
            let bin_op = match c.kind() {
                T![||] => BinOp::BooleanOr,
                T![&&] => BinOp::BooleanAnd,
                T![==] => BinOp::EqualityTest,
                T![!=] => BinOp::NegatedEqualityTest,
                T![<=] => BinOp::LesserEqualTest,
                T![>=] => BinOp::GreaterEqualTest,
                T![<] => BinOp::LesserTest,
                T![>] => BinOp::GreaterTest,
                T![+] => BinOp::Addition,
                T![*] => BinOp::Multiplication,
                T![-] => BinOp::Subtraction,
                T![/] => BinOp::Division,
                T![%] => BinOp::Remainder,
                T![<<] => BinOp::LeftShift,
                T![>>] => BinOp::RightShift,
                T![^] => BinOp::BitwiseXor,
                T![|] => BinOp::BitwiseOr,
                T![&] => BinOp::BitwiseAnd,
                T![**] => BinOp::Power,
                T![~^] | T![^~] => BinOp::BitwiseEq,
                _ => return None,
            };
            Some((c, bin_op))
        })
    }

    pub fn op_kind(&self) -> Option<BinOp> {
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
    String(ast::String),
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

        if let Some(t) = ast::String::cast(token.clone()) {
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
    pub fn value(&self) -> i64 {
        self.syntax.text().parse().unwrap()
    }
}

impl ast::String {
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
        support::children(self.syntax()).next()
    }

    pub fn else_val(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }
}

pub enum AsssigmentOp {
    /// a variable assigment stmt
    /// lhs must be an indentifier (example `I = V(a,c)/R;`)
    Eq,

    /// a contribute (<+) stmt
    /// lhs must be a branch access (example `I(a,c) <+ V(a,c)/R;`)
    Contribute,
}

impl ast::AssignStmt {
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
