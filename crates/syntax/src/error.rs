use crate::SyntaxKind;
use crate::{ast, AstPtr};
use stdx::{impl_display, pretty};
use text_size::TextRange;

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum SyntaxError {
    UnexpectedToken {
        expected: pretty::List<Vec<SyntaxKind>>,
        found: SyntaxKind,
        span: TextRange,
        expected_at: Option<TextRange>,
        missing_delimeter: bool,
    },
    SurplusToken {
        found: SyntaxKind,
        span: TextRange,
    },
    MissingToken {
        expected: SyntaxKind,
        span: TextRange,
        expected_at: TextRange,
    },
    IllegalRootSegment {
        path_segment: TextRange,
        prefix: Option<TextRange>,
    },
    BlockItemsAfterStmt {
        items: Vec<AstPtr<ast::BlockItem>>,
        first_stmt: TextRange,
    },
    BlockItemsWithoutScope {
        items: Vec<AstPtr<ast::BlockItem>>,
        begin_token: TextRange,
    },
    FunItemsAfterBody {
        items: Vec<AstPtr<ast::FunctionItem>>,
        body: TextRange,
    },
    MultipleFunBodys {
        additional_bodys: Vec<TextRange>,
        body: AstPtr<ast::Stmt>,
    },
    FunWithoutBody {
        fun: TextRange,
    },
    IllegalBranchNodeCnt {
        arg_list: TextRange,
        cnt: usize,
    },
    IllegalBranchNodeExpr {
        single: bool,
        illegal_nodes: Vec<TextRange>,
    },
    IllegalInfToken{
        range: TextRange
    }
}

use SyntaxError::*;

impl_display! {
    match SyntaxError{
        UnexpectedToken {expected,found,..} => "unexpected token '{}'; expected {}", found, expected;
        SurplusToken {found,..} => "unexpected token '{}'", found;
        MissingToken{expected, ..} => "unexpected token; expected '{}'", expected;
        IllegalRootSegment { ..} =>  "$root is only allowed as a prefix";
        BlockItemsAfterStmt{..}  => "declarations in blocks are only allowed before the first stmt";
        BlockItemsWithoutScope { ..} => "declarations in blocks require an explicit scope";
        FunItemsAfterBody{..} => "functions may not contain any items after the function body";
        MultipleFunBodys{..} => "functions may only contain one body";
        FunWithoutBody {..} => "function is missing a body";
        IllegalBranchNodeCnt { cnt,..} => "branch declaration require 1 or 2 nets; found {}", cnt;
        IllegalBranchNodeExpr{..} => "illegal expr was used to declare a branch node!";
        IllegalInfToken{..} => "unexpected token 'inf'; expected an expression";
    }
}
