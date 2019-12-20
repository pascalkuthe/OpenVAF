//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use indextree::{Arena, NodeId};
use node_types::*;

use crate::ast;
use pest::Span;
use std::fmt::{Display, Error, Formatter};

/// A now of a `RawAst` It contains the span of the source that generated it in addition to a normal Ast `Node`
#[derive(Debug, Clone)]
pub struct RawNode<'lt> {
    pub node_info: Node,
    pub src: Span<'lt>,
}

//TODO allow replacing name based references with node based references
#[derive(Clone, Debug)]
pub enum Node {
    Top,
    Attribute(String),
    Module(String),
    Nature {
        name: String,
        discipline: Option<NodeId>,
    },
    Task,
    Function,
    VariableOrWire {
        name: String,
        type_info: VARIABLE,
    },
    Branch {
        name: String,
        port_branch: bool,
    },
    Parameter {
        name: String,
        local: bool,
        type_info: SIGNED,
    },
    AliasParameter,
    DEFPARAM,
    PARASET,
    Range,
    ConstantExpression,

    StringValue(String),
    PREFIX,
    ASSERT,

    //name and whether a system function was called
    Fcall {
        hierarchical_identifier: Vec<String>,
        system_function: bool,
    },
    ToBits,
    TERNARY,
    MEMRD,
    MEMWR,
    MEMINIT,

    TCALL,
    ASSIGN,
    CELL,
    CELLARRAY,
    Always,
    Analog(bool),
    BLOCK(Option<String>),
    AssignEq,
    AssignLe,
    Contribute,
    ContributeIndirekt,
    Case,
    Cond,
    Default,
    For,
    While,
    Repeat,

    GENVAR,
    GENFOR,
    GENIF,
    GENCASE,
    GENBLOCK,
    TECALL,

    POSEDGE,
    NEGEDGE,
    EDGE,

    INTERFACE,
    INTERFACEPORT,
    INTERFACEPORTTYPE,
    ModPortDec(String, PORT),
    ModPort(String),
    PACKAGE,

    //PRIMITVES
    IntegerValue(i64),
    RealValue(f64),
    Reference(Vec<String>),
    PotentialAccess,
    FlowAccess,
    //OPERATORS
    CONCAT,
    REPLICATE,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    BitEq,
    ReduceAnd,
    ReduceOr,
    ReduceXor,
    ReduceXnor,
    ReduceBool,
    ShiftLeft,
    ShiftRight,
    ShiftSleft,
    ShiftSright,
    LT,
    LE,
    EQ,
    NE,
    EQX,
    NEX,
    GE,
    GT,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    POW,
    POS,
    NEG,
    LogicAnd,
    LogicOr,
    LogicNot,
}

pub mod node_types {
    #[derive(Clone, Debug)]
    pub struct PORT {
        pub is_input: bool,
        pub is_output: bool,
        pub base_type: VARIABLE,
    }

    #[derive(Clone, Debug)]
    pub struct SIGNED {
        pub verilog_type: VerilogType,
        pub signed: bool,
    }

    #[derive(Clone, Debug)]
    pub struct VARIABLE {
        pub basic_type: SIGNED,
        pub discipline: String,
    }

    impl VARIABLE {
        pub fn new() -> Self {
            Self {
                basic_type: SIGNED {
                    signed: false,
                    verilog_type: VerilogType::UNDECLARED,
                },
                discipline: String::from(""),
            }
        }
    }
    impl Default for VARIABLE {
        fn default() -> Self {
            Self::new()
        }
    }

    #[derive(Clone, Debug)]
    pub enum VerilogType {
        UNDECLARED,
        REG,
        WREAL,
        SUPPLY0,
        SUPPLY1,
        TRI,
        TRIAND,
        TRIOR,
        TRI0,
        TRI1,
        WIRE,
        UWIRE,
        WAND,
        WOR,
        TIME,
        INTEGER,
        REAL,
        REALTIME,
    }
}

/// An Abstract Syntax Tree that hasn't undergone schemantic analysis yet.
/// *Node* As Analysis of this Ast isn't complete yet it still contains references to the original source (to allow for error reporting) and its lifetime is therefor bounded by that of the source it was generated from
#[derive(Debug, Clone)]
pub struct RawAst<'lt> {
    pub(super) arena: Arena<ast::RawNode<'lt>>,
    pub(super) top_node: NodeId,
}

impl<'lt> Display for RawAst<'lt> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        fn pprint(
            f: &mut Formatter<'_>,
            node: NodeId,
            arena: &Arena<ast::RawNode>,
            prefix: String,
            last: bool,
        ) -> Result<(), Error> {
            let prefix_current = if last { "`- " } else { "|- " };

            write!(
                f,
                "{}{}{:?}",
                prefix,
                prefix_current,
                arena.get(node).unwrap().get()
            )?;

            let prefix_child = if last { "   " } else { "|  " };
            let prefix = prefix + prefix_child;

            if node.children(arena).next().is_some() {
                let last_child = node.children(arena).count() - 1;

                for (i, child) in node.children(arena).enumerate() {
                    pprint(f, child, arena, prefix.to_string(), i == last_child)?;
                }
            }
            Ok(())
        }
        pprint(f, self.top_node, &self.arena, "".to_string(), true)
    }
}
/// An [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) that abstractly represents a Verlog-AMS Project independent of the original source
pub struct Ast {
    pub(super) arena: Arena<ast::Node>,
    pub(super) top_node: NodeId, //TODO pub symtable
}
