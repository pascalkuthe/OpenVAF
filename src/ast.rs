//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use indextree::{Arena, NodeId};
use log::info;
use node_types::*;

use crate::ast;

//TODO allow replacing name based references with node based references
#[derive(Clone, Debug)]
pub enum Node {
    Top,
    Attribute(String),
    Module(String),
    Nature { name: String, discipline: Option<NodeId> },
    Task,
    Function,
    VariableOrWire { name: String, type_info: VARIABLE },
    Branch { name: String, port_branch: bool },
    Parameter { name: String, local: bool, type_info: SIGNED },
    AliasParameter,
    DEFPARAM,
    PARASET,
    Range,
    ConstantExpression,

    StringValue(String),
    PREFIX,
    ASSERT,

    //name and whether a system function was called
    Fcall { hierarchical_identifier: Vec<String>, system_function: bool },
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


#[derive(Debug, Clone)]
pub struct RawAst {
    pub(super) arena: Arena<ast::Node>,
    pub(super) top_node: NodeId,
}

impl RawAst {
    pub fn pprint(&self) {
        fn pprint(node: NodeId, arena: &Arena<ast::Node>, prefix: String, last: bool) {
            let prefix_current = if last { "`- " } else { "|- " };

            info!("{}{}{:?}", prefix, prefix_current, arena.get(node).unwrap().get());

            let prefix_child = if last { "   " } else { "|  " };
            let prefix = prefix + prefix_child;

            if node.children(arena).next().is_some() {
                let last_child = node.children(arena).count() - 1;

                for (i, child) in node.children(arena).enumerate() {
                    pprint(child, arena, prefix.to_string(), i == last_child);
                }
            }
        }
        pprint(self.top_node, &self.arena, "".to_string(), true);
    }
}

pub struct Ast {
    pub tree: RawAst,
    //TODO pub symbboltable
}

