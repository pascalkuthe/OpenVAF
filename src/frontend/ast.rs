use Node_Types::*;

#[derive(Clone, Debug)]
pub enum Node {
    TOP,
    ATTRIBUTE(String),
    MODULE(String),
    TASK,
    FUNCTION,
    WIRE { is_input: bool, is_output: bool },
    BRANCH_DECL,
    BRANC_REF,
    PARAMETER,
    LOCALPARAM,
    ALIAS_PARAMETER,
    DEFPARAM,
    PARASET,
    ARGUMENT,
    RANGE,
    MULTIRANGE,
    CONSTANT_EXPRESSION,
    INTEGERVALUE(i64),
    REALVALUE(f64),
    STRINGVALUE(String),
    IDENTIFIER { name: String, discipline: String, nature: String },
    PREFIX,
    ASSERT,

    FCALL,
    TO_BITS,
    TO_SIGNED,
    TO_UNSIGNED,
    CONCAT,
    REPLICATE,
    BIT_NOT,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    BIT_XNOR,
    REDUCE_AND,
    REDUCE_OR,
    REDUCE_XOR,
    REDUCE_XNOR,
    REDUCE_BOOL,
    SHIFT_LEFT,
    SHIFT_RIGHT,
    SHIFT_SLEFT,
    SHIFT_SRIGHT,
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
    LOGIC_AND,
    LOGIC_OR,
    LOGIC_NOT,
    TERNARY,
    MEMRD,
    MEMWR,
    MEMINIT,

    TCALL,
    ASSIGN,
    CELL,
    PRIMITIVE,
    CELLARRAY,
    ALWAYS,
    INITIAL,
    ANALOG(bool),
    BLOCK(Option<String>),
    ASSIGN_EQ,
    ASSIGN_LE,
    CONTRIBUTE,
    CONTRIBUTE_INDIREKT,
    CASE,
    COND,
    CONDX,
    CONDZ,
    DEFAULT,
    FOR,
    WHILE,
    REPEAT,

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
    MODPORT(Option<PORT>),
    MODPORTMEMBER,
    PACKAGE,
    WIRETYPE,
}

pub mod Node_Types {
    #[derive(Clone, Debug)]
    pub struct PORT {
        pub identifier: String,
        pub is_signed: bool,
        pub is_input: bool,
        pub is_output: bool,
        pub verilog_type: VERILOG_TYPE,
        pub discipline: String,
    }

    impl PORT {
        pub fn new() -> Self {
            PORT {
                identifier: String::from(""),
                is_signed: false,
                is_input: false,
                is_output: false,
                verilog_type: VERILOG_TYPE::UNDECLARED,
                discipline: String::from(""),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum VERILOG_TYPE {
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
//    fn expect<'pair>(expected_value: Option<Pair<'pair, parser::Rule>>, error_message: &str, containing_rule: pest::Span)
//                     -> parser::Result<Pair<'pair, parser::Rule>> {
//        match expected_value {
//            Some(pair) => Ok(pair),
//            None => error(error_message, containing_rule)
//        }
//    }




