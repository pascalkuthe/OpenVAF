use std::borrow::Borrow;

use ahash::AHashMap;
use lasso::Spur;
use stdx::packed_option::PackedOption;
use typed_index_collections::TiVec;

use crate::dfg::uses::UseData;
use crate::entities::{Param, Tag};
use crate::{DataFlowGraph, Ieee64, Inst, Use, Value};

use consts::*;

macro_rules! consts {
    (
    $(pub const $name: ident: $ty: ident = $val: expr;)*

    ) => {
      consts!(@const 0, $($name),*);
        pub(super) fn init(dst: &mut DfgValues){
            $(consts!(@init $ty dst $val);)*
        }
    };

    (@const $pos: expr, $name: ident $(,$rem: ident)*) => {
        pub const $name: Value = Value::with_number_($pos);
      consts!(@const $pos + 1, $($rem),*);
    };



    (@const $pos: expr,) => {};

    (@init f64 $dst: ident $val: expr) => {
        $dst.fconst($val.into())
    };


    (@init i32 $dst: ident $val: expr) => {
        $dst.iconst($val)
    };


    (@init bool $dst: ident $val: literal) => {
        if $val{
            $dst.make(ValueDataType::True, None)
        }else{
            $dst.make(ValueDataType::False, None)
        }
    };
}

pub mod consts {
    use std::f64::consts::{LN_2, LOG10_E};

    use super::DfgValues;
    use super::Value;
    use super::ValueDataType;

    consts! {
        // Place holder for unused values that must remain (in phis)
        pub const GRAVESTONE: bool = false;
        pub const FALSE: bool = false;
        pub const TRUE: bool = true;
        pub const F_ZERO: f64 = 0.0;
        pub const ZERO: i32 = 0;
        pub const ONE: i32 = 1;
        pub const F_ONE: f64 = 1.0;
        pub const F_N_ONE: f64 = -1.0;
        pub const F_LN2_N: f64 = -LN_2;
        pub const F_LN2: f64 = LN_2;
        pub const F_LOG10_E: f64 = LOG10_E;
        pub const F_TWO: f64 = 2.0;
        pub const N_ONE: i32 = -1;
        pub const F_TEN: f64 = 10.0;
        pub const F_THREE: f64 = 3.0;
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Self {
        if val {
            TRUE
        } else {
            FALSE
        }
    }
}

#[derive(Clone)]
pub struct DfgValues {
    /// Primary value table with entries for all values.
    pub(super) defs: TiVec<Value, ValueData>,

    /// Primary Use table with entries for all uses
    pub(super) uses: TiVec<Use, UseData>,

    /// interned integer constants
    int_consts: AHashMap<i32, Value>,

    /// interned real constants
    real_consts: AHashMap<Ieee64, Value>,

    /// interned string constants
    str_consts: AHashMap<Spur, Value>,
}

#[derive(Clone, Debug)]
pub(super) struct ValueData {
    pub(super) ty: ValueDataType,
    pub(super) uses_head: PackedOption<Use>,
    pub(super) uses_tail: PackedOption<Use>,
    tag: PackedOption<Tag>,
}

impl ValueData {
    pub fn new(ty: ValueDataType, tag: PackedOption<Tag>) -> Self {
        Self { ty, uses_head: None.into(), uses_tail: None.into(), tag }
    }
}

impl From<ValueDataType> for ValueData {
    fn from(ty: ValueDataType) -> Self {
        Self::new(ty, None.into())
    }
}

/// Internal table storage for extended values.
#[derive(Clone, Debug)]
pub(super) enum ValueDataType {
    /// Value is defined by an instruction.
    Inst {
        num: u16,
        inst: Inst,
    },
    /// A function parameter
    Param {
        param: Param,
    },
    Fconst {
        val: Ieee64,
    },
    Iconst {
        val: i32,
    },
    Sconst {
        val: Spur,
    },
    Alias(Value),
    Invalid,
    True,
    False,
}

/// Where did a value come from?
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueDef {
    /// Value is the n'th result of an instruction.
    Result(Inst, usize),
    /// Value is the n'th parameter to a function.
    Param(Param),
    Const(Const),
    Invalid,
}

impl ValueDef {
    /// Unwrap the instruction where the value was defined, or panic.
    #[inline]
    pub fn unwrap_inst(&self) -> Inst {
        self.inst().expect("Value is not an instruction result")
    }

    #[inline]
    pub fn unwrap_result(&self) -> (Inst, usize) {
        self.result().expect("Value is not an instruction result")
    }

    #[inline]
    pub fn unwrap_const(&self) -> Const {
        self.as_const().expect("Value is not a constant")
    }

    #[inline]
    pub fn unwrap_param(&self) -> Param {
        self.as_param().expect("Value is not a parameter")
    }

    /// Get the instruction where the value was defined, if any.
    #[inline]
    pub fn inst(&self) -> Option<Inst> {
        match *self {
            Self::Result(inst, _) => Some(inst),
            _ => None,
        }
    }

    /// Get the instruction where the value was defined, if any.
    #[inline]
    pub fn result(&self) -> Option<(Inst, usize)> {
        match *self {
            Self::Result(inst, i) => Some((inst, i)),
            _ => None,
        }
    }

    /// Get the instruction where the value was defined, if any.
    #[inline]
    pub fn as_const(&self) -> Option<Const> {
        match *self {
            Self::Const(const_) => Some(const_),
            _ => None,
        }
    }

    /// Get the instruction where the value was defined, if any.
    #[inline]
    pub fn as_param(&self) -> Option<Param> {
        match *self {
            Self::Param(param) => Some(param),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Const {
    Float(Ieee64),
    Int(i32),
    Str(Spur),
    Bool(bool),
}

impl Const {
    pub fn unwrap_f64(self) -> f64 {
        if let Const::Float(val) = self {
            val.into()
        } else {
            unreachable!("Const is not a float!")
        }
    }
}

/// Handling values.
///
/// Values are either block parameters or instruction results.
impl DfgValues {
    /// Create a new empty `DataFlowGraph`.
    pub fn new() -> Self {
        let mut res = Self {
            defs: TiVec::new(),
            uses: TiVec::new(),
            int_consts: AHashMap::new(),
            real_consts: AHashMap::new(),
            str_consts: AHashMap::new(),
        };

        consts::init(&mut res);
        res.defs[GRAVESTONE].ty = ValueDataType::Invalid;

        // normalize to plus zero for consts
        res.real_consts.insert((-0f64).into(), F_ZERO);

        res
    }

    /// Clear everything.
    pub fn clear(&mut self) {
        self.defs.clear();
        self.uses.clear();
        self.real_consts.clear();
        self.int_consts.clear();
        self.str_consts.clear();
    }

    /// Get the total number of values.
    pub fn num(&self) -> usize {
        self.defs.len()
    }

    /// Allocate an extended value entry.
    #[inline]
    pub(super) fn make(&mut self, ty: ValueDataType, tag: Option<Tag>) -> Value {
        let data = ValueData::new(ty, tag.into());
        self.defs.push_and_get_key(data)
    }

    /// Allocate an extended value entry.
    pub fn make_param(&mut self, param: Param) -> Value {
        self.make(ValueDataType::Param { param }, None)
    }

    #[inline]
    pub fn make_alias(&mut self, val: Value) -> Value {
        self.make(ValueDataType::Alias(val), None)
    }

    #[inline]
    pub fn make_alias_at(&mut self, val: Value, dst: Value) {
        self.defs[dst].ty = ValueDataType::Alias(val);
    }

    /// Allocate an extended value entry.
    pub fn make_param_at(&mut self, param: Param, val: Value) {
        self.defs[val].ty = ValueDataType::Param { param };
    }

    /// Get an iterator over all values.
    pub fn iter(&self) -> impl Iterator<Item = Value> + ExactSizeIterator {
        self.defs.keys()
    }

    /// Check if a value reference is valid.
    pub fn is_valid(&self, v: Value) -> bool {
        usize::from(v) < self.defs.len()
    }

    /// Get the definition of a value.
    ///
    /// This is either the instruction that defined it or the Block that has the value as an
    /// parameter.
    #[inline]
    pub fn def(&self, v: Value) -> ValueDef {
        match self.defs[v].ty {
            ValueDataType::Inst { inst, num } => ValueDef::Result(inst, num as usize),
            ValueDataType::Param { param } => ValueDef::Param(param),
            ValueDataType::Fconst { val } => ValueDef::Const(Const::Float(val)),
            ValueDataType::Iconst { val } => ValueDef::Const(Const::Int(val)),
            ValueDataType::Sconst { val } => ValueDef::Const(Const::Str(val)),
            ValueDataType::True => ValueDef::Const(Const::Bool(true)),
            ValueDataType::False => ValueDef::Const(Const::Bool(false)),
            ValueDataType::Alias(_) | ValueDataType::Invalid => ValueDef::Invalid,
        }
    }

    /// Get the definition of a value.
    ///
    /// This is either the instruction that defined it or the Block that has the value as an
    /// parameter.
    #[inline]
    pub fn def_allow_alias(&self, v: Value) -> ValueDef {
        match self.defs[v].ty {
            ValueDataType::Inst { inst, num } => ValueDef::Result(inst, num as usize),
            ValueDataType::Param { param } => ValueDef::Param(param),
            ValueDataType::Fconst { val } => ValueDef::Const(Const::Float(val)),
            ValueDataType::Iconst { val } => ValueDef::Const(Const::Int(val)),
            ValueDataType::Sconst { val } => ValueDef::Const(Const::Str(val)),
            ValueDataType::True => ValueDef::Const(Const::Bool(true)),
            ValueDataType::False => ValueDef::Const(Const::Bool(false)),
            ValueDataType::Alias(alias) => self.def_allow_alias(alias),
            ValueDataType::Invalid => ValueDef::Invalid,
        }
    }

    pub fn unwrap_bool(&self, v: Value) -> bool {
        match self.defs[v].ty {
            ValueDataType::True => true,
            ValueDataType::False => false,
            ref ty => unreachable!("called unwrap_bool on {:?} value", ty),
        }
    }

    pub fn unwrap_f64(&self, v: Value) -> f64 {
        match self.defs[v].ty {
            ValueDataType::Fconst { val } => val.into(),
            ref ty => unreachable!("called unwrap_f64 on {:?} value", ty),
        }
    }

    pub fn unwrap_i32(&self, v: Value) -> i32 {
        match self.defs[v].ty {
            ValueDataType::Iconst { val } => val,
            ref ty => unreachable!("called unwrap_i32 on {:?} value", ty),
        }
    }

    pub fn unwrap_str(&self, v: Value) -> Spur {
        match self.defs[v].ty {
            ValueDataType::Sconst { val } => val,
            ref ty => unreachable!("called unwrap_str on {:?} value", ty),
        }
    }

    pub fn tag(&self, val: Value) -> Option<Tag> {
        self.defs[val].tag.expand()
    }

    pub fn set_tag(&mut self, val: Value, tag: Option<Tag>) {
        self.defs[val].tag = tag.into()
    }

    pub fn is_dead(&self, val: Value) -> bool {
        self.defs[val].uses_head.is_none()
    }

    pub fn iconst(&mut self, val: i32) -> Value {
        *self.int_consts.entry(val).or_insert_with(|| {
            let data = ValueDataType::Iconst { val }.into();
            self.defs.push_and_get_key(data)
        })
    }

    pub fn fconst(&mut self, val: Ieee64) -> Value {
        *self.real_consts.entry(val).or_insert_with(|| {
            let data = ValueDataType::Fconst { val }.into();
            self.defs.push_and_get_key(data)
        })
    }

    pub fn sconst(&mut self, val: Spur) -> Value {
        *self.str_consts.entry(val).or_insert_with(|| {
            let data = ValueDataType::Sconst { val }.into();
            self.defs.push_and_get_key(data)
        })
    }

    pub fn iconst_at(&mut self, val: i32, dst: Value) {
        self.int_consts.insert(val, dst);
        self.defs[dst].ty = ValueDataType::Iconst { val };
    }

    pub fn fconst_at(&mut self, val: Ieee64, dst: Value) {
        self.real_consts.insert(val, dst);
        self.defs[dst].ty = ValueDataType::Fconst { val };
    }

    pub fn sconst_at(&mut self, val: Spur, dst: Value) {
        self.str_consts.insert(val, dst);
        self.defs[dst].ty = ValueDataType::Sconst { val };
    }

    #[inline]
    pub fn resolve_alias(&self, mut val: Value) -> Value {
        while let ValueDataType::Alias(res) = self.defs[val].ty {
            val = res
        }
        val
    }
}

impl DataFlowGraph {
    #[inline]
    pub fn resolve_alias(&self, val: Value) -> Value {
        self.values.resolve_alias(val)
    }

    pub fn strip_alias(&mut self) {
        let mut stack = Vec::with_capacity(64);
        for mut val in self.values() {
            loop {
                let def = unsafe { self.values.defs.get_unchecked(val) };
                if let ValueDataType::Alias(alias) = def.ty {
                    stack.push(val);
                    val = alias
                } else {
                    break;
                }
            }

            if let Some(first) = stack.get(0) {
                self.replace_uses(*first, val);
                for it in stack[1..].iter().copied() {
                    let dst = unsafe { self.values.defs.get_unchecked_mut(it) };
                    dst.ty = ValueDataType::Alias(val);
                }
                stack.clear();
            }
        }
    }
}

impl Borrow<DfgValues> for DataFlowGraph {
    fn borrow(&self) -> &DfgValues {
        &self.values
    }
}

impl Default for DfgValues {
    fn default() -> Self {
        Self::new()
    }
}
