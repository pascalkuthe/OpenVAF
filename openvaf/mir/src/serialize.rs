use std::fmt::{self, Display, Write};

use ahash::RandomState;
use indexmap::{IndexMap, IndexSet};
use lasso::Rodeo;

use crate::{
    Block, Const, ControlFlowGraph, Function, Inst, InstructionData, Param, Value, ValueDef,
};

macro_rules! wln {
    ($dst:expr) => {
        { let _ = writeln!($dst); }
    };
    ($dst:expr, $($arg:tt)*) => {
        { let _ = writeln!($dst, $($arg)*); }
    };
}

macro_rules! w {
    ($dst:expr) => {
        { let _ = write!($dst); }
    };
    ($dst:expr, $($arg:tt)*) => {
        { let _ = write!($dst, $($arg)*); }
    };
}

impl Function {
    pub fn to_json(
        &self,
        cfg: &ControlFlowGraph,
        intern: &Rodeo,
        mut param_name: impl FnMut(Param) -> (&'static str, String),
        outputs: impl Iterator<Item = (String, Value)>,
    ) -> String {
        let mut inst_map = IndexSet::default();
        let bb_map = cfg
            .reverse_postorder(self)
            .map(|bb| {
                inst_map.extend(self.layout.block_insts(bb));
                bb
            })
            .collect();
        let mut val_map: IndexSet<Value, RandomState> = IndexSet::default();
        for &inst in inst_map.iter() {
            if let InstructionData::PhiNode(phi) = &self.dfg.insts[inst] {
                val_map.extend(self.dfg.phi_edges(phi).map(|(_, val)| val))
            } else {
                val_map.extend(self.dfg.instr_args(inst));
            }
            val_map.extend(self.dfg.inst_results(inst));
        }
        let mut inputs: IndexMap<&'static str, Vec<_>> = IndexMap::default();
        for (i, val) in val_map.iter().copied().enumerate() {
            if let Some(param) = self.dfg.value_def(val).as_param() {
                let (kind, name) = param_name(param);
                inputs.entry(kind).or_default().push((name, i));
            }
        }
        let outputs: Vec<_> =
            outputs.map(|(name, val)| (name, val_map.insert_full(val).0)).collect();
        let mut serializer = Serializer {
            cfg,
            func: self,
            inst_map: &inst_map,
            bb_map: &bb_map,
            val_map: &val_map,
            inputs: &inputs,
            intern,
            buf: String::new(),
            indent_level: 0,
            needs_indent: false,
        };
        serializer.serialize_dict(|sel| {
            sel.serialize_key("cfg");
            sel.serialize_list_entries_with(sel.bb_map.iter().copied(), Serializer::serialize_bb);
            wln!(sel, ",");
            sel.serialize_key("instructions");
            sel.serialize_list_entries_with(sel.inst_map.iter(), |sel, val| {
                sel.serialize_inst(*val)
            });
            wln!(sel, ",");
            sel.serialize_key("vals");
            sel.serialize_list_entries_with(sel.val_map.iter(), |sel, val| {
                sel.serialize_val(*val, &mut param_name)
            });
            wln!(sel, ",");
            sel.serialize_key("inputs");
            sel.serialize_inputs();
            wln!(sel, ",");
            sel.serialize_key("outputs");
            sel.serialize_dict_entries(outputs.into_iter());
        });
        serializer.buf
    }
}

struct Serializer<'a> {
    cfg: &'a ControlFlowGraph,
    func: &'a Function,
    inst_map: &'a IndexSet<Inst, RandomState>,
    bb_map: &'a IndexSet<Block, RandomState>,
    val_map: &'a IndexSet<Value, RandomState>,
    inputs: &'a IndexMap<&'static str, Vec<(String, usize)>>,
    intern: &'a Rodeo,
    buf: String,
    indent_level: usize,
    needs_indent: bool,
}

impl Serializer<'_> {
    fn indented(&mut self, f: impl FnOnce(&mut Self)) {
        self.indent_level += 1;
        wln!(self);
        f(self);
        self.indent_level -= 1;
        self.buf = self.buf.trim_end_matches('\n').to_string();
    }

    fn serialize_bb(&mut self, bb: Block) {
        self.serialize_dict(|sel| {
            sel.serialize_key("predecessors");
            sel.serialize_list_entries(
                sel.cfg.pred_iter(bb).map(|bb| sel.bb_map.get_index_of(&bb).unwrap()),
            );
            wln!(sel, ",");
            sel.serialize_key("successors");
            sel.serialize_list_entries(
                sel.cfg.succ_iter(bb).map(|bb| sel.bb_map.get_index_of(&bb).unwrap()),
            );
            wln!(sel, ",");
            sel.serialize_key("instructions");
            sel.serialize_list_entries(
                sel.func
                    .layout
                    .block_insts(bb)
                    .map(|inst| sel.inst_map.get_index_of(&inst).unwrap()),
            );
        })
    }

    fn serialize_inputs(&mut self) {
        self.serialize_dict_entries_with(self.inputs.keys(), |sel, input| {
            sel.serialize_dict_entries(sel.inputs[input].iter().map(|(k, v)| (k, *v)));
        })
    }

    fn serialize_val(
        &mut self,
        val: Value,
        mut param_name: impl FnMut(Param) -> (&'static str, String),
    ) {
        match self.func.dfg.value_def(val) {
            ValueDef::Result(inst, idx) => {
                self.serialize_dict(|sel| {
                    w!(sel, "\"instruction\": {},", sel.inst_map.get_index_of(&inst).unwrap());
                    w!(sel, "\"idx\": {idx}")
                });
            }
            ValueDef::Param(param) => {
                let (kind, name) = param_name(param);
                w!(self, "{{ \"{kind}\": \"{name}\"}}")
            }
            ValueDef::Const(Const::Float(val)) => {
                w!(self, "{{ \"fconst\": {}}}", f64::from(val))
            }
            ValueDef::Const(Const::Int(val)) => w!(self, "{{ \"iconst\": {val}}}"),
            ValueDef::Const(Const::Str(val)) => {
                w!(self, "{{ \"sconst\": \"{}\"}}", &self.intern[val])
            }
            ValueDef::Const(Const::Bool(val)) => w!(self, "{{ \"bconst\": {val}}}"),
            ValueDef::Invalid => unreachable!(),
        }
    }

    fn serialize_inst(&mut self, inst: Inst) {
        self.serialize_dict(|sel| {
            sel.serialize_key("opcode");
            wln!(sel, "\"{}\",", sel.func.dfg.insts[inst].opcode());
            if let InstructionData::PhiNode(phi) = &sel.func.dfg.insts[inst] {
                sel.serialize_key("arguments");
                sel.serialize_dict_entries(
                    sel.func
                        .dfg
                        .phi_edges(phi)
                        .map(|(bb, val)| (bb, sel.val_map.get_index_of(&val).unwrap())),
                )
            } else {
                sel.serialize_key("arguments");
                sel.serialize_list_entries(
                    sel.func
                        .dfg
                        .instr_args(inst)
                        .iter()
                        .map(|val| sel.val_map.get_index_of(val).unwrap()),
                );
            }
            wln!(sel, ",");
            sel.serialize_key("results");
            sel.serialize_list_entries(
                sel.func
                    .dfg
                    .inst_results(inst)
                    .iter()
                    .map(|val| sel.val_map.get_index_of(val).unwrap()),
            );
            wln!(sel, ",");
            sel.serialize_key("uses");
            sel.serialize_list_entries(sel.func.dfg.inst_uses(inst).map(|use_| {
                sel.inst_map.get_index_of(&sel.func.dfg.use_to_operand(use_).0).unwrap()
            }));
        })
    }

    fn serialize_dict_entries<I, K, V>(&mut self, mut entries: I)
    where
        I: Iterator<Item = (K, V)>,
        K: Display,
        V: Display,
    {
        self.serialize_dict(|sel| {
            if let Some((key, val)) = entries.next() {
                sel.serialize_key(&key);
                w!(sel, "{val}")
            }
            for (key, val) in entries {
                wln!(sel, ",");
                sel.serialize_key(&key);
                w!(sel, "{val}")
            }
        });
    }

    fn serialize_dict_entries_with<I>(
        &mut self,
        mut entries: I,
        mut f: impl FnMut(&mut Self, I::Item),
    ) where
        I: Iterator,
        I::Item: Display,
    {
        self.serialize_dict(|sel| {
            if let Some(first) = entries.next() {
                sel.serialize_key(&first);
                f(sel, first);
            }
            for key in entries {
                wln!(sel, ",");
                sel.serialize_key(&key);
                f(sel, key)
            }
        });
    }

    fn serialize_dict(&mut self, f: impl FnOnce(&mut Self)) {
        w!(self, "{{");
        self.indented(|sel| f(sel));
        wln!(self);
        w!(self, "}}");
    }

    fn serialize_list_entries<I>(&mut self, entries: I)
    where
        I: Iterator,
        I::Item: Display,
    {
        self.serialize_list_entries_with(entries, |sel, item| w!(sel, "{item}"))
    }

    fn serialize_list_entries_with<I>(
        &mut self,
        mut entries: I,
        mut f: impl FnMut(&mut Self, I::Item),
    ) where
        I: Iterator,
        I::Item: Display,
    {
        self.serialize_list(|sel| {
            if let Some(first) = entries.next() {
                f(sel, first);
            }
            for item in entries {
                wln!(sel, ",");
                f(sel, item);
            }
        });
    }

    fn serialize_list(&mut self, f: impl FnOnce(&mut Self)) {
        w!(self, "[");
        self.indented(|sel| f(sel));
        wln!(self);
        w!(self, "]");
    }

    fn serialize_key(&mut self, key: impl Display) {
        w!(self, "\"{key}\": ");
    }
}

impl<'a> Write for Serializer<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for line in s.split_inclusive('\n') {
            if self.needs_indent {
                match self.buf.chars().last() {
                    Some('\n') | None => {}
                    _ => self.buf.push('\n'),
                }

                if line != "\n" {
                    // don't indent empty lines! required to play nice with expect_test
                    self.buf.push_str(&"    ".repeat(self.indent_level));
                }
                self.needs_indent = false;
            }

            self.buf.push_str(line);
            self.needs_indent = line.ends_with('\n');
        }

        Ok(())
    }
}
