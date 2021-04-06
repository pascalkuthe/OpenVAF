use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{
    CfgPass, ControlFlowGraph, InternedLocations, LocationId, TerminatorKind,
};
use openvaf_middle::{
    impl_pass_span, COperand, COperandData, CallType, OperandData, RValue, StmntKind,
};

pub struct Visit<'a, V> {
    visitor: V,
    locations: &'a InternedLocations,
}

impl<'a, C, V> CfgPass<'_, C> for Visit<'a, V>
where
    C: CallType,
    V: CfgVisitor<C>,
{
    type Result = ();

    fn run(mut self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        for bb in &cfg.blocks {
            for (stmnt, _) in &bb.statements {
                self.visitor.visit_stmnt(stmnt)
            }
            if let Some(term) = &bb.terminator {
                if let TerminatorKind::Split { ref condition, .. } = term.kind {
                    self.visitor.visit_rvalue(condition)
                }
            }
            // TODO phi stmnts?
        }
    }

    impl_pass_span!("cfg_visit");
}

pub trait CfgVisitor<C: CallType> {
    fn visit_input(&mut self, _input: &<C as CallType>::I) {}

    fn visit_operand(&mut self, operand: &COperand<C>) {
        if let OperandData::Read(ref input) = operand.contents {
            self.visit_input(input)
        }
    }

    fn visit_rvalue(&mut self, rval: &RValue<C>) {
        for operand in rval.operands() {
            self.visit_operand(operand)
        }
    }

    fn visit_stmnt(&mut self, stmnt: &StmntKind<C>) {
        match stmnt {
            StmntKind::Assignment(_, val) => self.visit_rvalue(val),
            StmntKind::Call(_, args, _) => {
                for arg in args {
                    self.visit_operand(arg)
                }
            }
            StmntKind::NoOp | StmntKind::CollapseHint(_, _) => {}
        }
    }
}
