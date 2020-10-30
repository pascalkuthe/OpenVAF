use crate::UseDefGraph;
use openvaf_middle::{CallType, impl_pass_span};
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph};

struct CopyPropagation<'a>(&'a UseDefGraph);

impl<'a, C: CallType > CfgPass<'_, C> for CopyPropagation<'a> {
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        for
    }

    impl_pass_span!("copy_propagation");
}
