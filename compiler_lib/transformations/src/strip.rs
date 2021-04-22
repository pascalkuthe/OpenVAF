use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, IntLocation, InternedLocations};
use openvaf_middle::{impl_pass_span, CallType};

pub struct Strip<'a> {
    pub retain: &'a BitSet<IntLocation>,
    pub locations: &'a InternedLocations,
}

impl<'a, C> CfgPass<'_, C> for Strip<'a>
where
    C: CallType,
{
    type Result = ();

    fn run(self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        for (bb, data) in cfg.blocks.iter_mut_enumerated() {
            let block_location = &self.locations[bb];

            let mut loc = block_location.phi_start;
            data.phi_statements.retain(|_| {
                let res = self.retain.contains(loc);
                loc += 1;
                res
            });

            let mut loc = block_location.stmnt_start;
            data.statements.retain(|_| {
                let res = self.retain.contains(loc);
                loc += 1;
                res
            })
        }
    }

    impl_pass_span!("cfg_strip");
}
