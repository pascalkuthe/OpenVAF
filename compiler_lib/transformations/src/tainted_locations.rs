use openvaf_data_structures::BitSet;
use openvaf_middle::cfg::{CfgPass, ControlFlowGraph, InternedLocations, LocationId};
use openvaf_middle::{impl_pass_span, CallType, StmntKind};

struct TaintedLocations<'a, F> {
    is_tainted: F,
    locations: &'a InternedLocations,
}

impl<'a, C, F> CfgPass<'_, C> for TaintedLocations<'a, F>
where
    C: CallType,
    F: FnMut(&StmntKind<C>) -> bool,
{
    type Result = BitSet<LocationId>;

    fn run(mut self, cfg: &mut ControlFlowGraph<C>) -> Self::Result {
        let mut tainted_locations = BitSet::new_empty(self.locations.len_idx());
        for (bb, data) in cfg.blocks.iter_enumerated() {
            let mut stmnt = self.locations[bb].stmnt_start;
            let tainted_locations_in_bb = data.statements.iter().filter_map(|(info, _)| {
                let res = if (self.is_tainted)(info) {
                    Some(stmnt)
                } else {
                    None
                };
                stmnt += 1;
                res
            });

            tainted_locations.extend(tainted_locations_in_bb)
        }

        tainted_locations
    }

    impl_pass_span!("tainted_locations");
}
