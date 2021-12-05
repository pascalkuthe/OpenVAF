use hir_def::{BranchId, NodeId};

pub enum TypeDiagnostics {
    IncompatibleBranchNodes { branch: BranchId, node: IncompatibleNodes },
}

pub struct IncompatibleNodes {
    node1: NodeId,
    node2: NodeId,
}
