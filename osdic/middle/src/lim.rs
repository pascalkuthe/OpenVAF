use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum LimFunction {
    Native(osdic_target::sim::LimFunction),
    // VerilogA TODO VerilogA Function Type
}

impl Display for LimFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LimFunction::Native(native_limfn_) => Display::fmt(native_limfn_, f),
        }
    }
}
