use crate::api::Opts;
use crate::load_impl;

#[test]
fn test(){
    let foo = load_impl("/home/dspom/Projects/OpenVAF/integration_tests/HICUML2/hicuml2.va".as_ref(), false, &Opts::default()).unwrap();
}
