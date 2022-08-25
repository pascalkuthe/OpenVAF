use anyhow::Result;

use crate::{Circuit, ExprArena, ExprEvalCtx};

#[test]
fn smoke_test() -> Result<()> {
    let mut earena = ExprArena::new();
    let mut circ = Circuit::new("test_circ".to_owned(), &mut earena);

    let gnd = circ.lookup_node("ground").expect("ground node");
    let node_x = circ.node("X".to_owned());

    let (vsrc1, _) =
        circ.new_device_instance_by_name("vsrc1".to_owned(), "vsource", vec![node_x, gnd])?;
    circ.set_instance_param(vsrc1, "dc", 1f64.into())?;

    let (res1, _) =
        circ.new_device_instance_by_name("res1".to_owned(), "resistor", vec![node_x, gnd])?;
    circ.set_instance_param(res1, "r", 1e3.into())?;

    let mut sim = circ.prepare_simulation(ExprEvalCtx::new(&earena), &earena)?;
    let solution = sim.find_dc_solution()?;
    assert_eq!(solution[gnd], 0.0);
    assert_eq!(solution[node_x], 1.0);
    let curr = sim.dc_lead_current(vsrc1)[0];
    assert_eq!(curr, -1e-3);

    Ok(())
}
