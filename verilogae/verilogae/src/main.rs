use verilogae::api::Opts;
use verilogae::load;

fn main() {
    let arg = std::env::args().nth(1).unwrap();
    load(arg.as_ref(), true, &Opts::default()).unwrap();
}
