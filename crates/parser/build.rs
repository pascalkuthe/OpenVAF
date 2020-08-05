extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
        .log_verbose()
        .use_cargo_dir_conventions()
        .emit_rerun_directives(true)
        .process()
        .unwrap();
}
