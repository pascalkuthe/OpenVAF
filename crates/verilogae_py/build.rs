fn main() {
    pyo3_build_config::add_extension_module_link_args();
    let interpreter_config = pyo3_build_config::get();
    interpreter_config.emit_pyo3_cfgs();
}
