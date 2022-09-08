# Melange

Melange is an experimental circuit simulator that leverage OpenVAF to gain access to compact models.
This allows it to easily support a large array of models without the huge effort associated with traditional simulators.
The focus of melange is primarily on automation.
That means melange focuses on providing an ergonomic and extensible API in mainstream programming languages (python and rust currently) instead of a special purpose netlist format.
However, to remain compatible with existing PDKs a subset of the spectre netlist format can be parsed.

Melange is currently in early development and most features (most notably the python API) are not complete.
Some mockups of planned usage in python can be found in examples/melange.
A working minimal example (in rust) can be found in crates/melange/test.rs
