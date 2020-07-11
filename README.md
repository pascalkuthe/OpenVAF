# OpenVAF

A framework that allows implementing compilers for VerilogA aimed predominently at compact modelling written in Rust.
The aim of this Project is to provide a high quality fully standard compliant compiler frontend for VerilogA.
Furthermore it aims to bring modern compiler construction algorithms/data structures to a field with a lack of such tooling allowing the creation of static analysis tools, (JIT) compilers for the use in the field.
While OpenVAF aims to be an independent libary it was primarly created for the use in [VerilogAE](https://dspom.gitlab.io/verilogae/). As such demonstration of the practical capabilities of OpenVAF can be found there.

Furthermore note that this Project has not yet reached a 1.0 release and is still in active development as such the public API may change in the future.
Some highlights of OpenVAF include:
* High quality diagnostic messages
* A lining framework (similar to rustc) built on this framework
* A Data flow analysis framework (currently a reaching definitions algorithm is implemented)
* Algorithms to construct control dependence graph (combined with reaching definitions this allows construction of a program dependence graph)
* A state of the art backward slicing algorithm using the program dependence graph
* Simple constant folding 
* A backend to automatically generate rust code in procedural macros or for build script
* High performance (even for complex model such as HICUM generating multiple program slices takes ~ 70ms including codegen and io on an i7 6700k)
* Automatic derivative calculation (currently requires that the variable and the unknown it is derived by is known. A forward autodiff algorithem will be added in the future)

#Acknowledgement

[rustc](https://github.com/rust-lang/rust/) has heavily inspired the design of this compiler. Some code has even benn from rustc (marked appropriately in sourcecode comments) to avoid needless rewrites.
