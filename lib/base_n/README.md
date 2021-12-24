# base_n


A simple base_n implementation lifted from [rustc](https://github.com/rust-lang/rust/blob/master/compiler/rustc_data_structures/src/base_n.rs).
This library converts unsigned integers into a string representation with some base.
Bases up to and including 36 can be used for case-insensitive things.
Note that this implementation is not necessary compatible with common standard used for base32/base64 and does not allow decoding.
Instead this library primary aims to offer a simple flexible implementation and fast compile times.


