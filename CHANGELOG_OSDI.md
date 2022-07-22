# Changelog - OpenVAF OSDI

All notable changes to OpenVAF relevant to OSDI will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Added

* Added support for the `analysis` function (always returned true previously)
* Automatically remove unused nodes (includes noise network until noise support is added)

### Fixed

* do not generate unkowns for noise phase shifts with ddt
* provide errors instead of crashing for unsupported Verilog-A builtins

## 0.1.2 - 2022-07-19

### Added

* Added support for `$limit`
* Added support for `$abstime` (now returns a non-zero value for large signal simulations).
* Added support for `$bound_step` (now the simulator can read the minimum value passed to `$bound_step`).
* Add general description to the start of the help text

### Changed

* Validate format arguments and properly handle all argument types instead of just passing the format specifier to C

## 0.1.1 - 2022-06-26

### Changed

* Refactor the algorithm that determines whether `ddt` needs an unkown, so that OpenVAF doesn't create one when not required.
* Automatically add whitespace between auto generated format characters for `$strope etc.`
* Check that fmt literals used in $display are correct and match the format arguments to avoid crashes/undefined behavior.

### Fixed

* `ddx` always returned 0
* Remove infinite loop during global value numbering optimization.
* Linking error if cache directory was missing during batch compilation.
* Fixed crash for nodes without any contributions besides collapsing
* Fixed select expressions producing incorrect values
* Fixed that the cache-dir argument was called chache-dir (typo)


## 0.1.0 - 2022-06-20

Initial prototype release
