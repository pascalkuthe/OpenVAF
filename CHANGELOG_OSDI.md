# Changelog - OpenVAF OSDI

All notable changes to OpenVAF relevant to OSDI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [unreleased]

### Added

* Added support for `$abstime` (now returns a non-zero value for large signal simulations).
* Added support for `$bound_step` (now the simulator can read the minium value passed to `$bound_step`).

## 0.1.1 - 2022-06-26

### Changed

* Refactor the algorithm that determines whether `ddt` needs an unkown, so that OpenVAF doesn't create one when not required.
* automatically add whitespace between auto generated format characters for `$strope etc.`

### Fixed

* Remove infinite loop during global value numbering optimization.
* Linking error if cache directory was missing during batch compilation.
* Fixed crash for nodes without any contributions besides collapsing
* Fixed select expressions producing incorrect values


## 0.1.0 - 2022-06-20

Initial prototype release
