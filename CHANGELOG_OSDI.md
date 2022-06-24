# Changelog - OpenVAF OSDI

All notable changes to OpenVAF relevant to OSDI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Changed

* Refactor the algorithm that determines whether `ddt` needs an unkown, so that OpenVAF doesn't create one when not required.

### Fixed

* Remove infinite loop during global value numbering optimization.
* Linking error if cache directory was missing during batch compilation.
* Fixed crash for nodes without any contributions besides collapsing


## 0.1.0 - 2022-06-20

Initial prototype release
