#!/bin/bash
set -ex

export CI=1

cargo machete
cargo clippy --all-targets -- -D warnings
cargo nextest run
git diff-files --quiet \
  || (echo "The working directory is dirty commit or stash your changes" \
  && exit 1)
