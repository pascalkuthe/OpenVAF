#!/bin/bash
set -ex

CI=1 cargo machete
CI=1 cargo clippy --all-targets -- -D warnings
CI=1 cargo nextest run
typos -w
git diff-files --quiet \
  || (echo "The working directory is dirty commit or stash your changes" \
  && exit 1)
