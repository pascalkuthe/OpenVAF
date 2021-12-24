#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-dqammx0gyltzg8ei8o7g7k32w.tar.zst vendor-dqammx0gyltzg8ei8o7g7k32w.tar.zst; then
    tar --zstd -xf "vendor-dqammx0gyltzg8ei8o7g7k32w.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi