#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-278atro83ch53ruzrq3i5mune.tar.zst vendor-278atro83ch53ruzrq3i5mune.tar.zst; then
    tar --zstd -xf "vendor-278atro83ch53ruzrq3i5mune.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.\"https://github.com/DSPOM2/pyo3\"]\ngit = \"https://github.com/DSPOM2/pyo3\"\nreplace-with = \"vendored-sources\"\n\n[source.\"https://github.com/DSPOM2/salsa\"]\ngit = \"https://github.com/DSPOM2/salsa\"\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi