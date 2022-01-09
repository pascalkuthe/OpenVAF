#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-4gqsw2ptwd3wmn27nti3xd7qu.tar.zst vendor-4gqsw2ptwd3wmn27nti3xd7qu.tar.zst; then
    tar --zstd -xf "vendor-4gqsw2ptwd3wmn27nti3xd7qu.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.\"https://github.com/DSPOM2/salsa\"]\ngit = \"https://github.com/DSPOM2/salsa\"\nbranch = \"patch-1\"\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi