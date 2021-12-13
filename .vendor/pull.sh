#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-62BAHZTZDAGFQ7N3JQD4SEACPDJXK7XQ86E1FF20BD5DXXGPPXF0.tar.zst vendor-62BAHZTZDAGFQ7N3JQD4SEACPDJXK7XQ86E1FF20BD5DXXGPPXF0.tar.zst; then
    tar --zstd -xf "vendor-62BAHZTZDAGFQ7N3JQD4SEACPDJXK7XQ86E1FF20BD5DXXGPPXF0.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi