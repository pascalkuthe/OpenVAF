#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-1R9APSEH00SRYCEX98ZYN3MAVXTTDCPRY5A6TD0SV9QN3ER7Z10G.tar.zst vendor-1R9APSEH00SRYCEX98ZYN3MAVXTTDCPRY5A6TD0SV9QN3ER7Z10G.tar.zst; then
    tar --zstd -xf "vendor-1R9APSEH00SRYCEX98ZYN3MAVXTTDCPRY5A6TD0SV9QN3ER7Z10G.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi