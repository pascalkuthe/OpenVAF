#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-PF2ECFA96CSZ0QMJYG7SK3RP2SH8D55XQKM74PN1W8BTGH58FQYG.tar.zst vendor-PF2ECFA96CSZ0QMJYG7SK3RP2SH8D55XQKM74PN1W8BTGH58FQYG.tar.zst; then
    tar --zstd -xf "vendor-PF2ECFA96CSZ0QMJYG7SK3RP2SH8D55XQKM74PN1W8BTGH58FQYG.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi