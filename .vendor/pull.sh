#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-NC2ANHCRQSNW943Z8GVTYM4AQNP8H0N4WK2HAESSB5CY8CD8BSCG.tar.zst vendor-NC2ANHCRQSNW943Z8GVTYM4AQNP8H0N4WK2HAESSB5CY8CD8BSCG.tar.zst; then
    tar --zstd -xf "vendor-NC2ANHCRQSNW943Z8GVTYM4AQNP8H0N4WK2HAESSB5CY8CD8BSCG.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi