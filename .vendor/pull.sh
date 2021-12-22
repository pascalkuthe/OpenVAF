#!/usr/bin/env bash 
if s3cmd get s3://openva/vendor-Y4BK83D0GE5MMY000HVK7K0R25CQEA5GWPR0JSB6C3KW31D79XQ0.tar.zst vendor-Y4BK83D0GE5MMY000HVK7K0R25CQEA5GWPR0JSB6C3KW31D79XQ0.tar.zst; then
    tar --zstd -xf "vendor-Y4BK83D0GE5MMY000HVK7K0R25CQEA5GWPR0JSB6C3KW31D79XQ0.tar.zst"
    found=1
fi
if [ -f ~/.s3cfg ] || [ -z ${found+x} ]; then
    echo -e "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> ./.cargo/config
    echo -e "\n[net]\noffline=true" >> ./.cargo/config
fi