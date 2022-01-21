FROM quay.io/pypa/manylinux2014_x86_64 as llvm-builder


RUN yum install git -y;

RUN yum install centos-release-scl epel-release -y;\
    yum install -y  scl-utils scl-utils-build;\
    yum install llvm-toolset-7.0 devtoolset-9 cmake3 git ninja-build  libffi-devel -y;

RUN git clone --depth 1 --single-branch --branch llvmorg-13.0.0 https://github.com/llvm/llvm-project;

RUN source scl_source enable devtoolset-9;\
    source scl_source enable llvm-toolset-7.0;\
    mkdir llvmbuild;\
    cd llvmbuild;\
    cmake3 -GNinja -DLLVM_PARALLEL_LINK_JOBS=1 -DCMAKE_BUILD_TYPE=RELEASE -DLLVM_ENABLE_LLD=ON -DCMAKE_INSTALL_PREFIX=/llvm /llvm-project/llvm;

RUN source scl_source enable devtoolset-9;\
    source scl_source enable llvm-toolset-7.0;\
    cd llvmbuild;\
    cmake3 --build . --target install --parallel 14 --config Release


FROM quay.io/pypa/manylinux2014_x86_64


ENV PATH /root/.cargo/bin:$PATH

# Add all supported python versions
ENV PATH /opt/python/cp37-cp37m/bin/:/opt/python/cp38-cp38/bin/:/opt/python/cp39-cp39/bin/:/opt/python/cp310-cp310/bin/:/llvm/bin:/opt/python/pp37-pypy37_pp73/bin/:/opt/python/pp38-pypy87_pp73/bin/:$PATH

# Otherwise `cargo new` errors
ENV USER root

COPY --from=llvm-builder /llvm /llvm

# Use an explicit version to actually install the version we require instead of using the cache
# It would be even cooler to invalidate the cache depending on when the official rust image changes,
# but I don't know how to do that
RUN curl --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain 1.58.0 -y

RUN python3.7 -m pip install --no-cache-dir setuptools-rust numpy\
    && python3.8 -m pip install --no-cache-dir setuptools-rust numpy \
    && python3.9 -m pip install --no-cache-dir setuptools-rust numpy \
    && python3.10 -m pip install --no-cache-dir setuptools-rust numpy\
    && pypy3.7 -m pip install --no-cache-dir setuptools-rust numpy\
    && pypy3.8 -m pip install --no-cache-dir setuptools-rust numpy;

Run pip install twine

WORKDIR /io
