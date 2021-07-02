FROM centos:7

#ENV RUST_VERSION class="download-link">Version
ENV HOME /home
ENV CARGO_HOME /usr/local/cargo
ENV PATH $CARGO_HOME/bin:$HOME/.cargo/bin:$PATH

RUN mkdir -p "$CARGO_HOME"
RUN curl -sSf https://sh.rustup.rs \
  | env -u CARGO_HOME sh -s -- -y

RUN rustup --version; \
    rustc --version; \
    cargo --version;


RUN yum install git -y;\
    git clone --depth 1 --recursive --branch llvmorg-12.0.0 https://github.com/llvm/llvm-project;

RUN yum install centos-release-scl epel-release -y;\
    yum install -y  scl-utils scl-utils-build;\
    yum install llvm-toolset-7.0 devtoolset-9 cmake3 git ninja-build -y;

RUN cargo install cargo-rpm;

RUN source scl_source enable devtoolset-9;\
    source scl_source enable llvm-toolset-7.0;\
    mkdir llvmbuild;\
    cd llvmbuild;\
    cmake3 -GNinja -DLLVM_PARALLEL_LINK_JOBS=1 -DCMAKE_BUILD_TYPE=RELEASE -DLLVM_ENABLE_PROJECTS='polly;lld' -DLLVM_ENABLE_LLD=ON /llvm-project/llvm;\
    cmake3 --build . --target install --parallel 8 --config Release;\
    rm -rf /llvm-project;

RUN yum install libffi-devel rpm-build -y;

RUN rustup component add clippy rustfmt;
