brew install llvm@15
export PATH="/opt/homebrew/opt/llvm@15/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/llvm@15/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm@15/include"]

cargo build --release
