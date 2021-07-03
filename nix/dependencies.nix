{ pkgs, lib }:

with pkgs;
{
  buildInputs = let
    mkStatic = lib.flip lib.overrideDerivation (
      o: {
        dontDisableStatic = true;
        configureFlags = stdenv.lib.toList (o.configureFlags or []) ++ [ "--enable-static" ];
        buildInputs = map mkStatic (o.buildInputs or []);
        propagatedBuildInputs = map mkStatic (o.propagatedBuildInputs or []);
      }
    );
  in
    map mkStatic [ libffi ncurses ] ++ [ llvmPackages_latest.llvm libxml2 zlib.static];

  nativeBuildInputs = [
    gcc
    cmake
    llvmPackages_latest.clang
    llvmPackages_latest.libclang
    llvmPackages_latest.lld.dev
  ];

  LLVM_SYS_120_PREFIX = "${llvmPackages_latest.llvm}";
  LIBCLANG_PATH = "${llvmPackages_latest.libclang}/lib";
  LLD_LIB_DIR = "${llvmPackages_latest.lld.dev}/lib";
}
