{ pkgs }:

with pkgs;
{
  buildInputs = let
    mkStatic = stdenv.lib.flip stdenv.lib.overrideDerivation (
      o: {
        dontDisableStatic = true;
        configureFlags = stdenv.lib.toList (o.configureFlags or []) ++ [ "--enable-static" ];
        buildInputs = map mkStatic (o.buildInputs or []);
        propagatedBuildInputs = map mkStatic (o.propagatedBuildInputs or []);
      }
    );
  in
    map mkStatic [ libffi ncurses ] ++ [ llvmPackages_latest.llvm libxml2 zlib.static ];

  nativeBuildInputs = [
    gcc
    cmake
  ];

  LLVM_SYS_110_PREFIX = "${llvmPackages_latest.llvm}";
}
