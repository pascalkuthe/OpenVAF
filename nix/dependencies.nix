{ pkgs, lib }:

with pkgs;
let llvmPackages = pkgs.llvmPackages_13; in
{
  buildInputs =
    let

      mkStatic = lib.flip lib.overrideDerivation (
        o: {
          dontDisableStatic = true;
          configureFlags = lib.toList (o.configureFlags or [ ]) ++ [ "--enable-static" ];
          buildInputs = map mkStatic (o.buildInputs or [ ]);
          propagatedBuildInputs = map mkStatic (o.propagatedBuildInputs or [ ]);
        }
      );
    in
    map mkStatic [ libffi ncurses ] ++ [ llvmPackages.llvm libxml2 zlib.static ];


  nativeBuildInputs =
    let
      bintools_lld = wrapBintoolsWith {
        bintools = llvmPackages.bintools;
      };
    in
    [
      bintools_lld
      gcc
      cmake
      llvmPackages.clang
      llvmPackages.libclang
    ];

  LIBCLANG_PATH = "${llvmPackages.libclang}/lib";
  LLD_LIB_DIR = "${llvmPackages.lld.dev}/lib";
}
