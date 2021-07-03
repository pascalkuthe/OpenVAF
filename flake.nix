{
  description = "Flake for the VerilogAE compiler: A compiler for VerilogA files that generates a python interface for compact model extraction";


  inputs.gitignore = {
    type = "github";
    owner = "hercules-ci";
    repo = "gitignore.nix";
    flake = false;
  };

  inputs.rust = {
    type = "github";
    owner = "oxalica";
    repo = "rust-overlay";
  };



  outputs = { self, nixpkgs, gitignore, rust,  ... }:
    let

      nameValuePair = name: value: { inherit name value; };
      genAttrs = names: f: builtins.listToAttrs (map (n: nameValuePair n (f n)) names);
      allSystems = [ "x86_64-linux" "aarch64-linux" "i686-linux" "x86_64-darwin" ];


      forSystems = systems: f: genAttrs systems (
        system: f rec {
          inherit system;
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              rust.overlay
            ];
          };
          dep = import nix/dependencies.nix { inherit pkgs; lib=nixpkgs.lib; };
        }
      );


      forAllSystems = f: forSystems allSystems f;
    in
      {
        devShell = forAllSystems (
          { pkgs, dep, ... }:
            with dep;
            pkgs.mkShell {
              name = "openvaf";

              inherit buildInputs LLVM_SYS_120_PREFIX LLD_LIB_DIR LIBCLANG_PATH;


              nativeBuildInputs = with pkgs; dep.nativeBuildInputs ++ [
                rust-bin.stable.latest.default
                crate2nix
                cargo-outdated
                cargo-edit
                cargo-flamegraph
                # callPackage import crate2nixGit { pkgs = final; } {}
                tokei
              ];
            }
        );

        # packages =
        #   forAllSystems
        #     (
        #       { system, pkgs, dep, ... }:
        #         let
        #           cargoNix = import ./Cargo.nix {
        #             inherit pkgs;
        #             defaultCrateOverrides = pkgs.defaultCrateOverrides // {
        #               llvm-sys = { ... }:
        #                 with dep;
        #                 {
        #                   inherit buildInputs LLVM_SYS_110_PREFIX;
        #                 };
        #               libmimalloc-sys = { ... }:
        #                 with dep;
        #                 {
        #                   inherit nativeBuildInputs;
        #                 };

        #               verilogae = { ... }:
        #                 with dep;
        #                 {
        #                   inherit buildInputs;
        #                 };
        #             };
        #           };

        #         in
        #           {
        #             openvaf = cargoNix.workspaceMembers.verilogae.build;
        #           }
        #     );



        # defaultPackage = forAllSystems ({ system, ... }: self.packages.${system}.openvaf);

      };

}
