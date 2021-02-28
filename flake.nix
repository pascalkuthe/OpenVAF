{
  description = "Flake for the VerilogAE compiler: A compiler for VerilogA files that generates a python interface for compact model extraction";


  inputs.gitignore = {
    type = "github";
    owner = "hercules-ci";
    repo = "gitignore.nix";
    flake = false;
  };

  inputs.mozilla = {
    type = "github";
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    flake = false;
  };

  inputs.crate2nixGit = {
    type = "github";
    owner = "kolloch";
    repo = "crate2nix";
    flake = false;
  };


  outputs = { self, nixpkgs, gitignore, mozilla, crate2nixGit, ... }:
    let

      nameValuePair = name: value: { inherit name value; };
      genAttrs = names: f: builtins.listToAttrs (map (n: nameValuePair n (f n)) names);
      allSystems = [ "x86_64-linux" "aarch64-linux" "i686-linux" "x86_64-darwin" ];

      rustOverlay = final: prev:
        let
          rustChannel = prev.rustChannelOf {
            # date = "2021-02-16";
            channel = "beta";
            sha256 = "sha256-ODUOh79XvmQ99BGd7Xjk45hrcK4kg5yM9rotvLhd5hs=";
          };
        in
          {
            inherit rustChannel;
            rustc = rustChannel.rust;
            cargo = rustChannel.rust;
            crate2nix = final.callPackage import crate2nixGit { pkgs = final; };
          };

      forSystems = systems: f: genAttrs systems (
        system: f rec {
          inherit system;
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (import "${mozilla}/rust-overlay.nix")
              rustOverlay
            ];
          };
          dep = import nix/dependencies.nix { inherit pkgs; };
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

              inherit buildInputs LLVM_SYS_110_PREFIX;


              nativeBuildInputs = with pkgs; dep.nativeBuildInputs ++ [
                (rustChannel.rust.override { extensions = [ "rust-src" ]; })
                crate2nix
                cargo-outdated
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
