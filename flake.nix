{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # We only need the nightly overlay in the devShell because .rs files are formatted with nightly.
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        rustNightly = pkgs.rust-bin.nightly."2025-10-15".default;
      in
      with pkgs;
      rec {
        packages.default = iamb.override (o: {
          rustPlatform = o.rustPlatform // {
            buildRustPackage =
              args:
              rustPlatform.buildRustPackage (
                args
                // rec {
                  version = self.shortRev or self.dirtyShortRev;
                  src = builtins.path {
                    path = ./.;
                    name = "iamb-${version}-source";
                  };
                  cargoLock.lockFile = ./Cargo.lock;
                  env.VERGEN_GIT_SHA = version;
                }
              );
          };
        });

        devShell = mkShell {
          inputsFrom = [ packages.default ];

          packages = [
            (lib.hiPrio (
              rustNightly.override {
                extensions = [
                  "rust-src"
                  "rust-analyzer-preview"
                  "rustfmt"
                  "clippy"
                ];
              }
            ))
            cargo-tarpaulin
            cargo-watch
            sqlite
          ];
        };
      }
    );
}
