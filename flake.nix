{
  description = "iamb";
  nixConfig.bash-prompt = "\[nix-develop\]$ ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # We only need the nightly overlay in the devShell because .rs files are formatted with nightly.
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        rustNightly = pkgs.rust-bin.nightly."2023-03-17".default;
      in 
      with pkgs;
      {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "iamb";
          version = "0.0.7";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
          nativeBuildInputs = [ pkgs.pkgconfig ];
          buildInputs = [ pkgs.openssl ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
            (with pkgs.darwin.apple_sdk.frameworks; [ AppKit Security ]);
        };
        devShell = mkShell {
          buildInputs = [
            (rustNightly.override { extensions = [ "rust-src" ]; })
            pkg-config
            cargo-tarpaulin
            rust-analyzer
            rustfmt
          ];
        };
      });
}