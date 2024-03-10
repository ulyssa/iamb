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
        rustNightly = pkgs.rust-bin.nightly."2024-03-08".default;
      in
      with pkgs;
      {
        packages.default = rustPlatform.buildRustPackage {
          pname = "iamb";
          version = self.shortRev or self.dirtyShortRev;
          src = ./.;
          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          nativeBuildInputs = [ pkg-config ];
          buildInputs = [ openssl ] ++ lib.optionals stdenv.isDarwin
            (with darwin.apple_sdk.frameworks; [ AppKit Security ]);
        };

        devShell = mkShell {
          buildInputs = [
            (rustNightly.override {
              extensions = [ "rust-src" "rust-analyzer-preview" "rustfmt" "clippy" ];
            })
            pkg-config
            cargo-tarpaulin
            cargo-watch
          ];
        };
      });
}
