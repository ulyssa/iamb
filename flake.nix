{
  description = "iamb";
  nixConfig.bash-prompt = "[nix-develop]$ ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    crane,
    flake-utils,
    fenix,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;

        rustToolchain = fenix.packages.${system}.fromToolchainFile {
          file = ./rust-toolchain.toml;
          # When the file changes, this hash must be updated.
          sha256 = "sha256-SBKjxhC6zHTu0SyJwxLlQHItzMzYZ71VCWQC2hOzpRY=";
        };

        # Nightly toolchain for rustfmt (pinned to current flake lock)
        # Note that the github CI uses "current nightly" for formatting, it 's not pinned.
        rustNightly = fenix.packages.${system}.latest;

        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
        craneLibNightly = (crane.mkLib pkgs).overrideToolchain rustNightly.toolchain;

        src = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            (craneLib.fileset.commonCargoSources ./.)
            ./src/windows/welcome.md

            ./docs/iamb.1
            ./docs/iamb.5
            ./docs/iamb.svg
            ./docs/iamb.metainfo.xml
            ./iamb.desktop
          ];
        };

        commonArgs = rec {
          inherit src;
          strictDeps = true;
          pname = "iamb";
          version = self.shortRev or self.dirtyShortRev;

          VERGEN_GIT_SHA = version;
        };

        # Build *just* the cargo dependencies, so we can reuse
        # all of that work (e.g. via cachix) when running in CI
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        # Build the actual crate
        iamb = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;

          nativeBuildInputs = [ pkgs.installShellFiles ];
          postInstall = ''
            mkdir -p completions
            ./target/release/iamb --completions zsh > completions/_iamb
            ./target/release/iamb --completions bash > completions/iamb.bash
            ./target/release/iamb --completions fish > completions/iamb.fish
            installShellCompletion --cmd iamb completions/*

            installManPage $src/docs/iamb.{1,5}
            install -D $src/docs/iamb.svg -t $out/share/icons/hicolor/scalable/apps
            install -D $src/docs/iamb.metainfo.xml $out/share/appdata/chat.iamb.iamb.appdata.xml
            install -D $src/iamb.desktop -t $out/share/applications
          '';

          meta = {
            description = "Matrix client for Vim addicts";
            mainProgram = "iamb";
            homepage = "https://github.com/ulyssa/iamb";
            license = lib.licenses.asl20;
          };
        });
      in
      {
        checks = {
          # Build the crate as part of `nix flake check`
          inherit iamb;

          iamb-clippy = craneLib.cargoClippy (commonArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets -- --deny warnings";
            });

          iamb-fmt = craneLibNightly.cargoFmt {
            inherit src;
          };

          iamb-nextest = craneLib.cargoNextest (commonArgs
            // {
              inherit cargoArtifacts;
              partitions = 1;
              partitionType = "count";
            });
        };

        packages.default = iamb;

        apps.default = flake-utils.lib.mkApp {
          drv = iamb;
        };

        devShells.default = craneLib.devShell {
          # Inherit inputs from checks
          checks = self.checks.${system};

          packages = with pkgs; [
            cargo-tarpaulin
            cargo-watch
            sqlite
          ];

          shellHook = ''
            # Prepend nightly rustfmt to PATH.
            export PATH="${rustNightly.rustfmt}/bin:$PATH"
          '';
        };
      }
    );
}
