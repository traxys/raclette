{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.rust-overlay.url = "github:oxalica/rust-overlay";
  inputs.naersk.url = "github:nix-community/naersk";
  inputs.hyperfine = {
    url = "github:sharkdp/hyperfine";
    flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    naersk,
    hyperfine,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;

        overlays = [(import rust-overlay)];
      };
      rust = pkgs.rust-bin.stable.latest.default.override {
        extensions = ["llvm-tools-preview" "rust-src"];
      };

      naersk' = pkgs.callPackage naersk {
        cargo = rust;
        rustc = rust;
      };

      hf-scripts = pkgs.stdenv.mkDerivation {
        pname = "hyperfine-scripts";
        version = "1.14.0";

        src = "${hyperfine}/scripts";

        installPhase = ''
          mkdir -p $out/bin

          for script in *.py; do
          	cp "$script" $out/bin/hf-$(basename "$script" .py)
          done
        '';

        propagatedBuildInputs = [
          pkgs.python3Packages.numpy
          pkgs.python3Packages.matplotlib
          pkgs.python3Packages.scipy
        ];
      };
    in {
      devShell = pkgs.mkShell {
        nativeBuildInputs = [
          pkgs.bashInteractive
          pkgs.cargo-fuzz
          pkgs.rustup
          pkgs.cargo-flamegraph
          pkgs.cargo-criterion
          pkgs.hyperfine
          hf-scripts
          rust
        ];
        buildInputs = [];
      };

      defaultPackage = naersk'.buildPackage {src = ./.;};
    });
}
