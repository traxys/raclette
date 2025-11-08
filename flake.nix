{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.fenix = {
    url = "github:nix-community/fenix";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.rust-analyzer-src.follows = "";
  };
  inputs.naersk.url = "github:nix-community/naersk";
  inputs.hyperfine = {
    url = "github:sharkdp/hyperfine";
    flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      fenix,
      naersk,
      hyperfine,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        rust = fenix.packages.${system}.stable.defaultToolchain;

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

          propagatedBuildInputs = with pkgs.python3Packages; [
            numpy
            matplotlib
            scipy
          ];
        };
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            (with pkgs; [
              bashInteractive
              cargo-fuzz
              cargo-flamegraph
              cargo-criterion
              cargo-tarpaulin
              glow
              m4
            ])
            ++ [
              pkgs.hyperfine
              hf-scripts
              rust
              (pkgs.python3.withPackages (
                ps: with ps; [
                  toml
                  pyyaml
                ]
              ))
            ];
          buildInputs = [ ];
        };

        packages = rec {
          raclette = default;
          default = naersk'.buildPackage {
            src = ./.;
            nativeBuildInputs = [ pkgs.m4 ];
          };
        };
      }
    );
}
