{
  description = "Quirky - Simple, flexible health monitoring";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        quirkyBinary = pkgs.haskell.lib.justStaticExecutables (
          haskellPackages.callCabal2nix "quirky" ./. {}
        );

        # Core action dependencies (always included)
        coreActionDeps = with pkgs; [
          coreutils  # df, date, nproc, etc.
          gawk
          gnused
          gnugrep
          jq
          bc
          findutils
        ];

        # Build frontend
        frontend = pkgs.buildNpmPackage {
          pname = "quirky-frontend";
          version = "1.0.0";
          src = ./web;

          npmDepsHash = "sha256-yPcwdzrHman/KOdoSPl9FPjl0ZfhGgb4AeqwxDoGEpA=";

          buildPhase = ''
            npm run build
          '';

          installPhase = ''
            mkdir -p $out
            cp -r dist/* $out/
          '';
        };

        quirky = pkgs.stdenv.mkDerivation {
          pname = "quirky";
          version = "0.1.0.0";
          src = ./.;

          nativeBuildInputs = [ pkgs.makeWrapper ];
          buildInputs = [ quirkyBinary ];

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/share/quirky/actions
            mkdir -p $out/share/quirky/web

            # Copy action library and wrap scripts with dependencies
            cp -r actions/* $out/share/quirky/actions/
            chmod +x $out/share/quirky/actions/**/*.sh

            # Wrap each shell script with core dependencies
            for script in $out/share/quirky/actions/**/*.sh; do
              wrapProgram $script \
                --prefix PATH : ${pkgs.lib.makeBinPath coreActionDeps}
            done

            # Copy the binary and wrap it with PATH to dependencies
            cp ${quirkyBinary}/bin/quirky $out/bin/quirky
            wrapProgram $out/bin/quirky \
              --prefix PATH : ${pkgs.lib.makeBinPath coreActionDeps} \
              --set QUIRKY_ACTION_PATH $out/share/quirky/actions

            # Copy frontend build
            cp -r ${frontend}/* $out/share/quirky/web/
          '';
        };

        # Define quirky package from cabal file
        quirkyPkg = haskellPackages.callCabal2nix "quirky" ./. {};

      in {
        packages = {
          default = quirky;
          quirky = quirky;
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ quirkyPkg ];

          withHoogle = true;

          nativeBuildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            nodejs_24
          ];

          buildInputs = with pkgs; [
            zlib
            zlib.dev
          ];

          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
            echo "Quirky development environment loaded"
            echo "All Haskell dependencies available from nixpkgs cache"
            echo ""
            echo "Available packages: $(ghc-pkg list 2>/dev/null | wc -l) packages"
            echo "Run 'cabal build' to build with cached dependencies"
          '';
        };
      }
    ) // {
      nixosModules.default = import ./nix/module.nix;
    };
}
