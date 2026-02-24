{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ self, ... }:
  let
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux"; overlays = [ self.overlays.this ];
    };
  in {

    overlays.this =
      let
        hsOverlay =  pkgs: hself: hsuper: {
          all-cabal-tool = hself.callPackage ./nix/packages/all-cabal-tool.nix {};
          Cabal = hself.callPackage ./nix/packages/Cabal.nix {};
          Cabal-syntax = hself.callPackage ./nix/packages/Cabal-syntax.nix {};
          # Tests require being run from within the git repo. Disable.
          hit = pkgs.haskell.lib.dontCheck (hself.callPackage ./nix/packages/hit.nix {});
        };
      in final: prev: {
        myHaskellPackages = prev.haskellPackages.override {
          overrides = hsOverlay final;
        };
      };

    packages.x86_64-linux.default = self.packages.x86_64-linux.all-cabal-tool;
    packages.x86_64-linux.all-cabal-tool = pkgs.myHaskellPackages.all-cabal-tool;

    packages.x86_64-linux.gen-packages = pkgs.writeShellApplication {
      name = "gen-packages";
      text = builtins.readFile ./nix/scripts/gen-packages.sh;
      runtimeInputs = [
        pkgs.cabal-install
        pkgs.cabal2nix
      ];
    };

    packages.x86_64-linux.sync-lts = pkgs.writeShellApplication {
      name = "sync-lts";
      text = builtins.readFile ./nix/scripts/sync-lts.sh;
      runtimeInputs = [
        pkgs.coreutils
        pkgs.jq
        pkgs.gh
        pkgs.gnused
        pkgs.gnugrep
      ];
    };

    devShells.x86_64-linux.stack = pkgs.mkShell {
      nativeBuildInputs = [
        pkgs.haskellPackages.ghc
        pkgs.zlib
      ];
    };
    devShells.x86_64-linux.default = pkgs.myHaskellPackages.shellFor {
      packages = hpkgs: [ hpkgs.all-cabal-tool ] ;
      nativeBuildInputs = [
        pkgs.cabal-install
        pkgs.haskell-language-server
      ];
    };
  };
}
