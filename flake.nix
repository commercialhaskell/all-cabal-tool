{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=69e7105d5d8bff9e0cb1718d4a76a54aa9210f98";
  };

  outputs = inputs@{ self, ... }:
  let
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux"; overlays = [ self.overlays.all-cabal-tool ];
    };
  in {

    overlays.all-cabal-tool =
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
