# Update all dependencies: nixpkgs, LTS resolver, and nix package definitions
update-deps:
    nix flake update
    nix run .#sync-lts
    nix run .#gen-packages

# Update just the nixpkgs flake input
update-nixpkgs:
    nix flake update

# Sync stack.yaml resolver with nixpkgs LTS version
sync-lts:
    nix run .#sync-lts

# Regenerate nix package definitions from cabal files
gen-packages:
    nix run .#gen-packages

# Build with nix
build:
    nix build

# Build with stack
build-stack:
    stack build

# Enter development shell
dev:
    nix develop
