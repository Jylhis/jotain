# Jotain library functions
{ lib, pkgs, ... }:

{
  # Dependency extraction utilities
  dependencies = import ./dependencies.nix { inherit lib pkgs; };

  # Runtime dependencies (external tools and LSP servers)
  runtimeDeps = import ./runtime-deps.nix { inherit pkgs; };

  # Additional utility functions can be added here
}
