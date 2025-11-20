# Jotain library functions
{ lib, pkgs, ... }:

{
  # Dependency extraction utilities
  dependencies = import ./dependencies.nix { inherit lib pkgs; };

  # Additional utility functions can be added here
}
