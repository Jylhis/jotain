# default.nix — Jotain Emacs distribution.
#
# Returns a structured attrset:
#   packages.default             — full Emacs distribution
#   overlays.default             — nixpkgs overlay (see overlay.nix)
#   homeManagerModules.default   — Home Manager module (see module.nix)
#   lib                          — use-package scanner utilities
#
# Usage:
#   nix-build -A packages.default                    # full distribution
#   nix-build emacs.nix                              # bare Emacs (cache-parity)
#   nix-build emacs.nix --arg withPgtk true          # variant builds
{
  pkgs ? import (import ./npins).nixpkgs {
    overlays = [ (import ./overlay.nix) ];
    config.allowUnfree = true;
  },
  lib ? pkgs.lib,
}:
let
  overlay = import ./overlay.nix;
  # Apply overlay if caller's pkgs doesn't have it
  # (e.g. module.nix passes Home Manager's pkgs without the overlay)
  pkgs' = if pkgs ? jotainEmacsPackages then pkgs else pkgs.extend overlay;
in
{
  packages.default = pkgs'.jotainEmacsPackages;

  overlays.default = overlay;

  homeManagerModules.default = import ./module.nix;

  lib = import ./nix/use-package.nix { inherit lib; };
}
