# default.nix — Jotain non-flake entry point.
#
# Packages for the current system are promoted to the top level, so:
#
#   nix-build                              # full distribution (jotainEmacsPackages)
#   nix-build -A emacs                     # bare Emacs only
#   nix-build emacs.nix                    # bare Emacs (cache-parity build)
#   nix-build emacs.nix --arg noGui true   # variant build (terminal-only)
{
  system ? builtins.currentSystem,
}:
let
  inputs = import ../../nix/inputs.nix { };
in
inputs.jotain.packages.${system}.default
