# default.nix — Jotain Emacs distribution.
#
# Wraps the Emacs build (emacs.nix) with all available tree-sitter
# grammars from nixpkgs.
#
# Usage:
#   nix-build                                          # Emacs + every grammar
#   nix-build --arg withTreeSitterGrammars false       # Emacs without grammars
#
# All emacs.nix arguments are forwarded:
#   nix-build --arg withPgtk true                      # pgtk + grammars
#   nix-build --arg variant '"git"'                    # git master + grammars
#   nix-build --arg noGui true                         # terminal-only + grammars
#
# nixpkgs is taken from the npins-pinned `nixpkgs-unstable' channel by
# default; pass --arg pkgs '<nixpkgs>' or override `pkgs` to use a
# different one.
{
  system ? builtins.currentSystem,
  pkgs ? import (import ./npins).nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  },
  withTreeSitterGrammars ? true,
  # All other arguments are forwarded to emacs.nix
  ...
}@args:

let
  lib = pkgs.lib;

  # Build the base Emacs from emacs.nix, forwarding every argument
  # except the ones default.nix consumes itself.
  emacsArgs = builtins.removeAttrs args [ "withTreeSitterGrammars" ];
  emacs = import ./emacs.nix emacsArgs;

in
if withTreeSitterGrammars then
  (pkgs.emacsPackagesFor emacs).withPackages (epkgs: [
    epkgs.treesit-grammars.with-all-grammars
  ])
else
  emacs
