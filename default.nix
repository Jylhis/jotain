# default.nix — Jotain Emacs distribution.
#
# Wraps the Emacs build (emacs.nix) with:
#
#   • every Emacs Lisp package declared via `(use-package ...)` in any
#     file under ./lisp (auto-extracted — see nix/use-package.nix), and
#   • all available tree-sitter grammars from nixpkgs.
#
# Usage:
#   nix-build                                          # Emacs + mapper + grammars
#   nix-build --arg withAutoPackages false             # Emacs + grammars only
#   nix-build --arg withTreeSitterGrammars false       # Emacs + mapper only
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
  withAutoPackages ? true,
  # All other arguments are forwarded to emacs.nix
  ...
}@args:

let
  lib = pkgs.lib;

  # Build the base Emacs from emacs.nix, forwarding every argument
  # except the ones default.nix consumes itself.
  emacsArgs = builtins.removeAttrs args [
    "withTreeSitterGrammars"
    "withAutoPackages"
  ];
  emacs = import ./emacs.nix emacsArgs;

  usePackage = import ./nix/use-package.nix { inherit lib; };

  extraPackages = import ./nix/extra-packages.nix { inherit (pkgs) fetchFromGitHub; };

  extraEmacsPackages =
    epkgs:
    # Packages whose `use-package` form uses `:ensure nil` to stop
    # package.el from fetching them. The auto-mapper skips `:ensure
    # nil` blocks on purpose, so we re-inject the Nix-exclusive ones
    # by hand.
    [
      epkgs.claude-code-ide
      epkgs.combobulate
    ]
    ++ lib.optionals withTreeSitterGrammars [
      epkgs.treesit-grammars.with-all-grammars
    ];

in
if withAutoPackages then
  usePackage.emacsWithPackagesFromUsePackage {
    config = ./lisp;
    package = emacs;
    emacsPackagesFor = pkgs.emacsPackagesFor;
    override = extraPackages;
    inherit extraEmacsPackages;
  }
else if withTreeSitterGrammars then
  (pkgs.emacsPackagesFor emacs).withPackages extraEmacsPackages
else
  emacs
