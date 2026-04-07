# default.nix — Jotain Emacs distribution
#
# Wraps the Emacs build (emacs.nix) with packages and tree-sitter grammars.
#
# Usage:
#   nix-build                                          # Emacs + all tree-sitter grammars
#   nix-build --arg withTreeSitterGrammars false       # Emacs without grammars
#
# All emacs.nix arguments are forwarded:
#   nix-build --arg withPgtk true                      # pgtk + grammars
#   nix-build --arg variant '"git"'                    # git master + grammars
#   nix-build --arg noGui true                         # terminal-only + grammars
{
  system ? builtins.currentSystem,
  pkgs ? import <nixpkgs> { inherit system; },
  withTreeSitterGrammars ? true,
  # All other arguments are forwarded to emacs.nix
  ...
}@args:

let
  lib = pkgs.lib;

  # Build the base Emacs from emacs.nix, forwarding all args except our own
  emacsArgs = builtins.removeAttrs args [ "withTreeSitterGrammars" ];
  emacs = import ./emacs.nix emacsArgs;

  # MELPA / ELPA packages that init.el expects.  Provided via Nix so
  # the wrapper is reproducible — :ensure t in init.el becomes a
  # no-op because the packages are already on the load-path.
  jotainEmacsPackages =
    epkgs: with epkgs; [
      magit
      nix-ts-mode
      markdown-mode
      wakatime-mode
      activity-watch-mode
      keyfreq
      org-clock-csv
    ];

  # All tree-sitter grammars for Emacs (275 grammars from nixpkgs)
  # Uses the NixOS-recommended emacsPackages.treesit-grammars mechanism:
  # https://wiki.nixos.org/wiki/Emacs
  allTreeSitterGrammars =
    epkgs:
    epkgs.treesit-grammars.with-grammars (
      grammars: builtins.attrValues (lib.filterAttrs (n: _: lib.hasPrefix "tree-sitter-" n) grammars)
    );

in
(pkgs.emacsPackagesFor emacs).withPackages (
  epkgs:
  (jotainEmacsPackages epkgs) ++ lib.optional withTreeSitterGrammars (allTreeSitterGrammars epkgs)
)
