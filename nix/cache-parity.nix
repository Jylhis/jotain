# nix/cache-parity.nix: each bare Emacs build must equal its prebuilt
# emacs-overlay / nixpkgs sibling, or its default-rev build is a cache miss.
# Linux only: the Darwin GUI is patched and never on parity.
#
#   just cache-parity
#
# `pkgs` must carry emacs-overlay and the Jotain overlay (the flake's
# pkgsFor). Consumed by the `cache-parity` check in nix/checks.nix.
{ pkgs }:
let
  bare = args: (import ../emacs.nix ({ inherit pkgs; } // args)).outPath;
in
{
  default = pkgs.jotainEmacs.outPath == pkgs.emacs-unstable-pgtk.outPath;
  bare-default = bare { } == pkgs.emacs-unstable-pgtk.outPath;
  mainline-pgtk = bare { variant = "mainline"; } == pkgs.emacs-pgtk.outPath;
  git-pgtk = bare { variant = "git"; } == pkgs.emacs-git-pgtk.outPath;
  igc-pgtk = bare { variant = "igc"; } == pkgs.emacs-igc-pgtk.outPath;
}
