# overlay.nix — Nixpkgs overlay for Jotain Emacs.
#
# Adds:
#   jotainEmacs           — bare Emacs binary (emacs.nix defaults)
#   jylhisEmacs           — bare Emacs from github:jylhis/emacs
#   jotainInfo            — Jotain manual (share/info/jotain.info + dir)
#   jotainEmacsPackages   — full distribution using jotainEmacs
#   jylhisEmacsPackages   — full distribution using jylhisEmacs
#
# Usage:
#   import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }
import ./nix/mk-overlay.nix { }
