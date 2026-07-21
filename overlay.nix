# overlay.nix — Nixpkgs overlay for Jotain Emacs.
#
# Adds:
#   jotainEmacs           — bare Emacs binary (unstable variant — the
#                           Emacs 31 pretest; see nix/mk-overlay.nix)
#   jylhisEmacs           — bare Emacs from github:jylhis/emacs
#   jotainInfo            — Jotain manual (share/info/jotain.info + dir)
#   jotainEmacsPackages   — full distribution using jotainEmacs
#   jylhisEmacsPackages   — full distribution using jylhisEmacs
#
# nix-community/emacs-overlay is composed underneath (pinned via
# flake.lock's root input map, same discipline as emacs.nix), so the
# emacs-git/unstable/igc bases and their epkgs snapshot resolve even
# when this file is imported standalone — e.g. the module.nix fallback
# without the flake, or:
#
#   import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }
#
# This keeps standalone consumers on the exact snapshot the flake build
# (and CI cache) uses, at the cost of one fixed-output fetchTarball.
let
  emacsOverlay = import (
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      n = lock.nodes.${lock.nodes.root.inputs.emacs-overlay}.locked;
    in
    fetchTarball {
      url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz";
      sha256 = n.narHash;
    }
  );
  jotainOverlay = import ./nix/mk-overlay.nix { };
in
# lib.composeExtensions, inlined: no nixpkgs lib is in scope before the
# overlay is applied.
final: prev:
let
  emacsAttrs = emacsOverlay final prev;
in
emacsAttrs // jotainOverlay final (prev // emacsAttrs)
