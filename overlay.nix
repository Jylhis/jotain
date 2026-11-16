# overlay.nix — Nixpkgs overlay for the configured Emacs.
#
# Adds:
#   emacsConfig              — bare Emacs binary (unstable variant, the
#                              newest Emacs release tag, currently 31.1;
#                              see nix/mk-overlay.nix)
#   emacsConfigNoGui         — terminal-only (noGui) twin of emacsConfig
#   emacsConfigInfo               — the configuration manual (share/info/emacs-config.info + dir)
#   emacsConfigPackages      — full distribution using emacsConfig
#   emacsConfigPackagesNoGui — full distribution on the noGui build
#   eca                      — prebuilt ECA server binary (lisp/init-ai.el)
#   likec4Lsp                — LikeC4 language server (lisp/init-lang-devops.el)
#
# nix-community/emacs-overlay (pinned by flake.lock, read through
# nix/locked.nix) is composed underneath, so standalone imports (the module
# fallback without the flake, or `import <nixpkgs> { overlays =
# [ (import ./overlay.nix) ]; }`) resolve the same Emacs bases and epkgs
# snapshot as the flake build.
let
  emacsOverlay = import (import ./nix/locked.nix).emacs-overlay;
  emacsConfigOverlay = import ./nix/mk-overlay.nix { };
in
# lib.composeExtensions, inlined: no nixpkgs lib is in scope before the
# overlay is applied.
final: prev:
let
  emacsAttrs = emacsOverlay final prev;
in
emacsAttrs // emacsConfigOverlay final (prev // emacsAttrs)
