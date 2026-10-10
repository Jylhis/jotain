# overlay.nix — Nixpkgs overlay for Jotain Emacs.
#
# Adds:
#   jotainEmacs              — bare Emacs binary (unstable variant, the
#                              newest Emacs release tag, currently 31.1;
#                              see nix/mk-overlay.nix)
#   jotainEmacsNoGui         — terminal-only (noGui) twin of jotainEmacs
#   jotainInfo               — Jotain manual (share/info/jotain.info + dir)
#   jotainEmacsPackages      — full distribution using jotainEmacs
#   jotainEmacsPackagesNoGui — full distribution on the noGui build
#   eca                      — prebuilt ECA server binary (lisp/init-ai.el)
#   likec4Lsp                — LikeC4 language server (lisp/init-lang-devops.el)
#
# nix-community/emacs-overlay (pinned by the monorepo's nixtamal lock,
# nix/tamal) is composed underneath, so standalone imports (the module
# fallback without the flake, or `import <nixpkgs> { overlays =
# [ (import ./overlay.nix) ]; }`) resolve the same Emacs bases and epkgs
# snapshot as the flake build.
let
  emacsOverlay = import (
    let
      pin = (builtins.fromJSON (builtins.readFile ../../nix/tamal/lock.json)).i.emacs-overlay;
      url = (builtins.elemAt pin.kd 1).ur;
      sha = pin.ha.vl;
    in
    builtins.fetchTarball { inherit url; sha256 = sha; }
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
