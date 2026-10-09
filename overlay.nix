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
# nix-community/emacs-overlay (pinned via flake.lock) is composed
# underneath, so standalone imports (the module fallback without the
# flake, or `import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }`)
# resolve the same Emacs bases and epkgs snapshot as the flake build.
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
