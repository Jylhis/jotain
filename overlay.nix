# Standalone overlay for non-flake consumers. emacs-overlay (pinned from
# flake.lock) sits underneath, so Emacs and the epkgs snapshot match the
# flake build.
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
  jotainOverlay = import ./nix/mk-overlay.nix;
in
final: prev:
let
  emacsAttrs = emacsOverlay final prev;
in
emacsAttrs // jotainOverlay final (prev // emacsAttrs)
