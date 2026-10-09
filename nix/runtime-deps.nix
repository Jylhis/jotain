# nix/runtime-deps.nix — Runtime binaries the Elisp config invokes
# unconditionally. Put on the Emacs wrapper's PATH (never the global
# environment) by nix/mk-overlay.nix and every module, so they work in any
# launch context (launchd/Dock, systemd) without GNU coreutils shadowing
# the host userland.
{ pkgs, pkgsWithOverlay }:
let
  inherit (pkgs) lib;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
with pkgs;
[
  ripgrep # xref-search-program, consult-ripgrep
  fd # project/consult fallback finder
  git # magit, vc
  jujutsu # vc-jj, majutsu, jotain-vc-stats (jj binary)
  zoxide # zoxide-add on find-file-hook, zoxide-find-file (M-g z)
  pkgsWithOverlay.eca # eca-emacs server; prevents runtime download fallback
  pkgsWithOverlay.likec4Lsp # LikeC4 LSP for likec4-mode eglot (init-lang-devops)
  rsync # dired-rsync (C-c C-r)
  nixd # shipped Nix LSP fallback (devenv--nix-lsp-program) when a devenv
  # project's own env provides no nixd/nil for eglot
  emacs-lsp-booster # eglot-booster (init-prog.el): resolved ONCE at enable
  # time, before any buffer-local exec-path exists, and the mode disables
  # itself if it is missing, so it must ride the wrapper PATH, never a
  # per-project devenv.
  qt6.qtdeclarative # qmlls (LSP) + qmlformat (apheleia) for qml-ts-mode
  # (init-lang-qml). Large Qt closure, accepted so QML support is
  # launch-context-independent; a project/devenv qmlls still wins.
]
# GNU userland for the wrapper only. On darwin the g-prefixed variant is
# required: init-navigation.el probes `gls' by name, and unprefixed GNU
# coreutils would shadow BSD ls/stat/… for every subprocess Emacs spawns.
++ lib.optional (!isDarwin) coreutils
++ lib.optional isDarwin coreutils-prefixed
