# nix/runtime-deps.nix — Runtime binaries the Elisp config invokes
# unconditionally, shared by every module entry point (module.nix,
# module-system.nix, module-nix-on-droid.nix).
#
# Each module scopes this list to the Emacs wrapper's PATH — never the
# global environment — so the tools are available regardless of launch
# context (launchd/Dock on macOS, systemd units on Linux) without GNU
# coreutils shadowing the host userland system-wide.
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
  emacs-lsp-booster # eglot-booster (init-prog.el): `eglot-booster-mode'
  # resolves this binary via `executable-find' ONCE at enable time, before
  # any buffer-local exec-path exists, and disables itself if it is missing
  # — so it must ride the wrapper PATH, never a per-project devenv.
  qt6.qtdeclarative # qmlls (LSP) + qmlformat (apheleia) for qml-ts-mode
  # (init-lang-qml). Large Qt closure, accepted so QML support is
  # launch-context-independent; a project/devenv qmlls still wins.
]
# GNU userland for the wrapper only. On darwin the g-prefixed variant is
# load-bearing twice over: init-navigation.el probes `gls' by that name
# for dirvish/dired listing switches, and unprefixed GNU coreutils would
# shadow BSD ls/stat/… for every subprocess Emacs spawns.
++ lib.optional (!isDarwin) coreutils
++ lib.optional isDarwin coreutils-prefixed
