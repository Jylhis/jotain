# Runtime binaries the config invokes. They go on the Emacs wrapper's PATH
# (see nix/wrap-runtime-deps.nix), never the global environment.
{ pkgs, pkgsWithOverlay }:
with pkgs;
[
  ripgrep # xref-search-program, consult-ripgrep
  fd # project/consult fallback finder
  git # magit, vc
  jujutsu # vc-jj, majutsu, jotain-vc-stats
  delta # magit-delta
  mergiraf # smerge-mode solve
  zoxide # zoxide-add on find-file-hook, zoxide-find-file
  pkgsWithOverlay.eca # prevents the runtime server download
  pkgsWithOverlay.likec4Lsp # jotain-likec4-mode eglot server
  rsync # dired-rsync
  nixd # Nix LSP fallback when a devenv project provides none
  # eglot-booster resolves this once at enable time and disables itself if
  # it is missing, so it must be on the wrapper PATH, not a per-project env.
  emacs-lsp-booster
  coreutils # GNU userland for subprocesses; appended, so the host's wins
  qt6.qtdeclarative # qmlls and qmlformat for qml-ts-mode
]
