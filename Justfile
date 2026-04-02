[private]
default:
  @just --list

# ── x86_64-linux variants (all binary-cached) ──────────────────────

linux:
  nix-build --argstr system x86_64-linux default.nix

linux-gtk3:
  nix-build --arg withGTK3 true --argstr system x86_64-linux default.nix

linux-nox:
  nix-build --arg noGui true --argstr system x86_64-linux default.nix

linux-pgtk:
  nix-build --arg withPgtk true --argstr system x86_64-linux default.nix

# ── x86_64-darwin variants (all binary-cached) ─────────────────────

macos:
  nix-build --argstr system x86_64-darwin default.nix

macos-gtk3:
  nix-build --arg withGTK3 true --argstr system x86_64-darwin default.nix

macos-nox:
  nix-build --arg noGui true --argstr system x86_64-darwin default.nix

macos-pgtk:
  nix-build --arg withPgtk true --argstr system x86_64-darwin default.nix

macos-macport:
  nix-build --arg variant '"macport"' --argstr system x86_64-darwin default.nix

# ── aarch64-linux ──────────────────────────────────────────────────

android:
  nix-build --arg noGui true --argstr system aarch64-linux default.nix

# ── Run & clean ────────────────────────────────────────────────────

run *ARGS:
  #!/usr/bin/env bash
  set -euo pipefail
  platform="$(uname -s)-$(uname -m)"
  echo "Platform: $platform"
  case "$platform" in
    Darwin-*) target=macos ;;
    Linux-aarch64) target=android ;;
    *) target=linux ;;
  esac
  echo "Building target: $target"
  just "$target"
  echo "Build output: $(readlink result)"
  echo "Launching Emacs..."
  ./result/bin/emacs --debug-init --eval '(setq debug-on-error t)' --init-directory='{{justfile_directory()}}' {{ARGS}}

clean:
  rm -rf eln-cache result custom.el transient elpa auto-save-list
