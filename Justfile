linux:
  nix-build --arg withPgtk true --argstr system x86_64-linux default.nix

macos:
  nix-build --arg variant '"macport"' --argstr system x86_64-darwin default.nix

android:
  nix-build --arg noGui true --argstr system aarch64-linux default.nix

run:
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
  ./result/bin/emacs --debug-init --eval '(setq debug-on-error t)' --init-directory='{{justfile_directory()}}'

clean:
  rm -rf eln-cache result custom.el transient elpa auto-save-list
