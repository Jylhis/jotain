linux:
  nix-build --arg withPgtk true --argstr system x86_64-linux default.nix

macos:
  nix-build --arg variant '"macport"' --argstr system x86_64-darwin default.nix

android:
  nix-build --arg noGui true --argstr system aarch64-linux default.nix

run:
  #!/usr/bin/env bash
  set -e
  case "$(uname -s)-$(uname -m)" in
    Darwin-*) just macos ;;
    Linux-aarch64) just android ;;
    *) just linux ;;
  esac
  ./result/bin/emacs --debug-init --init-directory='{{justfile_directory()}}'

clean:
  rm -rf eln-cache result custom.el transient elpa auto-save-list
