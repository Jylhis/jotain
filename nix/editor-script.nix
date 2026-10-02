# nix/editor-script.nix — the terminal `emacsclient' wrapper used as EDITOR.
# Shared by module-system.nix and module-nix-on-droid.nix.
#
# `--alternate-editor' points at a `-nw' Emacs from the same wrapped
# package, so EDITOR still works when no daemon is running — over SSH, in a
# `git commit', in a sudoedit.
#
# module.nix (Home Manager) deliberately builds its own pair instead: its
# fallback has to go through `emacsWrapper', which pins --init-directory at
# the deployed config, and it also ships a GUI `jotain-visual'.
{ pkgs, package }:
let
  inherit (pkgs) lib;
  emacsBin = "${lib.getBin package}/bin";

  editorFallback = pkgs.writeShellScript "jotain-editor-fallback" ''
    exec ${emacsBin}/emacs -nw -- "$@"
  '';
in
pkgs.writeShellScriptBin "jotain-editor" ''
  exec ${emacsBin}/emacsclient \
    --tty \
    --alternate-editor=${editorFallback} \
    -- \
    "$@"
''
