# nix/editor-script.nix — the terminal `emacsclient' wrapper used as EDITOR.
# Shared by module-system.nix.
#
# `--alternate-editor' falls back to a `-nw' Emacs from the same package
# when no daemon is running.
#
# module.nix builds its own: its fallback must go through `emacsWrapper',
# which pins --init-directory at the deployed config.
{ pkgs, package }:
let
  inherit (pkgs) lib;
  emacsBin = "${lib.getBin package}/bin";

  editorFallback = pkgs.writeShellScript "emacs-config-editor-fallback" ''
    exec ${emacsBin}/emacs -nw -- "$@"
  '';
in
pkgs.writeShellScriptBin "emacs-config-editor" ''
  exec ${emacsBin}/emacsclient \
    --tty \
    --alternate-editor=${editorFallback} \
    -- \
    "$@"
''
