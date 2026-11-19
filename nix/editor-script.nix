# nix/editor-script.nix — the terminal `emacsclient' wrapper used as EDITOR.
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
