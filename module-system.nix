# module-system.nix — NixOS / nix-darwin module for Jotain Emacs.
#
# Applies the project overlay to nixpkgs and adds the Jotain Emacs
# package to the system environment.  Shared between NixOS and
# nix-darwin — both module systems support nixpkgs.overlays and
# environment.systemPackages.
#
# For per-user daemon management (systemd service, launchd agent,
# emacsclient wrapper, desktop entry), use the Home Manager module
# (module.nix) instead.
#
# Usage in NixOS:
#
#   imports = [ jotain.nixosModules.default ];
#   services.jotain.enable = true;
#
# Usage in nix-darwin:
#
#   imports = [ jotain.darwinModules.default ];
#   services.jotain.enable = true;
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.jotain;

  # Fallback script for EDITOR when the daemon is not running.
  editorFallback = pkgs.writeShellScript "jotain-editor-fallback" ''
    exec ${lib.getBin cfg.package}/bin/emacs -nw "$@"
  '';

  # EDITOR — terminal-friendly emacsclient (works over SSH, in git commit, etc.)
  editorScript = pkgs.writeShellScriptBin "jotain-editor" ''
    exec ${lib.getBin cfg.package}/bin/emacsclient \
      --tty \
      --alternate-editor=${editorFallback} \
      "$@"
  '';

  # VISUAL — opens a GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "jotain-visual" ''
    exec ${lib.getBin cfg.package}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${lib.getBin cfg.package}/bin/emacs \
      "$@"
  '';
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs configuration";

    package = lib.mkOption {
      type = lib.types.package;
      default = (pkgs.extend (import ./overlay.nix)).jotainEmacsPackages;
      defaultText = lib.literalExpression "(pkgs.extend (import ./overlay.nix)).jotainEmacsPackages";
      description = "The Jotain Emacs package to use.";
    };

    defaultEditor = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = ''
        Whether to configure {command}`emacsclient` as the default
        editor using the {env}`EDITOR` and {env}`VISUAL`
        environment variables.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ (import ./overlay.nix) ];
    environment.systemPackages = [
      cfg.package
      editorScript
      visualScript
    ];
    environment.variables = lib.mkIf cfg.defaultEditor {
      EDITOR = lib.mkOverride 900 "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = lib.mkOverride 900 "${lib.getBin visualScript}/bin/jotain-visual";
    };
  };
}
