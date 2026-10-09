# module-nix-on-droid.nix — nix-on-droid module for Jotain Emacs.
#
# Installs a terminal-only Jotain Emacs plus an emacsclient EDITOR wrapper
# into nix-on-droid (Nix on Android under proot). A trimmed cousin of
# module-system.nix: no systemd/launchd, display server or
# `fonts.packages`, and `environment.packages` instead of
# `environment.systemPackages`.
#
# Usage in a nix-on-droid flake:
#
#   imports = [ jotain.nixOnDroidModules.default ];
#   services.jotain.enable = true;
args@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.jotain;
  jotainOverlay = args.jotainOverlay or (import ./overlay.nix);
  pkgsWithOverlay = pkgs.extend jotainOverlay;
  selectedPackage =
    if cfg.package != null then cfg.package else pkgsWithOverlay.jotainEmacsPackagesNoGui;

  runtimeDeps = import ./nix/runtime-deps.nix { inherit pkgs pkgsWithOverlay; };

  wrappedPackage = import ./nix/wrap-runtime-deps.nix {
    inherit pkgs runtimeDeps;
    package = selectedPackage;
  };

  # Terminal emacsclient for EDITOR, and VISUAL too (no GUI on Android).
  editorScript = import ./nix/editor-script.nix {
    inherit pkgs;
    package = wrappedPackage;
  };
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs configuration (terminal-only, for nix-on-droid)";

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      defaultText = lib.literalExpression "null";
      description = ''
        Custom Jotain Emacs package to use. Leave unset for the
        terminal-only distribution (`jotainEmacsPackagesNoGui`).
      '';
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
    environment.packages = [
      wrappedPackage
      editorScript
      pkgsWithOverlay.eca
      # jinx dictionary: libaspell finds $profile/lib/aspell via its
      # NIX_PROFILES patch, so it belongs in the profile, not on PATH.
      pkgs.aspellDicts.en
    ];

    environment.sessionVariables = lib.mkIf cfg.defaultEditor {
      EDITOR = "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = "${lib.getBin editorScript}/bin/jotain-editor";
    };
  };
}
