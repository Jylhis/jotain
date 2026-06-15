# module-nix-on-droid.nix — nix-on-droid module for Jotain Emacs.
#
# Installs a terminal-only Jotain Emacs into a nix-on-droid environment
# (Nix on Android, running under proot via Termux). Android has no
# systemd/launchd, no X/Wayland server, and no `fonts.packages`, and uses
# `environment.packages` instead of `environment.systemPackages`, so this
# module is a deliberately trimmed cousin of module-system.nix: it just
# adds a `-nw` Emacs plus an emacsclient EDITOR wrapper to the profile.
#
# For per-user daemon management on desktop Linux/macOS use the Home
# Manager module (module.nix); for NixOS / nix-darwin use module-system.nix.
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
    if cfg.package != null then
      cfg.package
    else if cfg.emacsBackend == "jylhis" then
      pkgsWithOverlay.jylhisEmacsPackages
    # Terminal-only build: Android under proot is headless.
    else
      pkgsWithOverlay.jotainEmacsPackagesNoGui;

  # Fallback for EDITOR when no daemon is running.
  editorFallback = pkgs.writeShellScript "jotain-editor-fallback" ''
    exec ${lib.getBin selectedPackage}/bin/emacs -nw -- "$@"
  '';

  # EDITOR — terminal emacsclient (works over SSH, in git commit, etc.).
  # There is no GUI on Android, so VISUAL points at the same client.
  editorScript = pkgs.writeShellScriptBin "jotain-editor" ''
    exec ${lib.getBin selectedPackage}/bin/emacsclient \
      --tty \
      --alternate-editor=${editorFallback} \
      -- \
      "$@"
  '';
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs configuration (terminal-only, for nix-on-droid)";

    emacsBackend = lib.mkOption {
      type = lib.types.enum [
        "mainline"
        "jylhis"
        "custom"
      ];
      default = "mainline";
      example = "jylhis";
      description = ''
        Which Emacs backend to install. `"mainline"` uses the
        cache-friendly terminal Emacs build from `emacs.nix`; `"jylhis"`
        uses the pinned `github:jylhis/emacs` Meson fork; `"custom"` uses
        `services.jotain.package`.
      '';
    };

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      defaultText = lib.literalExpression "null";
      description = ''
        Custom Jotain Emacs package to use. Leave this unset to use
        `services.jotain.emacsBackend`.
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
    assertions = [
      {
        assertion = cfg.emacsBackend != "custom" || cfg.package != null;
        message = "services.jotain.emacsBackend = \"custom\" requires services.jotain.package.";
      }
    ];

    environment.packages = [
      selectedPackage
      editorScript
      pkgsWithOverlay.eca
    ];

    environment.sessionVariables = lib.mkIf cfg.defaultEditor {
      EDITOR = "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = "${lib.getBin editorScript}/bin/jotain-editor";
    };
  };
}
