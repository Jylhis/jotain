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
    else
      pkgsWithOverlay.jotainEmacsPackages;

  # Fallback script for EDITOR when the daemon is not running.
  editorFallback = pkgs.writeShellScript "jotain-editor-fallback" ''
    exec ${lib.getBin selectedPackage}/bin/emacs -nw "$@"
  '';

  # EDITOR — terminal-friendly emacsclient (works over SSH, in git commit, etc.)
  editorScript = pkgs.writeShellScriptBin "jotain-editor" ''
    exec ${lib.getBin selectedPackage}/bin/emacsclient \
      --tty \
      --alternate-editor=${editorFallback} \
      "$@"
  '';

  # VISUAL — opens a GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "jotain-visual" ''
    exec ${lib.getBin selectedPackage}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${lib.getBin selectedPackage}/bin/emacs \
      "$@"
  '';
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs configuration";

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
        cache-friendly Emacs build from `emacs.nix`; `"jylhis"` uses
        the pinned `github:jylhis/emacs` Meson fork; `"custom"` uses
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

    nixpkgs.overlays = [ jotainOverlay ];
    environment.systemPackages = [
      selectedPackage
      editorScript
      visualScript
    ];
    # Colour-emoji fallback for the `emoji' / `symbol' fontsets wired
    # in lisp/init-ui.el.  Skipped on Darwin: macOS provides Apple
    # Color Emoji system-wide, and nix-darwin's `fonts.packages' has a
    # different shape from NixOS's.
    fonts.packages = lib.mkIf pkgs.stdenv.hostPlatform.isLinux [
      pkgs.noto-fonts-color-emoji
    ];
    environment.variables = lib.mkIf cfg.defaultEditor {
      EDITOR = lib.mkOverride 900 "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = lib.mkOverride 900 "${lib.getBin visualScript}/bin/jotain-visual";
    };
  };
}
