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
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ (import ./overlay.nix) ];
    environment.systemPackages = [ cfg.package ];
  };
}
