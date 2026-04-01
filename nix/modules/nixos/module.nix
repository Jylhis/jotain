# Parametric Jotain NixOS module.
#
# Accepts { jotainSelf } to use pre-built flake packages,
# or {} to build from the source tree using the consumer's pkgs
# (requires emacs-overlay + jotain overlay applied).
#
# Flake usage:
#   import ./module.nix { jotainSelf = self; }
#
# Non-flake usage (npins, fetchTarball, etc.):
#   import ./module.nix { }              — or just import the directory (default.nix)
{ jotainSelf ? null }:

{ config, lib, pkgs, ... }:

let
  cfg = config.programs.jotain;
  system = pkgs.stdenv.hostPlatform.system;

  defaultPackage =
    if jotainSelf != null
    then jotainSelf.packages.${system}.jotain
    else pkgs.callPackage ../../../default.nix { };

in
{
  options.programs.jotain = {
    enable = lib.mkEnableOption "Jotain Emacs distribution system-wide";

    package = lib.mkOption {
      type = lib.types.package;
      default = defaultPackage;
      defaultText = lib.literalExpression "jotain.packages.\${system}.jotain";
      description = "The Jotain package to use";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # System fonts for Emacs
    fonts.packages = with pkgs; [
      nerd-fonts.fira-code
      nerd-fonts.jetbrains-mono
    ];
  };
}
