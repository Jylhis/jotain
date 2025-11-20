{ config, lib, pkgs, ... }:

let
  cfg = config.programs.jotain;

in
{
  options.programs.jotain = {
    enable = lib.mkEnableOption "Jotain Emacs distribution system-wide";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.jotain;
      description = "The Jotain package to use";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # System fonts for Emacs
    fonts.packages = with pkgs; [
      fira-code
      jetbrains-mono
      (nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" ]; })
    ];
  };
}
