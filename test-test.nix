{ config, lib, pkgs, ... }:

let
  cfg = config.programs.jotain;
in
{
  options.programs.jotain = {
    enable = lib.mkEnableOption "Jotain Emacs distribution";
    includeRuntimeDeps = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = lib.mkIf (cfg.includeRuntimeDeps && pkgs.stdenv.isLinux) true;
  };
}
