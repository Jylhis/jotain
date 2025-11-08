# Home Manager module for Jotain
{ config, lib, pkgs, ... }:

let
  cfg = config.programs.jotain;

in
{
  options.programs.jotain = {
    enable = lib.mkEnableOption "Jotain Emacs distribution";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.jotain;
      defaultText = lib.literalExpression "pkgs.jotain";
      description = "The Jotain package to use";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    # Set Emacs as default editor
    home.sessionVariables = {
      EDITOR = "emacs -nw";
      VISUAL = "emacs";
    };

    # XDG configuration
    xdg.configFile."emacs/early-init.el".source = "${cfg.package}/share/jotain/templates/early-init.el";
    xdg.configFile."emacs/init.el".text = ''
      ;; Load Jotain
      (add-to-list 'load-path "${cfg.package}/share/emacs/site-lisp/jotain")
      (require 'jotain)
    '';
  };
}
