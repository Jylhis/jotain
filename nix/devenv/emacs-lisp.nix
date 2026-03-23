# Custom devenv language module for Emacs Lisp development
{ lib, config, pkgs, ... }:

let
  cfg = config.languages.emacs-lisp;
in
{
  options.languages.emacs-lisp = {
    enable = lib.mkEnableOption "Emacs Lisp development tools";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.emacs;
      description = "The Emacs package to use for Emacs Lisp development.";
    };
  };

  config = lib.mkIf cfg.enable {
    packages = [ cfg.package ];
  };
}
