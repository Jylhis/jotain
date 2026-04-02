{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.languages.emacs-lisp;
in
{
  options.languages.emacs-lisp = {
    enable = lib.mkEnableOption "Emacs lisp support";
    package = lib.mkPackageOption pkgs "emacs-nox" {
      example = "emacs";
    };

  };
  config = lib.mkIf cfg.enable {
    packages = [
      cfg.package
      pkgs.eask-cli
    ];
  };
}
