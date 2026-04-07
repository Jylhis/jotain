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
    enable = lib.mkEnableOption "Emacs Lisp support";

    package = lib.mkPackageOption pkgs "emacs-nox" {
      example = "emacs";
    };

    lsp = {
      enable = lib.mkEnableOption "Emacs Lisp Language Server (ellsp)" // {
        default = true;
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.callPackage ./ellsp.nix { emacs = cfg.package; };
        defaultText = lib.literalExpression "pkgs.callPackage ./ellsp.nix { emacs = cfg.package; }";
        description = "The Emacs Lisp language server package to use.";
      };
    };

    elsa = {
      enable = lib.mkEnableOption "Emacs Lisp Static Analyzer (Elsa)" // {
        default = true;
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.callPackage ./elsa.nix { emacs = cfg.package; };
        defaultText = lib.literalExpression "pkgs.callPackage ./elsa.nix { emacs = cfg.package; }";
        description = "The Elsa static analyzer package to use.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    packages = [
      cfg.package
      pkgs.eask-cli
    ]
    ++ lib.optional cfg.lsp.enable cfg.lsp.package
    ++ lib.optional cfg.elsa.enable cfg.elsa.package;
  };
}
