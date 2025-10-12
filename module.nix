{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.emacs;
in
{
  options = {
    programs.emacs.userConfig = lib.mkOption {
      type = lib.types.path;
      default = ./.; # TODO: use config package
      description = lib.mdDoc ''
        Path to the Emacs configuration directory.

        This directory should contain at minimum `init.el` and `early-init.el`,
        along with any additional configuration files organized in `config/` and
        `lisp/` subdirectories.

        When using this flake's home-manager module directly, this defaults to
        the source directory. For external usage, point this to your own
        configuration or use a pre-built configuration package.
      '';
      example = lib.literalExpression ''
        pkgs.fetchFromGitHub {
          owner = "username";
          repo = "emacs-config";
          rev = "v1.0.0";
          hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
        }
      '';
    };
  };
  config = lib.mkIf config.programs.emacs.enable {
    home = {
      file = {
        # Dynamically install all Emacs configuration files using filesets
        ".config/emacs" = {
          source = cfg.userConfig;
          recursive = true;
        };
      };
      shellAliases = {
        emc = "emacsclient -t -a emacs";
        emcg = "emacsclient -c -a emacs";
        emqg = "emacs -nw -Q";
        emq = "emacs -Q";
      };
      # TODO: move to fonts.nix
      packages =
        with pkgs.nerd-fonts;
        [
          dejavu-sans-mono
          envy-code-r
          fira-code
          fira-mono
          go-mono
          hack
          hasklug
          im-writing
          monaspace
          symbols-only
        ]
        ++ (with pkgs; [
          jetbrains-mono
          inter
          source-code-pro
        ]);
    };

    services = {
      emacs = {
        enable = lib.mkDefault true;
        defaultEditor = true;

        client = {
          enable = lib.mkDefault true;
          arguments = [
            "-c"
            "-a"
            "emacs"
          ];
        };
        socketActivation.enable = true;
      };
    };
  };
}
