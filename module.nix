{ nix-doom-emacs }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.jotain;
in
{
  imports = [
    # Import nix-doom-emacs home-manager module
    nix-doom-emacs.homeModule
  ];

  options = {
    programs.jotain = {
      enable = lib.mkEnableOption "Jotain Emacs configuration";

      doomDir = lib.mkOption {
        type = lib.types.pathInStore;
        default = ./doomdir;
        description = lib.mdDoc ''
          Path to the Jotain Emacs configuration directory (DOOMDIR).

          This directory should contain:
          - init.el: Module configuration with doom! block
          - config.el: User configuration and customizations
          - packages.el: Additional package declarations
        '';
        example = lib.literalExpression "./my-doomdir";
      };

      doomLocalDir = lib.mkOption {
        type = lib.types.path;
        default = "~/.local/share/doom";
        description = lib.mdDoc ''
          Directory for Jotain's local data (DOOMLOCALDIR).
          This is where Jotain stores profiles, cache, and state.
        '';
      };

      emacs = lib.mkOption {
        type = lib.types.package;
        default = pkgs.emacs-pgtk;
        defaultText = lib.literalExpression "pkgs.emacs-pgtk";
        example = lib.literalExpression "pkgs.emacs29-pgtk";
        description = lib.mdDoc "The Emacs package to use with Jotain.";
      };

      installFonts = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = lib.mdDoc ''
          Whether to install Nerd Fonts and other recommended fonts
          for use with Jotain Emacs configuration.
        '';
      };

      enableShellAliases = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = lib.mdDoc ''
          Whether to install shell aliases for convenient Emacs access.
          Aliases: jot, emc, emcg, emqg, emq
        '';
      };

      extraDoomOptions = lib.mkOption {
        type = lib.types.attrs;
        default = { };
        description = lib.mdDoc ''
          Additional options to pass to programs.doom-emacs.
          This allows configuring nix-doom-emacs-specific features like
          experimentalFetchTree, lspUsePlists, etc.
        '';
        example = lib.literalExpression ''
          {
            experimentalFetchTree = true;
            lspUsePlists = true;
            extraBinPackages = with pkgs; [ ripgrep fd ];
          }
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      # Configure doom-emacs via nix-doom-emacs module
      {
        programs.doom-emacs = {
          enable = true;
          inherit (cfg) doomDir doomLocalDir emacs;
          provideEmacs = true; # Provide "emacs" binary
        }
        // cfg.extraDoomOptions;
      }

      # Jotain-specific shell aliases
      (lib.mkIf cfg.enableShellAliases {
        home.shellAliases = {
          jot = "emacsclient -t -a emacs";
          emc = "emacsclient -t -a emacs";
          emcg = "emacsclient -c -a emacs";
          emqg = "emacs -nw -Q";
          emq = "emacs -Q";
        };
      })

      # Font packages for Jotain themes and UI
      (lib.mkIf cfg.installFonts {
        home.packages =
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
            # Emoji support
            noto-fonts-emoji
          ]);
      })

      # Emacs systemd service configuration
      {
        services.emacs = {
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
      }
    ]
  );
}
