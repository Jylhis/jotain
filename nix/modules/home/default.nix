{ config, options, lib, pkgs, ... }:

let
  cfg = config.programs.jotain;

  jotainEmacs = pkgs.jotainEmacs;

  lspServers = jotainEmacs.lspServers or [ ];
  cliTools = jotainEmacs.cliTools or [ ];
  fonts = jotainEmacs.fonts or [ ];
  treeSitterGrammars = jotainEmacs.treeSitterGrammars or [ ];
  allRuntimeDeps = jotainEmacs.allRuntimeDeps or [ ];

in
{
  options.programs.jotain = {
    enable = lib.mkEnableOption "Jotain Emacs distribution";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.jotain;
      defaultText = lib.literalExpression "pkgs.jotain";
      description = "The Jotain configuration package to use";
    };

    extraPackages = lib.mkOption {
      type = lib.types.functionTo (lib.types.listOf lib.types.package);
      default = _: [ ];
      defaultText = lib.literalExpression "epkgs: [ ]";
      description = "Extra packages to install alongside Jotain Emacs";
    };

    includeRuntimeDeps = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Include runtime dependencies (LSP servers, CLI tools, fonts) in the user environment.

        When enabled, this option installs:
        - LSP servers (gopls, nil, etc.)
        - CLI tools (ripgrep, fd)
        - Nerd Fonts and other font packages

        These dependencies are exposed via the Emacs package's passthru.runtimeDeps attribute
        and are made available in home.packages and properly configured for fontconfig.

        Set to false if you prefer to manage these dependencies separately or don't need them.
      '';
    };

    enableDaemon = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Enable Emacs daemon and configure emacsclient for editor commands.

        When enabled:
        - Starts Emacs as a systemd user service (Linux) or launchd service (macOS)
        - Sets EDITOR/VISUAL environment variables to use emacsclient
        - Enables socket activation for faster startup (Linux only)
        - Creates desktop entry for emacsclient

        When disabled, Emacs runs as a regular application without daemon mode.
        This is useful for debugging, development, or platforms without systemd support.
      '';
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      # Install Emacs via the programs.emacs module
      programs.emacs = {
        enable = true;
        package = jotainEmacs;
        extraPackages = cfg.extraPackages;
      };

      # Configure Emacs daemon service when enabled
      services.emacs = lib.mkIf cfg.enableDaemon {
        enable = true;
        socketActivation.enable = pkgs.stdenv.isLinux;
        client = {
          enable = true;
          arguments = [ "-c" "-a" "" ];
        };
      };

      # Install configuration files and runtime dependencies
      home.packages = [ cfg.package ]
        ++ lib.optionals cfg.includeRuntimeDeps (
          lspServers
          ++ cliTools
          ++ fonts
        );

      # Set Emacs as default editor
      home.sessionVariables = lib.mkMerge [
        (lib.mkIf (!cfg.enableDaemon) {
          EDITOR = "emacs -nw";
          VISUAL = "emacs";
        })
        (lib.mkIf cfg.enableDaemon {
          EDITOR = "emacsclient -t -a ''";
          VISUAL = "emacsclient -c -a ''";
        })
      ];

      # XDG configuration
      xdg.configFile."emacs/early-init.el".source = "${cfg.package}/share/jotain/early-init.el";
      xdg.configFile."emacs/init.el".source = "${cfg.package}/share/jotain/init.el";
    }

    # Configure fonts for fontconfig when runtime deps are included
    (lib.optionalAttrs (cfg.includeRuntimeDeps && options ? fonts.fontconfig) {
  fonts.fontconfig.enable = true;
})
  ]);
}
