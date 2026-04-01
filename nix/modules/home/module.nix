# Parametric Jotain home-manager module.
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

  # Resolve packages: prefer pre-built flake packages, fall back to source build
  defaultPackages =
    if jotainSelf != null then {
      jotain = jotainSelf.packages.${system}.jotain;
      emacs = jotainSelf.packages.${system}.emacs;
    } else {
      jotain = pkgs.callPackage ../../../default.nix { };
      emacs = pkgs.callPackage ../../../emacs.nix { devMode = false; };
    };

  jotainEmacs = cfg.emacsPackage;

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
      default = defaultPackages.jotain;
      defaultText = lib.literalExpression "jotain.packages.\${system}.jotain";
      description = "The Jotain configuration package to use";
    };

    emacsPackage = lib.mkOption {
      type = lib.types.package;
      default = defaultPackages.emacs;
      defaultText = lib.literalExpression "jotain.packages.\${system}.emacs";
      description = ''
        The Jotain Emacs package (Emacs with all Jotain packages and runtime dependencies).

        Override this to use a custom-built Emacs or one from a different nixpkgs.
      '';
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
      programs.emacs = {
        enable = true;
        package = jotainEmacs;
        extraPackages = cfg.extraPackages;
      };

      services.emacs = lib.mkIf cfg.enableDaemon {
        enable = true;
        socketActivation.enable = pkgs.stdenv.isLinux;
        client = {
          enable = true;
          arguments = [ "-c" "-a" "" ];
        };
      };

      home.packages = [ cfg.package ]
        ++ lib.optionals cfg.includeRuntimeDeps (
        lspServers
          ++ cliTools
          ++ fonts
      );

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

      xdg.configFile."emacs/early-init.el".source = "${cfg.package}/share/jotain/early-init.el";
      xdg.configFile."emacs/init.el".source = "${cfg.package}/share/jotain/init.el";
    }

    # fonts.fontconfig is Linux-only in home-manager; nix-darwin doesn't have it
    (lib.mkIf (cfg.includeRuntimeDeps && pkgs.stdenv.isLinux) {
      fonts.fontconfig.enable = true;
    })
  ]);
}
